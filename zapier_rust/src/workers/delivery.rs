use reqwest::Client;
use sqlx::PgPool;
use std::time::Duration;
use uuid::Uuid;

use crate::models::delivery::PendingDelivery;

pub fn start_delivery_worker(pool: PgPool, disable_webhook_delivery: bool) -> tokio::task::JoinHandle<()> {
    tokio::spawn(async move {
        delivery_worker_loop(pool, disable_webhook_delivery).await;
    })
}

async fn delivery_worker_loop(pool: PgPool, disable_webhook_delivery: bool) {
    let client = Client::builder()
        .timeout(Duration::from_secs(10))
        .pool_max_idle_per_host(50)
        .build()
        .expect("Failed to build HTTP client");

    let mut interval = tokio::time::interval(Duration::from_millis(100));

    loop {
        interval.tick().await;

        if let Err(e) = process_pending_deliveries(&pool, &client, disable_webhook_delivery).await {
            tracing::error!("Error processing deliveries: {:?}", e);
        }
    }
}

async fn process_pending_deliveries(pool: &PgPool, client: &Client, disable_webhook_delivery: bool) -> anyhow::Result<()> {
    // Fetch pending deliveries
    let deliveries = sqlx::query_as::<_, PendingDelivery>(
        r#"
        SELECT
            ed.id as delivery_id,
            ed.event_id,
            e.event_type,
            e.payload,
            o.webhook_url,
            ed.attempts
        FROM event_deliveries ed
        JOIN events e ON e.id = ed.event_id
        JOIN organizations o ON o.id = e.organization_id
        WHERE ed.status = 'pending' AND ed.attempts < 5
        ORDER BY ed.created_at
        LIMIT 100
        "#
    )
    .fetch_all(pool)
    .await?;

    if deliveries.is_empty() {
        return Ok(());
    }

    tracing::debug!("Processing {} pending deliveries", deliveries.len());

    // Process deliveries in parallel
    let tasks: Vec<_> = deliveries
        .into_iter()
        .map(|delivery| {
            let client = client.clone();
            let pool = pool.clone();
            tokio::spawn(async move {
                process_single_delivery(client, pool, delivery, disable_webhook_delivery).await
            })
        })
        .collect();

    // Wait for all tasks
    for task in tasks {
        if let Err(e) = task.await {
            tracing::error!("Delivery task failed: {:?}", e);
        }
    }

    Ok(())
}

async fn process_single_delivery(client: Client, pool: PgPool, delivery: PendingDelivery, disable_webhook_delivery: bool) {
    let payload = serde_json::json!({
        "event_id": delivery.event_id,
        "type": delivery.event_type,
        "data": delivery.payload,
    });

    // Check if webhook delivery is disabled (for performance testing)
    let result = if disable_webhook_delivery {
        tracing::debug!(
            event_id = %delivery.event_id,
            "Webhook delivery disabled, marking as delivered without HTTP call"
        );
        // Simulate successful response without making HTTP call
        Ok(reqwest::Response::from(http::Response::builder()
            .status(200)
            .body("")
            .unwrap()))
    } else {
        client
            .post(&delivery.webhook_url)
            .json(&payload)
            .header("X-Event-ID", delivery.event_id.to_string())
            .header("X-Event-Type", &delivery.event_type)
            .send()
            .await
    };

    match result {
        Ok(resp) if resp.status().is_success() => {
            // Success - mark as delivered
            if let Err(e) = sqlx::query(
                r#"
                UPDATE event_deliveries
                SET status = 'delivered',
                    response_status = $1,
                    updated_at = NOW()
                WHERE id = $2
                "#
            )
            .bind(resp.status().as_u16() as i32)
            .bind(delivery.delivery_id)
            .execute(&pool)
            .await
            {
                tracing::error!("Failed to update delivery status: {:?}", e);
            } else {
                tracing::info!(
                    delivery_id = %delivery.delivery_id,
                    event_id = %delivery.event_id,
                    "Delivery successful"
                );
            }
        }
        Ok(resp) => {
            // HTTP error
            let status = resp.status().as_u16();
            let error_msg = format!("HTTP {}", status);
            update_failed_delivery(&pool, delivery.delivery_id, delivery.attempts, Some(status as i32), error_msg).await;
        }
        Err(e) => {
            // Network/timeout error
            let error_msg = e.to_string();
            update_failed_delivery(&pool, delivery.delivery_id, delivery.attempts, None, error_msg).await;
        }
    }
}

async fn update_failed_delivery(
    pool: &PgPool,
    delivery_id: Uuid,
    attempts: i32,
    status: Option<i32>,
    error_message: String,
) {
    let new_attempts = attempts + 1;
    let new_status = if new_attempts >= 5 { "failed" } else { "pending" };

    if let Err(e) = sqlx::query(
        r#"
        UPDATE event_deliveries
        SET attempts = $1,
            status = $2,
            response_status = $3,
            error_message = $4,
            last_attempt_at = NOW(),
            updated_at = NOW()
        WHERE id = $5
        "#
    )
    .bind(new_attempts)
    .bind(new_status)
    .bind(status)
    .bind(&error_message)
    .bind(delivery_id)
    .execute(pool)
    .await
    {
        tracing::error!("Failed to update failed delivery: {:?}", e);
    } else {
        tracing::warn!(
            delivery_id = %delivery_id,
            attempts = new_attempts,
            status = new_status,
            "Delivery failed"
        );
    }
}
