use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::time::Duration;

/// Benchmark event ingestion latency
fn bench_event_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("event_validation");

    // Small payload (1KB)
    let small_payload = serde_json::json!({
        "user_id": "12345",
        "action": "login",
        "timestamp": "2025-01-01T00:00:00Z"
    });

    // Medium payload (10KB)
    let mut medium_data = serde_json::Map::new();
    for i in 0..100 {
        medium_data.insert(format!("field_{}", i), serde_json::json!(format!("value_{}", i)));
    }
    let medium_payload = serde_json::Value::Object(medium_data);

    // Large payload (100KB)
    let mut large_data = serde_json::Map::new();
    for i in 0..1000 {
        large_data.insert(format!("field_{}", i), serde_json::json!(format!("value_{}", i)));
    }
    let large_payload = serde_json::Value::Object(large_data);

    group.bench_with_input(BenchmarkId::new("payload_size", "1KB"), &small_payload, |b, payload| {
        b.iter(|| {
            let size = serde_json::to_vec(black_box(payload)).unwrap().len();
            black_box(size)
        });
    });

    group.bench_with_input(BenchmarkId::new("payload_size", "10KB"), &medium_payload, |b, payload| {
        b.iter(|| {
            let size = serde_json::to_vec(black_box(payload)).unwrap().len();
            black_box(size)
        });
    });

    group.bench_with_input(BenchmarkId::new("payload_size", "100KB"), &large_payload, |b, payload| {
        b.iter(|| {
            let size = serde_json::to_vec(black_box(payload)).unwrap().len();
            black_box(size)
        });
    });

    group.finish();
}

/// Benchmark Argon2id hashing (authentication overhead)
fn bench_argon2_hashing(c: &mut Criterion) {
    use argon2::{Argon2, PasswordHasher};
    use argon2::password_hash::SaltString;

    let mut group = c.benchmark_group("authentication");
    group.measurement_time(Duration::from_secs(10));

    let api_key = "zap_live_1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcd";
    let salt = SaltString::from_b64("c29tZXNhbHR2YWx1ZQ").unwrap();
    let argon2 = Argon2::default();

    group.bench_function("argon2id_hash", |b| {
        b.iter(|| {
            let hash = argon2.hash_password(black_box(api_key.as_bytes()), &salt).unwrap();
            black_box(hash)
        });
    });

    group.finish();
}

/// Benchmark channel-based event queuing
fn bench_event_queuing(c: &mut Criterion) {
    use tokio::runtime::Runtime;
    use tokio::sync::mpsc;

    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("event_queuing");

    group.bench_function("mpsc_channel_send", |b| {
        let (tx, mut rx) = mpsc::channel::<String>(10000);

        // Spawn receiver task
        rt.spawn(async move {
            while let Some(_) = rx.recv().await {
                // Consume events
            }
        });

        b.to_async(&rt).iter(|| async {
            tx.send(black_box("test_event".to_string())).await.unwrap();
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_event_validation,
    bench_argon2_hashing,
    bench_event_queuing
);
criterion_main!(benches);
