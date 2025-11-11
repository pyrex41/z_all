# Zapier Triggers API - Product Requirements Document (PRD)

**Version:** 1.2 (Elixir Implementation)  
**Status:** Refined Draft  
**Last Updated:** November 10, 2025  
**Owner:** Reuben (Technical Lead)  
**Stakeholders:** Developers, Automation Engineers, Integration Partners

---

## 1. Executive Summary

### The Opportunity
Zapier lacks a unified public API for real-time event ingestion, forcing fragmented custom integrations and polling-based triggers. This limits adoption of event-driven automation.

### The Solution
A simple RESTful Triggers API for webhook ingestion, durable storage, and reliable delivery to Zapier workflows. Modeled after proven patterns (e.g., Stripe Webhooks), it's a lightweight primitive: three core endpoints, one worker process, standard CRUD. Implemented in Elixir for superior concurrency, fault-tolerance, and low-latency handling via OTP.

### Success Criteria
- **Technical:** 99.9% uptime, <100ms p95 ingestion latency, <5s p95 delivery.
- **Adoption:** 50+ integrations migrated in 6 months; 10K+ new Zaps.
- **Developer NPS:** 50+ via surveys.
- **Revenue:** Enable usage-based pricing for high-volume events.

**Why a Layup:** Solved problem, proven tech (Elixir/Phoenix), small scope (MVP in 2 weeks), high leverage for real-time workflows. Elixir's lightweight processes excel at high-throughput event processing without Redis overhead.

---

## 2. Problem Statement

**Current Pains:**
- **Developers:** Custom integrations per app; no standard real-time push; polling delays (15min free tier).
- **Users:** Delayed automations; limited to pre-built integrations; no reactive workflows.
- **Platform:** Fragmented triggers across 6K+ integrations; high maintenance; misses agentic use cases.

**Core Insight:** Webhooks are commoditized—make them trivial for any event source to connect to Zapier. Elixir's concurrency model handles event storms gracefully.

---

## 3. Goals & Success Metrics

### North Star
Events delivered to workflows/month (target: 1M+ in 3 months).

### Launch Goals (3 Months)
| Metric | Target | Rationale |
|--------|--------|-----------|
| Uptime | 99.9% | Webhook standard |
| Ingestion Latency (p95) | <100ms | Instant ACK |
| Delivery Latency (p95) | <5s | Real-time feel |
| Delivery Success | 99.5% | Handle transients |
| Integrations Onboarded | 50+ | Network effects |
| Events/Month | 1M+ | PMF validation |
| Developer NPS | 50+ | Frictionless DX |

**Long-Term (6-12 Months):** 10M+ events/month; 500+ integrations; sub-second delivery; 99.99% uptime.

---

## 4. Target Users & Personas

### Primary: Integration Developer (Marcus)
- **Profile:** Backend engineer at SaaS (50-200 employees); knows REST/webhooks.
- **JTBD:** Send events to Zapier in <1hr; avoid full integrations.
- **Pains:** Weeks of dev time; complex OAuth; no delivery visibility.
- **Success:** Copy API key → send event → see in Zapier (<10min); auto-retries/logs.

### Secondary: Automation Engineer (Sarah)
- **Profile:** Ops lead at startup; heavy Zapier user, non-dev.
- **JTBD:** Instant reactions; connect non-integrated tools.
- **Pains:** Polling delays; integration limits.
- **Success:** No-code event sends; sub-5s executions.

### Tertiary: Platform Architect (David)
- **Profile:** Eng director at enterprise; evaluates for scale/compliance.
- **JTBD:** Centralize routing; audit/secure millions of events.
- **Pains:** No enterprise features; unclear quotas.
- **Success:** SOC2 docs; transparent high-volume pricing.

---

## 5. User Stories (Prioritized)

### Epic 1: Ingestion (P0)
- **1.1:** As Marcus, send first event in <5min. *AC:* Dashboard shows key/curl example; 201 response with ID.
- **1.2:** As Marcus, auto-handle duplicates. *AC:* 409 on dedup_id retry (24h window).
- **1.3:** As Marcus, send high-volume without limits. *AC:* Accept 1K/sec on paid; alerts near caps.

### Epic 2: Delivery (P0)
- **2.1:** As Sarah, trigger Zaps instantly. *AC:* <5s execution; event in history.
- **2.2:** As Marcus, auto-retry failures. *AC:* Exp backoff (5 attempts); dashboard shows tries.
- **2.3:** As Marcus, view failures. *AC:* Dashboard lists errors; manual retry; email alerts.

### Epic 3: Management (P1)
- **3.1:** As Marcus, retrieve undelivered via API. *AC:* Paginated GET /inbox; POST /ack (up to 1K IDs).
- **3.2:** As David, access logs. *AC:* Filter/export (CSV/JSON) by type/status/date.
- **3.3:** As Marcus, test webhooks. *AC:* Dashboard "Send Test"; shows response.

---

## 6. Functional Requirements

### Core API (P0)
- **POST /events:** Ingest JSON (type req, data freeform ≤256KB, optional dedup_id). 201 w/ID; 409 dup; 429 limit. <100ms p95.
- **GET /inbox:** Paginated undelivered (limit 1-1K; filter status; cursor). Desc by created_at.
- **POST /ack:** Mark delivered (up to 1K IDs); idempotent; returns count.

### Delivery System (P0)
- Worker: Queue via Oban (Postgres-backed jobs); POST to webhook w/headers (X-Zapier-Event-*); 10s timeout.
- Retries: Built-in exp backoff (1-16s, 5 max); fail → log/error.
- Targets: 95% <5s; 1K+/sec/node; horizontal scale via OTP supervisors.

### Auth/Security (P0)
- Keys: 64-char prefixed (zap_live_/test_); hashed (bcrypt via Comeonin); rotation support.
- Limits: Tiered (free:100/min; pro:1K; ent:custom); 429 w/Retry-After (via Hammer for rate limiting).
- Sec: HTTPS only; CORS; no sens data logged.

### Dashboard/Docs (P1)
- UI: Phoenix LiveView for interactive dashboard; keys, webhook config, tests, logs, retries, exports.
- Docs: OpenAPI/Swagger (via Phoenix Swagger); quickstarts; langs (curl/Elixir/JS).

### SDKs (P2)
- Elixir client (via HTTPoison/Tesla); JS client.

---

## 7. Non-Functional Requirements

### Performance/Scalability
| Metric | Target |
|--------|--------|
| Uptime | 99.9% |
| Ingestion p95 | <100ms |
| Delivery p95 | <5s |
| Throughput | 10K/sec/region |

- Scale: Stateless Phoenix endpoints; Oban queues; PG read replicas.
- Limits: 256KB payload; no batch MVP.

### Reliability
- Durability: PG ACID; daily backups; RPO 5min/RTO 15min.
- Failures: 503 on DB down; at-least-once via Oban; DLQ for fails.
- Monitoring: Telemetry/Prometheus; alerts (PagerDuty) on errors>1%, latency>200ms, queue>10K.

### Security/Compliance
- Auth: Keys only (MVP); org isolation.
- Protection: TLS 1.2+; encrypt at rest/transit; redact logs.
- Comp: GDPR export/delete; SOC2 audit logs.

### Observability
- Logs: Logger (JSON via LoggerJSON); 30d hot/1y cold.
- Metrics: :telemetry events to Prometheus.
- Tracing: OpenTelemetry (1% sample; 100% errors).

---

## 8. Technical Architecture

### Stack
- **API:** Phoenix 1.7+ (with LiveView for dashboard); Cowboy/Bandit server.
- **Data:** PG 16 (RDS Multi-AZ; Ecto 3.11+/Ecto.SQL); Oban 2.18+ for jobs (replaces Redis Streams—no extra dep, leverages PG for queuing/retries).
- **Infra:** Fly.io (MVP) → AWS ECS Fargate (Elixir Docker images); Terraform; GitHub Actions CI/CD.
- **Tools:** ExUnit for tests; Credo/Sobelow linting; Dialyzer types; Mix for deps/build.

**Why Oban over Redis:** Postgres-backed, no additional service (reduces ops), built-in cron/scheduling/retries, OTP-integrated for fault-tolerance. Handles 10K+ jobs/sec in prod; scales with PG replicas.

### Schema (Key Tables; Ecto-friendly)
```elixir
# lib/my_app/events.ex (Ecto Schema)
defmodule MyApp.Events.Event do
  use Ecto.Schema
  import Ecto.Changeset

  schema "events" do
    field :type, :string
    field :dedup_id, :string
    field :payload, :map  # JSONB via Jason
    belongs_to :organization, MyApp.Organizations.Organization
    timestamps(type: :utc_datetime)
  end

  # Changeset validation...
end

defmodule MyApp.Events.EventDelivery do
  use Ecto.Schema
  # status: :string (enum), attempts: :integer, etc.
  belongs_to :event, MyApp.Events.Event
end
```
Indexes: org_id/inserted_at; unique dedup; GIN on payload.

### Key Code Flows
- **Ingestion:** Plug pipeline (auth → rate/dedup via ETS/Cachex → Repo TX insert (event+delivery) → Oban.insert! → 201.
- **Worker:** Oban worker module: perform/1 fetches event → HTTPoison POST webhook → update status/retry or discard.

**Sample Ingestion Endpoint (Phoenix Controller):**
```elixir
# lib/my_app_web/controllers/event_controller.ex
defmodule MyAppWeb.EventController do
  use MyAppWeb, :controller
  import Ecto.Query

  def create(conn, %{"type" => type, "data" => data} = params) do
    org = authenticate_org!(conn)

    # Rate limit (Hammer)
    if Hammer.check_rate("rate:#{org.id}", 60_000, org.rate_limit_per_minute) != :ok do
      conn |> put_status(429) |> json(%{error: "Rate limited"}) |> halt()
    end

    # Dedup (ETS or PG unique)
    if params["dedup_id"] && Repo.exists?(from e in Event, where: e.dedup_id == ^params["dedup_id"]) do
      conn |> put_status(409) |> json(%{error: "Duplicate"}) |> halt()
    end

    # TX insert
    {:ok, event} = Repo.transaction(fn ->
      %Event{organization_id: org.id, type: type, payload: data}
      |> Event.changeset(params)
      |> Repo.insert!()

      %EventDelivery{event_id: event.id, status: "pending"}
      |> Repo.insert!()

      # Queue delivery
      %Oban.Job{
        queue: :delivery,
        worker: MyApp.Workers.DeliveryWorker,
        args: %{event_id: event.id, org_id: org.id}
      }
      |> Oban.insert!()

      event
    end)

    conn |> put_status(201) |> json(%{id: event.id, status: "pending"})
  end
end
```

**Sample Delivery Worker:**
```elixir
# lib/my_app/workers/delivery_worker.ex
defmodule MyApp.Workers.DeliveryWorker do
  use Oban.Worker, queue: :delivery, max_attempts: 5

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"event_id" => event_id}}) do
    event = Repo.get!(Event, event_id) |> Repo.preload(:organization)
    delivery = Repo.get_by!(EventDelivery, event_id: event_id)

    case HTTPoison.post(event.organization.webhook_url, Jason.encode!(%{event_id: event.id, type: event.type, data: event.payload}), [
      {"X-Zapier-Event-Id", event.id},
      {"Content-Type", "application/json"}
    ], timeout: 10_000) do
      {:ok, %HTTPoison.Response{status_code: code}} when code in [200, 201, 204] ->
        update_delivery(delivery, "delivered", code)
        :ok

      _ ->
        update_delivery(delivery, "failed", nil)
        {:error, "Delivery failed"}
    end
  end

  defp update_delivery(delivery, status, code) do
    delivery
    |> EventDelivery.changeset(%{status: status, response_status: code, attempts: delivery.attempts + 1})
    |> Repo.update!()
  end
end
```

### Diagram (Simplified)
```
Client → Load Balancer → Phoenix Endpoints → PG (events) + Oban (queue/jobs)
                                                ↓
Oban Workers (OTP) → Webhook (HTTPoison POST)
```

---

## 9. Deployment & GTM

### Deployment Phases
- **Phase 1 (Wk1-2):** Fly.io MVP (2 nodes, PG/Oban; Elixir release; $30/mo).
- **Phase 2 (Wk3-4):** AWS prod (ECS Fargate w/Distillery/Edeliver releases, Multi-AZ; $250/mo; Terraform).

### CI/CD
GitHub Actions: mix test → build release → deploy staging → prod (on main).

### GTM (30 Days)
- **Wk1:** Private alpha (10 devs; 5 success).
- **Wk2:** Public beta (blog/Twitter/HN; 100 signups).
- **Wk3:** Docs/tutorials/videos.
- **Wk4:** Partner outreach (top 50; 10 commits).

### Pricing
| Plan | Events/Mo | Limit/Min | Price |
|------|-----------|-----------|-------|
| Free | 1K | 100 | $0 |
| Pro | 100K | 1K | $29 |
| Business | 1M | 10K | $199 |
| Enterprise | Unlimited | Custom | Custom |

Add-ons: Retention (+$49/90d); SLA (+$99).

---

## 10. Risks & Mitigations

| Risk | Prob/Impact | Mitigation |
|------|-------------|------------|
| DB Bottleneck | Med/High | Replicas, partitioning; Oban pruning. |
| Low Adoption | Med/High | Free tier, outreach, docs. |
| Scale Costs | Low/Med | Usage alerts, auto-scale. |
| Security Breach | Low/Crit | Pen tests, bounty, SOC2. |

---

## 11. Open Questions
- Batch ingestion? (No for MVP.)
- Multi-webhook/org? (No; multi-keys workaround.)
- Queue: Oban vs RabbitMQ? (Oban MVP; RabbitMQ for ultra-high throughput later.)
- Retention: 7d free/30d pro/90d ent.

---

## 12. Timeline
- **Wk1:** Mix setup/Ecto/Phoenix API.
- **Wk2:** Auth/Oban workers.
- **Wk3:** Tests/monitoring/deploy.
- **Wk4:** LiveView docs/alpha.
- **Wk5-6:** Beta/launch.

**Post-Launch:** Mo2 scale; Mo3 SDKs; Mo4 ent features.

**Next:** Approve; kickoff; spike deploy.

**Elixir's OTP makes this even more resilient—let's ship.**
