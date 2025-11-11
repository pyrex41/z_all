# Zapier Triggers API - Product Requirements Document (PRD)

**Version:** 1.1  
**Status:** Refined Draft  
**Last Updated:** November 10, 2025  
**Owner:** Reuben (Technical Lead)  
**Stakeholders:** Developers, Automation Engineers, Integration Partners

---

## 1. Executive Summary

### The Opportunity
Zapier lacks a unified public API for real-time event ingestion, forcing fragmented custom integrations and polling-based triggers. This limits adoption of event-driven automation.

### The Solution
A simple RESTful Triggers API for webhook ingestion, durable storage, and reliable delivery to Zapier workflows. Modeled after proven patterns (e.g., Stripe Webhooks), it's a lightweight primitive: three core endpoints, one worker, standard CRUD.

### Success Criteria
- **Technical:** 99.9% uptime, <100ms p95 ingestion latency, <5s p95 delivery.
- **Adoption:** 50+ integrations migrated in 6 months; 10K+ new Zaps.
- **Developer NPS:** 50+ via surveys.
- **Revenue:** Enable usage-based pricing for high-volume events.

**Why a Layup:** Solved problem, proven tech, small scope (MVP in 2 weeks), high leverage for real-time workflows.

---

## 2. Problem Statement

**Current Pains:**
- **Developers:** Custom integrations per app; no standard real-time push; polling delays (15min free tier).
- **Users:** Delayed automations; limited to pre-built integrations; no reactive workflows.
- **Platform:** Fragmented triggers across 6K+ integrations; high maintenance; misses agentic use cases.

**Core Insight:** Webhooks are commoditized—make them trivial for any event source to connect to Zapier.

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
- Worker: Queue (Redis Streams); POST to webhook w/headers (X-Zapier-Event-*); 10s timeout.
- Retries: Exp backoff (1-16s, 5 max); fail → log/error.
- Targets: 95% <5s; 1K+/sec/worker; horizontal scale.

### Auth/Security (P0)
- Keys: 64-char prefixed (zap_live_/test_); hashed (bcrypt); rotation support.
- Limits: Tiered (free:100/min; pro:1K; ent:custom); 429 w/Retry-After.
- Sec: HTTPS only; CORS; no sens data logged.

### Dashboard/Docs (P1)
- UI: htmx + FastAPI templates; keys, webhook config, tests, logs, retries, exports.
- Docs: OpenAPI/Swagger; quickstarts; langs (curl/Python/JS).

### SDKs (P2)
- Python/JS clients for create/list/ack.

---

## 7. Non-Functional Requirements

### Performance/Scalability
| Metric | Target |
|--------|--------|
| Uptime | 99.9% |
| Ingestion p95 | <100ms |
| Delivery p95 | <5s |
| Throughput | 10K/sec/region |

- Scale: Stateless API/workers; read replicas; Redis cluster.
- Limits: 256KB payload; no batch MVP.

### Reliability
- Durability: PG ACID; daily backups; RPO 5min/RTO 15min.
- Failures: 503 on DB down; at-least-once via Streams; DLQ for fails.
- Monitoring: Prometheus/Grafana; alerts (PagerDuty) on errors>1%, latency>200ms, queue>10K.

### Security/Compliance
- Auth: Keys only (MVP); org isolation.
- Protection: TLS 1.2+; encrypt at rest/transit; redact logs.
- Comp: GDPR export/delete; SOC2 audit logs.

### Observability
- Logs: Structured JSON; 30d hot/1y cold.
- Metrics: Requests, latencies, deliveries (per org/status).
- Tracing: OTEL (1% sample; 100% errors).

---

## 8. Technical Architecture

### Stack
- **API:** FastAPI (async); Uvicorn/Gunicorn.
- **Data:** PG 16 (RDS Multi-AZ; SQLModel/Alembic); Redis 7 (ElastiCache; Streams).
- **Infra:** Fly.io (MVP) → AWS ECS Fargate; Terraform; GitHub Actions CI/CD.
- **Tools:** pytest; ruff; mypy; Docker Compose local.

### Schema (Key Tables)
```sql
organizations: id, name, api_key_hash, webhook_url, rate_limit, plan.
events: id, org_id, type, dedup_id, payload (JSONB), created_at (partitioned).
event_deliveries: id, event_id, status, attempts, error_message.
audit_log: id, org_id, action, metadata.
```
Indexes: org/created_at; dedup unique; GIN on payload.

### Key Code Flows
- **Ingestion:** Auth → rate/dedup (Redis) → TX insert (event+delivery) → queue → 201.
- **Worker:** XREADGROUP → fetch → POST webhook → update status/retry or ack.

### Diagram (Simplified)
```
Client → ALB → API Servers (FastAPI) → PG (events) + Redis (dedup/rate/queue)
                                      ↓
Redis Streams → Workers (Python) → Webhook (POST)
```

---

## 9. Deployment & GTM

### Deployment Phases
- **Phase 1 (Wk1-2):** Fly.io MVP (2 API, 1 worker, PG/Redis; $30/mo).
- **Phase 2 (Wk3-4):** AWS prod (ECS Fargate, Multi-AZ; $250/mo; Terraform).

### CI/CD
GitHub Actions: test → deploy staging → prod (on main).

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
| DB Bottleneck | Med/High | Replicas, partitioning. |
| Low Adoption | Med/High | Free tier, outreach, docs. |
| Scale Costs | Low/Med | Usage alerts, auto-scale. |
| Security Breach | Low/Crit | Pen tests, bounty, SOC2. |

---

## 11. Open Questions
- Batch ingestion? (No for MVP.)
- Multi-webhook/org? (No; multi-keys workaround.)
- Queue: Redis vs SQS? (Redis MVP; SQS ent.)
- Retention: 7d free/30d pro/90d ent.

---

## 12. Timeline
- **Wk1:** Setup/DB/API.
- **Wk2:** Auth/delivery/worker.
- **Wk3:** Tests/monitoring/deploy.
- **Wk4:** Docs/alpha.
- **Wk5-6:** Beta/launch.

**Post-Launch:** Mo2 scale; Mo3 SDKs; Mo4 ent features.

**Next:** Approve; kickoff; spike deploy.

**Let's ship a tight MVP.**
