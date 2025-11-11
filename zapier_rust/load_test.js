import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate } from 'k6/metrics';

const errorRate = new Rate('errors');

export const options = {
  stages: [
    { duration: '30s', target: 50 },   // Ramp up to 50 users
    { duration: '1m', target: 100 },   // Ramp up to 100 users
    { duration: '2m', target: 100 },   // Stay at 100 users for 2 minutes
    { duration: '30s', target: 0 },    // Ramp down to 0 users
  ],
  thresholds: {
    http_req_duration: ['p(95)<100'],  // 95% of requests must complete within 100ms
    http_req_failed: ['rate<0.01'],    // Error rate must be below 1%
    errors: ['rate<0.01'],             // Custom error rate must be below 1%
  },
};

const BASE_URL = __ENV.BASE_URL || 'http://localhost:3000';
const API_KEY = __ENV.API_KEY || 'test_api_key';

export default function () {
  const payload = JSON.stringify({
    type: 'user.login',
    dedup_id: `event_${__VU}_${__ITER}_${Date.now()}`,
    payload: {
      user_id: `user_${__VU}`,
      timestamp: new Date().toISOString(),
      action: 'login',
      ip_address: '192.168.1.1',
      user_agent: 'k6-load-test',
    },
  });

  const params = {
    headers: {
      'Content-Type': 'application/json',
      'X-API-Key': API_KEY,
    },
  };

  const res = http.post(`${BASE_URL}/api/events`, payload, params);

  const success = check(res, {
    'status is 202': (r) => r.status === 202,
    'response time < 100ms': (r) => r.timings.duration < 100,
    'response time < 50ms': (r) => r.timings.duration < 50,
    'response time < 10ms': (r) => r.timings.duration < 10,
    'has event id': (r) => JSON.parse(r.body).id !== undefined,
    'status is accepted': (r) => JSON.parse(r.body).status === 'accepted',
  });

  errorRate.add(!success);

  // Small random sleep to simulate realistic traffic
  sleep(Math.random() * 0.1);
}

export function handleSummary(data) {
  return {
    'stdout': textSummary(data, { indent: ' ', enableColors: true }),
    'load_test_results.json': JSON.stringify(data),
  };
}

function textSummary(data, options) {
  const indent = options.indent || '';
  const enableColors = options.enableColors !== false;

  let summary = '\n';
  summary += `${indent}=== Load Test Summary ===\n\n`;

  // Request metrics
  summary += `${indent}Requests:\n`;
  summary += `${indent}  Total: ${data.metrics.http_reqs.values.count}\n`;
  summary += `${indent}  Failed: ${data.metrics.http_req_failed.values.rate * 100}%\n\n`;

  // Duration metrics
  summary += `${indent}Response Time:\n`;
  summary += `${indent}  p50: ${data.metrics.http_req_duration.values['p(50)']}ms\n`;
  summary += `${indent}  p95: ${data.metrics.http_req_duration.values['p(95)']}ms\n`;
  summary += `${indent}  p99: ${data.metrics.http_req_duration.values['p(99)']}ms\n`;
  summary += `${indent}  avg: ${data.metrics.http_req_duration.values.avg}ms\n`;
  summary += `${indent}  max: ${data.metrics.http_req_duration.values.max}ms\n\n`;

  // Throughput
  const duration = data.state.testRunDurationMs / 1000;
  const rps = data.metrics.http_reqs.values.count / duration;
  summary += `${indent}Throughput: ${rps.toFixed(2)} req/s\n\n`;

  // Thresholds
  summary += `${indent}Thresholds:\n`;
  for (const [name, threshold] of Object.entries(data.metrics)) {
    if (threshold.thresholds) {
      for (const [thresholdName, thresholdResult] of Object.entries(threshold.thresholds)) {
        const status = thresholdResult.ok ? '✓' : '✗';
        summary += `${indent}  ${status} ${thresholdName}\n`;
      }
    }
  }

  return summary;
}
