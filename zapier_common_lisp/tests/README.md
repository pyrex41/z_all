# Zapier Triggers API - Test Suite

## Smoke Tests

Basic smoke tests to verify the API is functioning correctly.

### Prerequisites

- SBCL installed
- Server running on `http://localhost:5001`
- PostgreSQL running with `zapier_triggers` database

### Running Tests

```bash
# Start the server first (in another terminal):
cd zapier_common_lisp
sbcl --load simple-server.lisp --eval '(zapier-simple:start-server)'

# Run smoke tests:
sbcl --load tests/smoke-tests.lisp --eval '(zapier-smoke-tests:run-tests)' --quit
```

### Test Coverage

The smoke tests verify:

1. **Health Check** - Server is responding
2. **Cache Stats** - Monitoring endpoint works
3. **API Key Generation** - Can create organizations
4. **Authentication** - Rejects requests without API keys
5. **Event Creation** - Can create events with valid API key
6. **Deduplication** - Duplicate events are detected
7. **Inbox Retrieval** - Can query events from database
8. **Invalid Keys** - Properly rejects invalid API keys

### Expected Output

```
═══════════════════════════════════════════════════════════
   Zapier Triggers API - Smoke Tests
═══════════════════════════════════════════════════════════

Running tests against: http://localhost:5001

✅ PASS: Health check
✅ PASS: Cache stats endpoint
✅ PASS: Generate API key
   Generated API key: sk_...
✅ PASS: Create event without API key (auth required)
✅ PASS: Create event
   Created event: ...
✅ PASS: Duplicate detection
✅ PASS: Get inbox
   Retrieved X events from inbox
✅ PASS: Invalid API key rejected

═══════════════════════════════════════════════════════════
   Test Summary
═══════════════════════════════════════════════════════════

Total:  8 tests
Passed: 8 (100%)
Failed: 0

✅ All tests passed!
```

### Adding New Tests

To add a new test:

1. Define a test function in `smoke-tests.lisp`:
   ```lisp
   (defun test-my-feature ()
     (handler-case
         ;; Test logic here
         (test-passed "My feature")
       (error (e)
         (test-failed "My feature" (format nil "Exception: ~a" e)))))
   ```

2. Add it to `run-tests`:
   ```lisp
   (defun run-tests ()
     ...
     (test-my-feature)
     ...)
   ```

### Continuous Integration

For CI/CD pipelines, the test runner returns an exit code:
- `0` - All tests passed
- `1` - Some tests failed

Example CI script:
```bash
#!/bin/bash
sbcl --load tests/smoke-tests.lisp --eval '(if (zapier-smoke-tests:run-tests) (sb-ext:exit :code 0) (sb-ext:exit :code 1))'
```
