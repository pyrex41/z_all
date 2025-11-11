# Security Configuration Guide

## ⚠️ IMPORTANT: Development vs Production

The default configuration is for **DEVELOPMENT ONLY** and is **NOT SECURE** for production use.

---

## 🔒 Production Security Checklist

### 1. PostgreSQL Authentication (CRITICAL)

**Current State (Development):**
```conf
# pg_hba.conf
host    all             all             127.0.0.1/32            trust
```

**⚠️ WARNING**: This allows **password-less access** to PostgreSQL!

**Production Configuration:**

```bash
# Step 1: Create a dedicated database user with password
sudo -u postgres psql
CREATE USER zapier_triggers_app WITH PASSWORD 'strong_random_password_here';
GRANT ALL PRIVILEGES ON DATABASE zapier_triggers TO zapier_triggers_app;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO zapier_triggers_app;
\q

# Step 2: Update pg_hba.conf to require password authentication
sudo nano /etc/postgresql/16/main/pg_hba.conf

# Change from:
# host    all             all             127.0.0.1/32            trust

# To:
host    all             all             127.0.0.1/32            scram-sha-256

# Step 3: Reload PostgreSQL
sudo service postgresql reload

# Step 4: Update environment variables
export DB_USER=zapier_triggers_app
export DB_PASSWORD=strong_random_password_here
```

### 2. Environment Variables (CRITICAL)

**Never hardcode credentials!** Use environment variables:

```bash
# Required for Production:
export DB_NAME=zapier_triggers
export DB_USER=zapier_triggers_app
export DB_PASSWORD=your_secure_password
export DB_HOST=localhost
export DB_PORT=5432

# Optional:
export PORT=5001
export RATE_LIMIT_RPM=1000
export DEDUP_MAX_SIZE=10000
```

**Security Best Practices:**
- Store passwords in a secrets manager (AWS Secrets Manager, Vault, etc.)
- Use IAM authentication if on AWS RDS
- Rotate passwords regularly
- Never commit `.env` files to git

### 3. Network Security

**Firewall Configuration:**

```bash
# Only allow PostgreSQL connections from localhost
sudo ufw allow from 127.0.0.1 to any port 5432

# Restrict HTTP server access
sudo ufw allow 5001/tcp  # Or use reverse proxy
```

**Reverse Proxy (Recommended):**

Use nginx or Apache as a reverse proxy:

```nginx
# /etc/nginx/sites-available/zapier-triggers
server {
    listen 80;
    server_name api.example.com;

    location / {
        proxy_pass http://localhost:5001;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Security headers
        add_header X-Content-Type-Options nosniff;
        add_header X-Frame-Options DENY;
        add_header X-XSS-Protection "1; mode=block";
    }
}
```

### 4. SSL/TLS Configuration

**Enable SSL for PostgreSQL:**

```bash
# Edit postgresql.conf
sudo nano /etc/postgresql/16/main/postgresql.conf

# Enable SSL
ssl = on
ssl_cert_file = '/path/to/server.crt'
ssl_key_file = '/path/to/server.key'

# Restart PostgreSQL
sudo service postgresql restart
```

**Update connection string:**
```lisp
(defun load-config ()
  ;; ... existing config ...
  (setf (gethash "db-ssl" *config*) (or (uiop:getenv "DB_SSL") "require")))
```

### 5. Input Validation (HIGH PRIORITY)

**Current State**: Minimal validation

**Recommended Additions:**

```lisp
;; Add to simple-server.lisp
(defun validate-org-name (name)
  "Validate organization name"
  (and name
       (stringp name)
       (> (length name) 0)
       (<= (length name) 255)
       (not (find-if (lambda (c) (member c '(#\< #\> #\& #\"))) name))))

(defun validate-event-type (type)
  "Validate event type format"
  (and type
       (stringp type)
       (cl-ppcre:scan "^[a-z0-9._-]+$" type)
       (<= (length type) 100)))

(defun validate-payload-size (payload)
  "Ensure payload isn't too large"
  (let ((json-string (with-output-to-string (s)
                      (yason:encode payload s))))
    (< (length json-string) (* 1024 1024)))) ; 1MB limit
```

### 6. Rate Limiting (PRODUCTION TUNING)

**Adjust based on your needs:**

```bash
# Conservative (default)
export RATE_LIMIT_RPM=1000

# High-traffic
export RATE_LIMIT_RPM=10000

# Per-tier limits (future enhancement)
# export RATE_LIMIT_FREE=100
# export RATE_LIMIT_PROFESSIONAL=1000
# export RATE_LIMIT_ENTERPRISE=10000
```

### 7. Connection Pool Configuration

**Production Settings:**

```bash
# Increase pool size for higher concurrency
# Default: 10 connections
# Recommended for production: 20-50 connections

export DB_POOL_SIZE=20
export DB_POOL_TIMEOUT=30  # seconds (future enhancement)
```

**Monitor Connection Usage:**
```bash
# Check PostgreSQL connections
sudo -u postgres psql -c "SELECT count(*) FROM pg_stat_activity WHERE datname='zapier_triggers';"
```

### 8. Logging and Monitoring

**Production Logging:**

```bash
# Disable verbose logging in production
export LOG_LEVEL=warn  # future enhancement

# Log to file instead of stdout
export LOG_FILE=/var/log/zapier-triggers/api.log
```

**Monitoring Checklist:**
- [ ] Database connection pool usage
- [ ] Rate limit violations
- [ ] Failed authentication attempts
- [ ] Error rates per endpoint
- [ ] Response time percentiles

---

## 🛡️ Security Hardening Checklist

Before deploying to production, verify:

### Database Security
- [ ] PostgreSQL using password authentication (not `trust`)
- [ ] Dedicated database user with limited privileges
- [ ] SSL/TLS enabled for database connections
- [ ] Database user password stored in secrets manager
- [ ] Regular password rotation schedule established
- [ ] Database backups configured and tested

### Application Security
- [ ] All environment variables configured
- [ ] No hardcoded credentials in code
- [ ] Input validation enabled for all endpoints
- [ ] Rate limiting configured appropriately
- [ ] Error messages don't leak sensitive information
- [ ] Logging configured (but not logging sensitive data)

### Network Security
- [ ] Firewall rules configured
- [ ] Reverse proxy configured (nginx/Apache)
- [ ] HTTPS enabled (via reverse proxy)
- [ ] Database only accessible from application server
- [ ] Security headers configured

### Operational Security
- [ ] Monitoring and alerting configured
- [ ] Incident response plan documented
- [ ] Access logs retained and reviewed
- [ ] Dependencies regularly updated
- [ ] Security patches applied promptly

---

## 🚨 Common Security Mistakes

### ❌ DON'T DO THIS:

```bash
# Hardcoded password in code
(pomo:connect "zapier_triggers" "postgres" "password123" "localhost")

# Committing .env files
git add .env  # NEVER DO THIS!

# Running as root
sudo sbcl --load simple-server.lisp  # Use dedicated user instead

# Exposing PostgreSQL to internet
# pg_hba.conf
host    all             all             0.0.0.0/0            trust  # DANGEROUS!
```

### ✅ DO THIS INSTEAD:

```bash
# Use environment variables
export DB_PASSWORD=$(vault read secret/db/password)

# Use .gitignore
echo ".env" >> .gitignore

# Run as dedicated user
sudo useradd -r -s /bin/false zapier-triggers
sudo -u zapier-triggers sbcl --load simple-server.lisp

# Restrict PostgreSQL access
# pg_hba.conf
host    all             all             127.0.0.1/32         scram-sha-256
```

---

## 📚 Additional Resources

- [PostgreSQL Security Best Practices](https://www.postgresql.org/docs/current/auth-pg-hba-conf.html)
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [Common Lisp Security Considerations](https://www.cliki.net/security)
- [SBCL Security](https://www.sbcl.org/manual/#Security-Considerations)

---

## 🆘 Security Issues?

If you discover a security vulnerability:

1. **DO NOT** create a public GitHub issue
2. Email security@yourcompany.com
3. Provide details: impact, reproduction steps, suggested fix
4. Allow reasonable time for patching before disclosure

---

## 📝 Version History

- **2025-11-11**: Initial security guide created
- **TBD**: Add SSL/TLS configuration examples
- **TBD**: Add OAuth integration guide
