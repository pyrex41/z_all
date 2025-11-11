"""
Gunicorn configuration for production deployment.

This config optimizes for high-performance async FastAPI applications.
"""
import multiprocessing
import os

# Server socket
bind = "0.0.0.0:8000"
backlog = 2048

# Worker processes
workers = int(os.getenv("GUNICORN_WORKERS", multiprocessing.cpu_count() * 2))
worker_class = "uvicorn.workers.UvicornWorker"
worker_connections = 1000
max_requests = 10000  # Restart workers after N requests (prevents memory leaks)
max_requests_jitter = 1000  # Add randomness to prevent all workers restarting simultaneously
timeout = 30
graceful_timeout = 10
keepalive = 5

# Logging
accesslog = "-"  # Log to stdout
errorlog = "-"   # Log to stderr
loglevel = os.getenv("LOG_LEVEL", "warning")
access_log_format = '%(h)s %(l)s %(u)s %(t)s "%(r)s" %(s)s %(b)s "%(f)s" "%(a)s" %(D)s'

# Process naming
proc_name = "zapier-triggers-api"

# Server mechanics
daemon = False
pidfile = None
user = None
group = None
tmp_upload_dir = None

# SSL (if needed)
# keyfile = "/path/to/keyfile"
# certfile = "/path/to/certfile"

# Performance tuning
preload_app = True  # Load app code before forking workers (saves memory)
reuse_port = True   # Allow multiple workers to bind to same port (Linux 3.9+)

# Hooks
def on_starting(server):
    """Called before master process is initialized."""
    print(f"Starting Gunicorn with {workers} workers")

def on_reload(server):
    """Called when configuration is reloaded."""
    print("Configuration reloaded")

def when_ready(server):
    """Called after server is ready."""
    print(f"Server ready at {bind}")

def worker_int(worker):
    """Called when worker receives INT or QUIT signal."""
    print(f"Worker {worker.pid} received INT/QUIT signal")

def worker_abort(worker):
    """Called when worker times out."""
    print(f"Worker {worker.pid} timed out (exceeded {timeout}s)")
