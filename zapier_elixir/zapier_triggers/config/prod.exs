import Config

# Do not print debug messages in production
config :logger, level: :info

# Configure SSL/HTTPS for production
config :zapier_triggers, ZapierTriggersWeb.Endpoint,
  force_ssl: [hsts: true, rewrite_on: [:x_forwarded_host, :x_forwarded_port, :x_forwarded_proto], exclude: ["/health/live", "/health/ready"]]

# Runtime production configuration, including reading
# of environment variables, is done on config/runtime.exs.
