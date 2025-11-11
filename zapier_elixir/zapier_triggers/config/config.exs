# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

config :zapier_triggers,
  ecto_repos: [ZapierTriggers.Repo],
  generators: [timestamp_type: :utc_datetime, binary_id: true]

# Configures the endpoint
config :zapier_triggers, ZapierTriggersWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [json: ZapierTriggersWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: ZapierTriggers.PubSub,
  live_view: [signing_salt: "kI41mfwr"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :organization_id, :event_id]

# Filter sensitive parameters from logs
config :phoenix, :filter_parameters, ["api_key", "password", "token", "secret", "authorization"]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Configure Hammer rate limiting
config :hammer,
  backend: {Hammer.Backend.ETS, [expiry_ms: 60_000 * 60 * 2, cleanup_interval_ms: 60_000 * 10]}

# Configure Oban job queue
config :zapier_triggers, Oban,
  repo: ZapierTriggers.Repo,
  plugins: [
    # Prune completed jobs after 7 days
    {Oban.Plugins.Pruner, max_age: 60 * 60 * 24 * 7},
    # Run deduplication cleanup daily at 2 AM
    {Oban.Plugins.Cron,
     crontab: [
       {"0 2 * * *", ZapierTriggers.Workers.DeduplicationCleanup}
     ]}
  ],
  queues: [delivery: 50]

# Webhook delivery configuration (for performance testing)
# Can be overridden via DISABLE_WEBHOOK_DELIVERY=true environment variable
disable_webhook_delivery = System.get_env("DISABLE_WEBHOOK_DELIVERY") == "true"
config :zapier_triggers, :disable_webhook_delivery, disable_webhook_delivery

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
