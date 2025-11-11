import Config

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :zapier_triggers, ZapierTriggers.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "zapier_triggers_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 2

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :zapier_triggers, ZapierTriggersWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "BzV/4uNizlliHhjEfUR6XsjckkBZ+EeCnpvh0Ep2qpVwgH5t1dPerhJEBXUjprCQ",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Configure Oban for testing with minimal resources
config :zapier_triggers, Oban,
  repo: ZapierTriggers.Repo,
  plugins: false,
  queues: false,
  testing: :inline
