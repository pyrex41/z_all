defmodule ZapierTriggers.Repo do
  use Ecto.Repo,
    otp_app: :zapier_triggers,
    adapter: Ecto.Adapters.Postgres
end
