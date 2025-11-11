defmodule ZapierTriggers.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    # Skip EventQueueProcessor in test environment to reduce database connections
    queue_processor =
      if Mix.env() == :test do
        []
      else
        [ZapierTriggers.Workers.EventQueueProcessor]
      end

    children =
      [
        ZapierTriggersWeb.Telemetry,
        ZapierTriggers.Repo,
        {DNSCluster, query: Application.get_env(:zapier_triggers, :dns_cluster_query) || :ignore},
        {Phoenix.PubSub, name: ZapierTriggers.PubSub},
        # Start Cachex for deduplication cache
        Supervisor.child_spec({Cachex, name: :dedup_cache}, id: :dedup_cache),
        # Start Cachex for authentication cache (5-minute TTL)
        Supervisor.child_spec({Cachex, name: :auth_cache}, id: :auth_cache),
        # Start Oban for background jobs
        {Oban, Application.fetch_env!(:zapier_triggers, Oban)},
        # Start the endpoint (HTTP server)
        ZapierTriggersWeb.Endpoint
      ] ++ queue_processor

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: ZapierTriggers.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    ZapierTriggersWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
