defmodule ZapierTriggersWeb.Router do
  use Phoenix.Router

  import Plug.Conn
  import Phoenix.Controller

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug ZapierTriggersWeb.Plugs.Authenticate
    plug ZapierTriggersWeb.Plugs.RateLimit
  end

  scope "/api", ZapierTriggersWeb do
    pipe_through :api

    # Public endpoints (no authentication)
    post "/keys/generate", ApiKeyController, :generate

    # Authenticated endpoints
    pipe_through :authenticated

    # API key management
    get "/keys", ApiKeyController, :show
    post "/keys/rotate", ApiKeyController, :rotate

    # Webhook configuration
    post "/webhook/config", ManagementController, :configure_webhook

    # Event ingestion (async)
    post "/events", EventController, :create

    # Inbox management
    get "/inbox", ManagementController, :inbox
    post "/ack/:event_id", ManagementController, :acknowledge
  end

  # Health check endpoints
  scope "/health", ZapierTriggersWeb do
    get "/live", HealthController, :live
    get "/ready", HealthController, :ready
  end

  # Enable LiveDashboard in development
  if Application.compile_env(:zapier_triggers, :dev_routes) do
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through [:fetch_session, :protect_from_forgery]

      live_dashboard "/dashboard", metrics: ZapierTriggersWeb.Telemetry
    end
  end
end
