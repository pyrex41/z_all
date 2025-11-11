defmodule ZapierTriggersWeb.HealthController do
  use Phoenix.Controller, formats: [:json]

  alias ZapierTriggers.Repo

  @doc """
  Liveness probe - checks if application is running.
  """
  def live(conn, _params) do
    conn
    |> json(%{status: "ok", timestamp: DateTime.utc_now()})
  end

  @doc """
  Readiness probe - checks if application can serve traffic.
  Verifies database connectivity.
  """
  def ready(conn, _params) do
    case Repo.query("SELECT 1", []) do
      {:ok, _} ->
        conn
        |> json(%{
          status: "ready",
          database: "connected",
          timestamp: DateTime.utc_now()
        })

      {:error, reason} ->
        conn
        |> put_status(:service_unavailable)
        |> json(%{
          status: "not_ready",
          database: "disconnected",
          error: inspect(reason),
          timestamp: DateTime.utc_now()
        })
    end
  end
end
