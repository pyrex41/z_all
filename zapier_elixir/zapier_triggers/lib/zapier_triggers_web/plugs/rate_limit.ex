defmodule ZapierTriggersWeb.Plugs.RateLimit do
  @moduledoc """
  Rate limiting plug using Hammer.
  Enforces tier-based rate limits per organization.
  """
  import Plug.Conn
  require Logger

  alias ZapierTriggers.Organizations.Organization

  def init(opts), do: opts

  def call(conn, _opts) do
    organization = conn.assigns[:current_organization]

    if organization do
      check_rate_limit(conn, organization)
    else
      # No organization assigned yet (public endpoints)
      conn
    end
  end

  defp check_rate_limit(conn, organization) do
    rate_limit = Organization.get_rate_limit(organization)
    bucket_key = "rate_limit:#{organization.id}"

    case Hammer.check_rate(bucket_key, 60_000, rate_limit) do
      {:allow, _count} ->
        conn
        |> put_resp_header("x-ratelimit-limit", to_string(rate_limit))
        |> put_resp_header("x-ratelimit-remaining", to_string(rate_limit - _count))

      {:deny, _limit} ->
        Logger.warning("Rate limit exceeded for organization #{organization.id}",
          organization_id: organization.id,
          tier: organization.tier
        )

        conn
        |> put_status(:too_many_requests)
        |> Phoenix.Controller.json(%{
          error: "Rate limit exceeded",
          limit: rate_limit,
          window: "1 minute"
        })
        |> halt()
    end
  end
end
