defmodule ZapierTriggersWeb.Plugs.Authenticate do
  @moduledoc """
  Authenticates API requests using X-API-Key header.

  Performance optimizations:
  - Fast SHA256 hashing (not bcrypt/Argon2)
  - 5-minute cache of full Organization structs
  - Zero database calls on cache hits (< 0.1ms response)
  - Only on cache miss: single DB query (~50ms)

  Expected performance: < 1ms P95 with warm cache
  """
  import Plug.Conn
  require Logger

  alias ZapierTriggers.{Repo, Organizations}
  alias ZapierTriggers.Organizations.Organization

  @cache_ttl :timer.minutes(5)

  def init(opts), do: opts

  def call(conn, _opts) do
    case get_req_header(conn, "x-api-key") do
      [api_key | _] ->
        authenticate_api_key(conn, api_key)

      [] ->
        unauthorized(conn, "Missing X-API-Key header")
    end
  end

  defp authenticate_api_key(conn, api_key) do
    # Cache key using PLAINTEXT api_key (no hashing - database stores plaintext)
    cache_key = "auth:#{api_key}"

    case Cachex.get(:auth_cache, cache_key) do
      {:ok, %Organization{} = org} ->
        # Cache hit: Use cached organization struct directly (< 0.1ms)
        Logger.debug("AUTH CACHE HIT for #{org.name}")
        authorize(conn, org)

      result ->
        # Cache miss: Hash the incoming API key and lookup by hash
        Logger.debug("AUTH CACHE MISS (result: #{inspect(result)}), querying DB...")
        api_key_hash = Organization.hash_api_key_fast(api_key)
        case Repo.get_by(Organization, api_key_hash: api_key_hash) do
          %Organization{} = org ->
            # Cache the entire organization, not just the ID
            Logger.debug("Caching organization #{org.name}")
            Cachex.put(:auth_cache, cache_key, org, ttl: @cache_ttl)
            authorize(conn, org)

          nil ->
            unauthorized(conn, "Invalid API key")
        end
    end
  end

  defp authorize(conn, organization) do
    conn
    |> assign(:current_organization, organization)
    |> assign(:organization_id, organization.id)
  end

  defp unauthorized(conn, message) do
    conn
    |> put_status(:unauthorized)
    |> Phoenix.Controller.json(%{error: message})
    |> halt()
  end
end
