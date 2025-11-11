defmodule ZapierTriggersWeb.Plugs.Authenticate do
  @moduledoc """
  Authenticates API requests using X-API-Key header.
  Uses fast SHA256 hashing with 5-minute caching for optimal performance.
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
    api_key_hash = Organization.hash_api_key_fast(api_key)
    cache_key = "auth:#{api_key_hash}"

    case Cachex.get(:auth_cache, cache_key) do
      {:ok, org_id} when not is_nil(org_id) ->
        # Cache hit: Load by primary key (fast ~10ms)
        case Repo.get(Organization, org_id) do
          %Organization{} = org ->
            authorize(conn, org)

          nil ->
            # Organization was deleted, invalidate cache
            Cachex.del(:auth_cache, cache_key)
            unauthorized(conn, "Invalid API key")
        end

      _ ->
        # Cache miss: Lookup by hash and cache result (~70ms)
        case Repo.get_by(Organization, api_key_hash: api_key_hash) do
          %Organization{} = org ->
            Cachex.put(:auth_cache, cache_key, org.id, ttl: @cache_ttl)
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
