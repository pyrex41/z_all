defmodule ZapierTriggersWeb.ApiKeyController do
  use Phoenix.Controller, formats: [:json]

  alias ZapierTriggers.{Repo, Organizations}
  alias ZapierTriggers.Organizations.Organization

  @doc """
  Generate a new API key for an organization.
  This is a public endpoint (no authentication required).
  """
  def generate(conn, %{"organization_name" => org_name} = params) do
    tier = Map.get(params, "tier", "free")

    # Generate API key
    {api_key, api_key_hash} = Organization.generate_api_key()

    # Create organization
    changeset = Organization.changeset(%Organization{}, %{
      name: org_name,
      api_key_hash: api_key_hash,
      tier: tier
    })

    case Repo.insert(changeset) do
      {:ok, organization} ->
        # Return API key only once (never stored in plaintext)
        conn
        |> put_status(:created)
        |> json(%{
          api_key: api_key,
          organization_id: organization.id,
          tier: organization.tier,
          rate_limit_per_minute: Organization.get_rate_limit(organization),
          message: "Save this API key securely - it won't be shown again"
        })

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Failed to create organization", details: changeset.errors})
    end
  end

  def generate(conn, _params) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: "Missing required field: organization_name"})
  end

  @doc """
  Get API key information (requires authentication).
  Does not return the actual key.
  """
  def show(conn, _params) do
    organization = conn.assigns.current_organization

    conn
    |> json(%{
      organization_id: organization.id,
      organization_name: organization.name,
      tier: organization.tier,
      rate_limit_per_minute: Organization.get_rate_limit(organization),
      webhook_url: organization.webhook_url
    })
  end

  @doc """
  Rotate API key (requires authentication).
  Invalidates the old key and returns a new one.
  """
  def rotate(conn, _params) do
    organization = conn.assigns.current_organization

    # Generate new API key
    {api_key, api_key_hash} = Organization.generate_api_key()

    # Update organization
    changeset = Organization.changeset(organization, %{api_key_hash: api_key_hash})

    case Repo.update(changeset) do
      {:ok, _updated_org} ->
        # Invalidate auth cache for old key
        # (New key will be cached on first use)

        conn
        |> json(%{
          api_key: api_key,
          message: "API key rotated successfully - update your applications immediately"
        })

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Failed to rotate API key", details: changeset.errors})
    end
  end
end
