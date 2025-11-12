defmodule ZapierTriggers.Organizations.Organization do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "organizations" do
    field :name, :string
    field :api_key_hash, :string  # Database stores SHA-256 hashed API keys
    field :webhook_url, :string
    field :rate_limit_per_minute, :integer, default: 100
    field :tier, :string, default: "free"

    has_many :events, ZapierTriggers.Events.Event
    has_many :event_queue_items, ZapierTriggers.Events.EventQueue

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(organization, attrs) do
    organization
    |> cast(attrs, [:name, :api_key_hash, :webhook_url, :rate_limit_per_minute, :tier])
    |> validate_required([:name, :api_key_hash])
    |> validate_inclusion(:tier, ["free", "pro", "business", "enterprise"])
    |> unique_constraint(:api_key_hash)
  end

  @doc """
  Generates a random API key (256-bit, base64-url encoded).
  Returns the plaintext key (to show to user) and the hash (to store in DB).
  """
  def generate_api_key do
    # Generate 32 random bytes (256 bits)
    api_key = :crypto.strong_rand_bytes(32) |> Base.url_encode64(padding: false)
    api_key_hash = hash_api_key_fast(api_key)

    {api_key, api_key_hash}
  end

  @doc """
  Fast hashing using SHA256 for API keys.
  API keys are system-generated 256-bit random strings with maximum entropy,
  so SHA256 provides sufficient security with ~1000x better performance than bcrypt.
  """
  def hash_api_key_fast(api_key) when is_binary(api_key) do
    :crypto.hash(:sha256, api_key)
    |> Base.encode16(case: :lower)
  end

  @doc """
  Verifies an API key against the stored hash using constant-time comparison.
  Prevents timing attacks.
  """
  def verify_api_key(%__MODULE__{} = organization, api_key) do
    computed_hash = hash_api_key_fast(api_key)
    Plug.Crypto.secure_compare(computed_hash, organization.api_key_hash)
  end

  @doc """
  Returns rate limit based on tier.
  """
  def get_rate_limit(%__MODULE__{tier: tier}) do
    case tier do
      "free" -> 100
      "pro" -> 1_000
      "business" -> 10_000
      "enterprise" -> 100_000
      _ -> 100
    end
  end
end
