defmodule ZapierTriggers.Events.Event do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "events" do
    field :type, :string
    field :dedup_id, :string
    field :payload, :map

    belongs_to :organization, ZapierTriggers.Organizations.Organization
    has_many :deliveries, ZapierTriggers.Events.EventDelivery

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(event, attrs) do
    event
    |> cast(attrs, [:type, :dedup_id, :payload, :organization_id])
    |> validate_required([:type, :payload, :organization_id])
    |> unique_constraint(:dedup_id)
  end
end
