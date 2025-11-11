defmodule ZapierTriggers.Events.EventQueue do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: false}
  @foreign_key_type :binary_id

  schema "event_queue" do
    field :type, :string
    field :payload, :map
    field :dedup_id, :string
    field :status, :string, default: "pending"
    field :inserted_at, :utc_datetime_usec

    belongs_to :organization, ZapierTriggers.Organizations.Organization
  end

  @doc false
  def changeset(queue_item, attrs) do
    queue_item
    |> cast(attrs, [:id, :type, :payload, :dedup_id, :organization_id, :status, :inserted_at])
    |> validate_required([:id, :type, :payload, :organization_id, :inserted_at])
  end

  @doc """
  Creates a queue item for fast event ingestion.
  Event ID is generated upfront so we can return it immediately.
  """
  def create_queue_item(organization_id, type, payload, dedup_id) do
    event_id = Ecto.UUID.generate()

    %__MODULE__{}
    |> changeset(%{
      id: event_id,
      organization_id: organization_id,
      type: type,
      payload: payload,
      dedup_id: dedup_id,
      status: "pending",
      inserted_at: DateTime.utc_now()
    })
    |> ZapierTriggers.Repo.insert()
  end
end
