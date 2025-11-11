defmodule ZapierTriggers.Events.EventDelivery do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "event_deliveries" do
    field :status, :string, default: "pending"
    field :attempts, :integer, default: 0
    field :response_status, :integer
    field :last_error, :string

    belongs_to :event, ZapierTriggers.Events.Event

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(delivery, attrs) do
    delivery
    |> cast(attrs, [:status, :attempts, :response_status, :last_error, :event_id])
    |> validate_required([:status, :event_id])
    |> validate_inclusion(:status, ["pending", "delivered", "failed", "dead_letter"])
  end
end
