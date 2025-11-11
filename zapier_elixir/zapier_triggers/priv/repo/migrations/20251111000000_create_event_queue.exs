defmodule ZapierTriggers.Repo.Migrations.CreateEventQueue do
  use Ecto.Migration

  def change do
    # Create event_queue table for async event processing
    # Events are inserted here first for instant response (<10ms),
    # then processed asynchronously by Broadway pipeline
    create table(:event_queue, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :organization_id, references(:organizations, type: :binary_id, on_delete: :delete_all), null: false
      add :type, :string, null: false
      add :payload, :map, null: false
      add :dedup_id, :string
      add :status, :string, default: "pending", null: false
      add :inserted_at, :utc_datetime_usec, null: false
    end

    # Index for efficient queue processing (FIFO order)
    create index(:event_queue, [:status, :inserted_at])

    # Index for organization queries
    create index(:event_queue, [:organization_id])
  end
end
