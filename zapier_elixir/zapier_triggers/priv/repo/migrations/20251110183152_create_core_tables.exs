defmodule ZapierTriggers.Repo.Migrations.CreateCoreTables do
  use Ecto.Migration

  def change do
    # Create organizations table
    create table(:organizations, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :name, :string, null: false
      add :api_key_hash, :string, null: false
      add :webhook_url, :string
      add :rate_limit_per_minute, :integer, null: false, default: 100
      add :tier, :string, null: false, default: "free"

      timestamps(type: :utc_datetime)
    end

    # Unique index on api_key_hash for fast lookups during authentication
    create unique_index(:organizations, [:api_key_hash])

    # Create events table
    create table(:events, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :type, :string, null: false
      add :dedup_id, :string
      add :payload, :map, null: false
      add :organization_id, references(:organizations, type: :binary_id, on_delete: :delete_all), null: false

      timestamps(type: :utc_datetime)
    end

    # Composite index on organization_id and inserted_at for efficient queries
    create index(:events, [:organization_id, :inserted_at])

    # Unique composite index on (organization_id, dedup_id) for per-organization deduplication
    # This ensures dedup_id is unique within each organization, not globally
    create unique_index(:events, [:organization_id, :dedup_id], where: "dedup_id IS NOT NULL")

    # GIN index on payload for JSONB queries
    create index(:events, [:payload], using: :gin)

    # Create event_deliveries table
    create table(:event_deliveries, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :status, :string, null: false, default: "pending"
      add :attempts, :integer, null: false, default: 0
      add :response_status, :integer
      add :last_error, :text
      add :event_id, references(:events, type: :binary_id, on_delete: :delete_all), null: false

      timestamps(type: :utc_datetime)
    end

    # Index on event_id for foreign key lookups
    create index(:event_deliveries, [:event_id])

    # Composite index on status and inserted_at for inbox queries
    create index(:event_deliveries, [:status, :inserted_at])
  end
end
