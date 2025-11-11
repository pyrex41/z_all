defmodule ZapierTriggers.Repo.Migrations.AddProcessingStartedAtToEventQueue do
  use Ecto.Migration

  def change do
    # Add processing_started_at to track when event processing actually began
    # This allows accurate detection of stuck items (vs using inserted_at which is when queued)
    alter table(:event_queue) do
      add :processing_started_at, :utc_datetime_usec
    end

    # Index for efficient stuck item cleanup
    create index(:event_queue, [:status, :processing_started_at])
  end
end
