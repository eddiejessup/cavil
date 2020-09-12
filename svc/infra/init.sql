CREATE USER cavil WITH PASSWORD 'password123';

CREATE DATABASE cavil;

\connect cavil

CREATE TYPE event_type AS ENUM ('decider', 'ledger');

CREATE TABLE event (
    sequence_nr bigserial,
    created timestamp with time zone not null default current_timestamp,
    event_type event_type not null,
    aggregate_id uuid not null,
    data jsonb not null
)
;

GRANT ALL PRIVILEGES ON TYPE event_type TO cavil;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO cavil;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO cavil;
