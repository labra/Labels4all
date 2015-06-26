# Database schema

# ¡¡¡ CREATE FUNCTION pseudo_encrypt (located in pseudo_encrypt.sql) 
# BEFORE APPLYING THIS EVOLUTION ¡¡¡

# --- !Ups

CREATE TABLE Jobs (
	ticket BIGSERIAL PRIMARY KEY,
	redeemed BOOLEAN NOT NULL,
    result json
);

ALTER TABLE Jobs ALTER COLUMN ticket SET DEFAULT pseudo_encrypt(nextval('Jobs_ticket_seq')::int);

# --- !Downs

DROP TABLE Jobs;
