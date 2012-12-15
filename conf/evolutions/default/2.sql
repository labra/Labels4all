# Database schema for languages
 
# --- !Ups


CREATE SEQUENCE lang_id_seq start with 1000;

CREATE TABLE language (
    id 				bigint NOT NULL DEFAULT nextval('lang_id_seq'),
    langCode 		varchar(100) not null,
    langName 		varchar(255) not null,
    constraint 		pk_lang primary key (id)
);


# --- !Downs

DROP TABLE if exists language;

DROP SEQUENCE if exists lang_id_seq;
