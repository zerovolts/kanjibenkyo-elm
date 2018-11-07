CREATE TABLE users (
    id serial primary key,
    name varchar not null,
    email varchar not null,
    password_digest varchar not null,
    experience integer not null
);

CREATE INDEX email_index ON users (email);