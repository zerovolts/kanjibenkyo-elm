CREATE TABLE kanji (
    id serial primary key,
    character varchar(1) not null,
    onyomi text[] not null,
    kunyomi text[] not null,
    meanings text[] not null,
    jlpt integer not null,
    radical varchar not null,
    components text[] not null
)