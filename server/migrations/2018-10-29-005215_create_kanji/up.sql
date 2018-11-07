CREATE TABLE kanji (
    id serial primary key,
    character varchar(1) not null,
    strokes integer not null,
    onyomi text[] not null,
    kunyomi text[] not null,
    meanings text[] not null,
    grade integer not null,
    radical varchar not null,
    components text[] not null
);