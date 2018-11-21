CREATE TABLE kana (
    id serial primary key,
    hiragana varchar(1) not null,
    katakana varchar(1) not null,
    romaji varchar not null 
);