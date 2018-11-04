table! {
    kana (id) {
        id -> Int4,
        hiragana -> Varchar,
        katakana -> Varchar,
        romaji -> Varchar,
    }
}

table! {
    kanji (id) {
        id -> Int4,
        character -> Varchar,
        strokes -> Int4,
        onyomi -> Array<Text>,
        kunyomi -> Array<Text>,
        meanings -> Array<Text>,
        grade -> Int4,
        radical -> Varchar,
        components -> Array<Text>,
    }
}

allow_tables_to_appear_in_same_query!(
    kana,
    kanji,
);
