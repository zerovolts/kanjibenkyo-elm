use crate::dbconn::DbConn;
use crate::schema::kana;
use diesel::prelude::*;

#[derive(Identifiable, Serialize, Queryable)]
#[table_name = "kana"]
pub struct Kana {
    pub id: i32,
    pub hiragana: String, //ideally a char
    pub katakana: String, // ideally a char
    pub romaji: String,
}

impl Kana {
    pub fn example() -> Kana {
        Kana {
            id: 0,
            hiragana: String::from("つ"),
            katakana: String::from("ツ"),
            romaji: String::from("tsu"),
        }
    }

    pub fn all(conn: DbConn) -> Vec<Kana> {
        kana::table
            .load::<Kana>(&*conn)
            .expect("Error loading kana")
    }
}
