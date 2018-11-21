use crate::dbconn::DbConn;
use crate::schema::kanji;
use diesel::prelude::*;

#[derive(Identifiable, Serialize, Queryable)]
#[table_name = "kanji"]
pub struct Kanji {
    pub id: i32,
    pub character: String, // ideally a char
    pub strokes: i32,
    pub onyomi: Vec<String>,
    pub kunyomi: Vec<String>,
    pub meanings: Vec<String>,
    pub grade: i32,
    pub radical: String, // ideally a char
    pub components: Vec<String>,
}

impl Kanji {
    pub fn mock() -> Kanji {
        Kanji {
            id: 0,
            character: String::from("見"),
            strokes: 7,
            onyomi: vec![String::from("ケン")],
            kunyomi: vec![String::from("み")],
            meanings: vec![String::from("see")],
            grade: 1,
            radical: String::from("見"),
            components: vec![],
        }
    }

    pub fn all(conn: DbConn) -> Vec<Kanji> {
        kanji::table
            .load::<Kanji>(&*conn)
            .expect("Error loading kana")
    }
}
