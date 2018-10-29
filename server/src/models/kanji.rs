use crate::dbconn::DbConn;
use crate::schema::kanji;
use diesel::prelude::*;

#[derive(Identifiable, Serialize, Queryable)]
#[table_name = "kanji"]
pub struct Kanji {
    pub id: i32,
    pub character: String, // ideally a char
    pub onyomi: Vec<String>,
    pub kunyomi: Vec<String>,
    pub meanings: Vec<String>,
    pub jlpt: i32,
    pub radical: String, // ideally a char
    pub components: Vec<String>,
}

impl Kanji {
    pub fn mock() -> Kanji {
        Kanji {
            id: 0,
            character: String::from("見"),
            onyomi: vec![String::from("ケン")],
            kunyomi: vec![String::from("み")],
            meanings: vec![String::from("see")],
            jlpt: 5,
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
