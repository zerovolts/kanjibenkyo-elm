use crate::schema::kanji;
use rocket_contrib::Json;

#[derive(Identifiable, Serialize, Queryable)]
#[table_name = "kanji"]
pub struct Kanji {
    id: i32,
    character: char,
    onyomi: Vec<String>,
    kunyomi: Vec<String>,
    meanings: Vec<String>,
    jlpt: u8,
    radical: char,
    components: Vec<char>,
}

#[get("/")]
pub fn index() -> Json<Vec<Kanji>> {
    Json(vec![Kanji {
        id: 0,
        character: '見',
        onyomi: vec![String::from("ケン")],
        kunyomi: vec![String::from("み")],
        meanings: vec![String::from("see")],
        jlpt: 5,
        radical: '見',
        components: vec![],
    }])
}
