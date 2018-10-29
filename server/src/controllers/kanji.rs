use crate::models::kanji::Kanji;
use rocket_contrib::Json;

#[get("/")]
pub fn index() -> Json<Vec<Kanji>> {
    Json(vec![Kanji {
        id: 0,
        character: String::from("見"),
        onyomi: vec![String::from("ケン")],
        kunyomi: vec![String::from("み")],
        meanings: vec![String::from("see")],
        jlpt: 5,
        radical: String::from("見"),
        components: vec![],
    }])
}
