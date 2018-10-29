use crate::schema::kana;
use rocket_contrib::Json;

#[derive(Identifiable, Serialize, Queryable)]
#[table_name = "kana"]
pub struct Kana {
    id: i32,
    pub hiragana: String,
    pub katakana: String,
    pub romaji: String,
}

#[get("/")]
pub fn index() -> Json<Vec<Kana>> {
    Json(vec![Kana {
        id: 0,
        hiragana: String::from("つ"),
        katakana: String::from("ツ"),
        romaji: String::from("tsu"),
    }])
}
