use crate::models::kana::Kana;
use rocket_contrib::Json;

#[get("/")]
pub fn index() -> Json<Vec<Kana>> {
    Json(vec![Kana {
        id: 0,
        hiragana: String::from("つ"),
        katakana: String::from("ツ"),
        romaji: String::from("tsu"),
    }])
}
