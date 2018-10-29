use crate::schema::kana;

#[derive(Identifiable, Serialize, Queryable)]
#[table_name = "kana"]
pub struct Kana {
    pub id: i32,
    pub hiragana: String, //ideally a char
    pub katakana: String, // ideally a char
    pub romaji: String,
}
