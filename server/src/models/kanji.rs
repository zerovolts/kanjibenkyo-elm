use crate::schema::kanji;

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
