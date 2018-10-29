use crate::schema::kanji;

#[derive(Identifiable, Serialize, Queryable)]
#[table_name = "kanji"]
pub struct Kanji {
    pub id: i32,
    pub character: char,
    pub onyomi: Vec<String>,
    pub kunyomi: Vec<String>,
    pub meanings: Vec<String>,
    pub jlpt: u8,
    pub radical: char,
    pub components: Vec<char>,
}
