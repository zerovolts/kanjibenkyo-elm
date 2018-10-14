#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;
extern crate rocket_cors;
#[macro_use] extern crate rocket_contrib;
#[macro_use] extern crate serde_derive;

use rocket_contrib::Json;
use rocket_cors::{AllowedOrigins};

#[derive(Serialize)]
pub struct Kanji {
    character: char,
    onyomi: Vec<String>,
    kunyomi: Vec<String>,
    meanings: Vec<String>,
    jlpt: u8,
    radical: char,
    components: Vec<char>
}

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[get("/")]
fn kanji_index() -> Json<Vec<Kanji>> {
    Json(vec![Kanji {
        character: '見',
        onyomi: vec![String::from("ケン")],
        kunyomi: vec![String::from("み")],
        meanings: vec![String::from("see")],
        jlpt: 5,
        radical: '見',
        components: vec![]
    }])
}

fn main() {
    let all_origins = AllowedOrigins::all();
    let options = rocket_cors::Cors {
        allowed_origins: all_origins,
        ..Default::default()
    };

    rocket::ignite()
        .mount("/", routes![index])
        .mount("/kanji", routes![kanji_index])
        .attach(options)
        .launch();
}