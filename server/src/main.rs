#![feature(plugin)]
#![plugin(rocket_codegen)]
// used to silence diesel macro warnings
#![allow(proc_macro_derive_resolution_fallback)]

extern crate dotenv;
extern crate rocket;
extern crate rocket_cors;

// #[macro_use]
extern crate rocket_contrib;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate diesel;

mod models;
mod schema;

use crate::schema::kana::table;
use diesel::pg::PgConnection;
use diesel::prelude::*;
use dotenv::dotenv;
use rocket_cors::AllowedOrigins;
use std::env;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

fn main() {
    let all_origins = AllowedOrigins::all();
    let options = rocket_cors::Cors {
        allowed_origins: all_origins,
        ..Default::default()
    };

    let connection = establish_connection();
    let results = table
        .limit(5)
        .load::<models::kana::Kana>(&connection)
        .expect("Error loading kana");

    for result in results {
        println!("{:?}", result.hiragana);
    }

    rocket::ignite()
        .mount("/", routes![index])
        .mount("/kana", routes![models::kana::index])
        .mount("/kanji", routes![models::kanji::index])
        .attach(options)
        .launch();
}

pub fn establish_connection() -> PgConnection {
    dotenv().ok();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    PgConnection::establish(&database_url).expect(&format!("Error connecting to {}", database_url))
}
