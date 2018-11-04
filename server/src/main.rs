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

mod controllers;
mod dbconn;
mod models;
mod schema;

use crate::schema::{kana, kanji};
use diesel::pg::PgConnection;
use diesel::prelude::*;
use diesel::r2d2::{ConnectionManager, Pool, PooledConnection};
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

    rocket::ignite()
        .manage(init_pool())
        .mount("/", routes![index])
        .mount("/kana", routes![controllers::kana::index])
        .mount("/kanji", routes![controllers::kanji::index])
        .attach(options)
        .launch();
}

fn init_pool() -> dbconn::PgPool {
    dotenv().ok();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    let manager = ConnectionManager::<PgConnection>::new(database_url);
    Pool::new(manager).expect("db pool")
}
