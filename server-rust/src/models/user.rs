use crate::dbconn::DbConn;
use crate::schema::users;
use diesel::prelude::*;

#[derive(Identifiable, Serialize, Queryable)]
pub struct User {
    pub id: i32,
    pub name: String,
    pub email: String,
    pub password_digest: String,
    pub experience: i32,
}

impl User {
    pub fn mock() -> User {
        User {
            id: 0,
            name: String::from("guest"),
            email: String::from("guest@kanjibenkyo.com"),
            password_digest: String::from(""),
            experience: 0,
        }
    }

    pub fn find(conn: DbConn, email: String) -> Option<User> {
        users::table
            .filter(users::email.eq(&email))
            .first(&*conn)
            .optional()
            .expect("Error loading user")
    }
}
