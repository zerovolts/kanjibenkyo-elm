use crate::dbconn::DbConn;
use crate::models::user::User;
use rocket_contrib::Json;

#[get("/<email>")]
pub fn show(conn: DbConn, email: String) -> Json<Option<User>> {
    Json(User::find(conn, email))
}
