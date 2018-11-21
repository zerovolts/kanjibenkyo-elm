use crate::dbconn::DbConn;
use crate::models::kanji::Kanji;
use rocket_contrib::Json;

#[get("/")]
pub fn index(conn: DbConn) -> Json<Vec<Kanji>> {
    Json(Kanji::all(conn))
}
