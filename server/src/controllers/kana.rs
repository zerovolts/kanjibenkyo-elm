use crate::dbconn::DbConn;
use crate::models::kana::Kana;
use rocket_contrib::Json;

#[get("/")]
pub fn index(conn: DbConn) -> Json<Vec<Kana>> {
    Json(Kana::all(conn))
}
