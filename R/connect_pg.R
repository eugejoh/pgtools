#' Connect to Postgres Database
#'
#' @param local
#' @param getenv
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' connect_pg(local = FALSE, getenv = FALSE,
#'   host = "34.15.631",
#'   port = 5432,
#'   dbname = "mydb",
#'   user = "myusername",
#'   password = "mypw"
#' )

connect_pg <- function(
  local = FALSE, #connect to local postgres if TRUE
  getenv = FALSE, #connect using credentials in .REnviron file
  ...) {

  if (!getenv) {
    RPostgres::dbConnect(drv = DBI::dbDriver("Postgres"), ...)
  }

  if (getenv) {
    RPostgres::dbConnect(
      drv = DBI::dbDriver("Postgres"),
      host = Sys.getenv("db_ip"),
      port = 5432, dbname = Sys.getenv("db_name"),
      user = Sys.getenv("db_user"),
      password = Sys.getenv("db_pw"))
  }

}