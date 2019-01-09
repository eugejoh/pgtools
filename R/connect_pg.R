#' Title
#'
#' @param local
#' @param getenv
#'
#' @return
#' @export
#'
#' @examples
connect_pg <- function(
  local = FALSE, #connect to local postgres if TRUE
  getenv = TRUE #connect using credentials in .REnviron file
  ) {

  if (getenv) {
    RPostgres::dbConnect(
      drv,
      host = Sys.getenv("db_ip"),
      port = 5432, dbname = Sys.getenv("db_name"),
      user = Sys.getenv("db_user"),
      password = Sys.getenv("db_pw"))
  }

}