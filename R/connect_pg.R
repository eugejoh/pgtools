#' Connect to Postgres Database
#'
#' This is a convenience wrapper to \code{DBI::dbConnect()} where if the database credentials
#' are saved in the .REnviron file, they will be automatically read.
#'
#' All other arguments pass through \code{dbConnect()} except for \code{drv} which is already specified
#' to \code{RPostgres::Postgres()}
#'
#' @param getenv get credentials from the local .REnviron file
#' @param verbose print database connection information from \code{\link{dbGetInfo}()}
#' @param ... other arguments to pass through \code{\link{dbConnect}()}
#'
#' @return connection to Postgres database
#'
#' @export
#'
#' @examples
#'\dontrun{
#' connect_pg(getenv = FALSE,
#'   host = DBI::dbDriver("Postgres"),
#'   port = 5432,
#'   dbname = "mydb",
#'   user = "myusername",
#'   password = "mypw"
#' )
#' }
connect_pg <- function(
  getenv = FALSE,
  verbose = FALSE,
  ...) {

  if (!getenv) {
    con <- DBI::dbConnect(drv = RPostgres::Postgres(), ...)
  }

  if (getenv) {
    con <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      host = Sys.getenv("db_ip"),
      port = 5432,
      dbname = Sys.getenv("db_name"),
      user = Sys.getenv("db_user"),
      password = Sys.getenv("db_pw"))
  }

  if (verbose) print(DBI::dbGetInfo(con))

  return(con)

}