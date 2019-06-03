#' create_pgfield
#'
#' @param conn a object inheriting from \code{DBIDriver} or \code{DBIconection}.
#' @param table_name a \code{character} string of target table name
#' @param field_name a \code{character} string of field in specified \code{table}
#' @param fieldtype a \code{character} string of a Postgres field data type, see \code{?dbDataType}
#' @param schema a \code{character} string of the schema name, default is \code{public}
#'
#' @return
#' @export
#'
#' @examples
create_pgfield <- function(conn, table_name, field_name, fieldtype, schema) {
  if (missing(conn)) stop("required connection")
  if (missing(schema)) schema <- "public"
  if (missing(table_name)) stop("require table name")
  if (missing(field_name)) stop("specify column name")

  sql_state <- DBI::sqlInterpolate(conn = conn,
                                   sql = "ALTER TABLE ?schema.?table ADD COLUMN ?field ?fieldtype",
                                   schema = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(schema)),
                                   table = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(table_name)),
                                   field = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(field_name)),
                                   fieldtype = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(fieldtype)))
  qry <- dbSendQuery(conn = conn, statement = sql_state)
  qry
  DBI::dbClearResult(qry)
}

#' update_pgfield
#'
#' @param con a object inheriting from \code{DBIDriver} or \code{DBIconection}.
#' @param table_name a \code{character} of target table name
#' @param value a \code{vector} to insert into the specified \code{table}
#' @param field_name a \code{character} string of field in specified \code{table}
#' @param key a \code{character} string used for the \code{WHERE} SQL clause, recommended to use primary key of table
#' @param schema a \code{character} of the schema name, default is \code{public}
#'
#' @return
#' @export
#'
#' @examples
update_pgfield <- function(conn, table_name, value, field_name, key, schema, verbose = FALSE) {
  if (missing(conn)) stop("required connection")
  if (missing(schema)) schema <- "public"
  if (missing(table_name)) stop("require table name")
  if (missing(field_name)) stop("specify column name")

  for(i in seq_along(value)) {
    sql_statement <- DBI::sqlInterpolate(
      conn = conn,
      sql = paste0("UPDATE ?schema.?table SET ?field = $1 WHERE ?id=",i),
      schema = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(schema)),
      table = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(table_name)),
      field = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(field_name)),
      id = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(key))
    )
    DBI::dbExecute(conn = conn, statement = sql_statement,
                   params = list(value[i]))
    if (verbose)
      message(paste0("[i] = ", i, "/", length(value), " ", round(i/length(value),1)*100, "% complete"))
    }
  }


#' drop_pgfield
#'
#' @param con a object inheriting from \code{DBIDriver} or \code{DBIconection}.
#' @param table_name a \code{character} of target table name
#' @param field_name a \code{character} string of field in specified \code{table}
#' @param schema a \code{character} of the schema name, default is \code{public}
#'
#' @return
#' @export
#'
#' @examples
drop_pgfield <- function(conn, table_name, field_name, schema) {
  if (missing(conn)) stop("required connection")
  if (missing(schema)) schema <- "public"
  if (missing(table_name)) stop("require table name")
  if (missing(field_name)) stop("specify column name")

  sql_state <- DBI::sqlInterpolate(conn = conn,
                                   sql = "ALTER TABLE ?schema.?table DROP COLUMN ?field",
                                   schema = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(schema)),
                                   table = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(table_name)),
                                   field = DBI::dbQuoteIdentifier(conn = conn, DBI::SQL(field_name)))
  qry <- dbSendQuery(conn = conn, statement = sql_state)
  qry
  DBI::dbClearResult(qry)
}