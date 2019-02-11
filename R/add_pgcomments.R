#' Write Postgres Comments for a Table
#'
#' This function allows a user connected to a Postgres database to easily add comments for a single table
#' or the table's fields.
#'
#' @param conn a object inheriting from \code{DBIDriver} or \code{DBIConnection}.
#' @param schema an optional argument to specify the desired database schema location, default is \code{public}
#' @param tbl_name a required option if \code{nchar_df} argument is a single \code{data.frame}
#' @param tbl.comments an optional argument to include a comment for the table being written, if a \code{list} it must be named
#' @param field.comments an optional argument to include comments for each field type within the table being written , if a \code{list} it must be named
#' @param override a \code{logical} argument whether to override the writing of pre-existing comments
#'
#' @return \code{add_pgcomments()} returns \code{TRUE} invisibly.
#'
#' @importFrom stats "setNames"
#' @export
#'
#' @examples
#' \dontrun{
#' nchar_df <- get_nchar(iris)
#'
#' myconn <- DBI::dbConnect(RSQL::SQLite(), ":memory:")
#'
#' my_fields <- set_pgfields(nchar_df,
#' default = FALSE,
#' conn = myconn)
#'
#' write_pgtable(input = iris,
#' field.types = my_fields,
#' conn = myconn,
#' tbl_name = "iris")
#'
#' add_pgcomments(conn = myconn,
#' tbl_name = "iris",
#' tbl.comments = "this is the iris dataset!",
#' override = TRUE)
#' }
#'
add_pgcomments <- function(
  conn = NULL,
  schema = "public",
  tbl_name = NULL,
  tbl.comments = NULL,
  field.comments = NULL,
  override = FALSE
  ) {

  # check if schema exists in db
  if (nrow(DBI::dbGetQuery(conn,
                       DBI::sqlInterpolate(conn,
                       "SELECT nspname
                         FROM pg_catalog.pg_namespace
                         WHERE nspname = ?schema_name",
                       schema_name = schema))) < 1)
    stop("schema does not exist")

  # check if table exists in db
  if (!DBI::dbExistsTable(conn, DBI::Id(schema = schema, table = tbl_name)))
    stop("table does not exist")

  #helper functions
  # write table comment
  write_tbl_comments <- function(tbl.comments, schema, tbl_name, conn) {
    if (!is.character(tbl.comments)) stop("tbl.comments must be character")

    tbl_cl <- DBI::sqlInterpolate(conn = conn,
                                  sql = "COMMENT ON TABLE ?schema.?table IS ?comment",
                                  schema = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(schema)),
                                  table = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(tbl_name)),
                                  comment = tbl.comments)
    qry <- DBI::dbSendQuery(
      conn = conn,
      statement = tbl_cl)
    qry
    DBI::dbClearResult(qry)
    message(paste0("COMMENT ON TABLE completed"))
  }

  # write field comments
  write_field_comments <- function(field.comments, schema, tbl_name, conn) {

    field_cl <- purrr::map_chr(names(field.comments), function(field) {
      DBI::sqlInterpolate(conn = conn,
                          sql = "COMMENT ON COLUMN ?schema.?table.?fields IS ?comment",
                          schema = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(schema)),
                          table = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(tbl_name)),
                          fields = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(field)),
                          comment = field.comments[[field]])
      })
    qry <- DBI::dbSendQuery(
      conn = conn,
      statement = field_cl)
    qry
    DBI::dbClearResult(qry)
    message(paste0("COMMENT ON TABLE completed"))
  }

  get_field_comments <- DBI::sqlInterpolate( #get number of fields present for the specified table
    conn,
    sql = "SELECT COUNT(*) AS n
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE table_catalog = 'sl_phc'
    AND table_schema = ?schema_name
    AND table_name = ?table;",
    schema_name = schema,
    table = tbl_name)



  # check if comments exist (for both table and fields)
  if (!override) {
    if (!is.na(DBI::sqlInterpolate(conn,
    "SELECT relname, obj_description(oid)
      FROM pg_class
      WHERE relname = ?table;",
    table = tbl_name)$obj_description)) {   # if !override and comments exists = stop
      stop("comments exists and override = FALSE")

    } else { #if override = FALSE and comments DON't exist

      #write comments
      if (!missing(tbl.comments)) {

        write_tbl_comments(tbl.comments, schema, tbl_name, conn)

      }

      if (!missing(field.comments)) {
        #check if number of field comments is equal to pre-existing table columns
        if (length(field.comments != dplyr::pull(DBI::dbGetQuery(conn, get_field_comments))))
          stop("number of 'field.comments' and fields do not match")

        write_field_comments(tbl.comments, schema, tbl_name, conn)
      }


    }
  } else if (override) {   # if override TRUE, just write comments
    #write comments, THIS IS REPEATED ABOVE, make function?

    if (!missing(tbl.comments)) {

      write_tbl_comments(tbl.comments, schema, tbl_name, conn)

    }

    if (!missing(field.comments)) {

      if (length(field.comments != dplyr::pull(DBI::dbGetQuery(conn, get_field_comments))))
        stop("number of 'field.comments' and fields do not match")

      write_field_comments(tbl.comments, schema, tbl_name, conn)
    }


  }

}