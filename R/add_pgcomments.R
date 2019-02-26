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
#' @param overwrite a \code{logical} argument whether to override the writing of pre-existing comments
#' @param verbose a \code{logical} argument whether to display messages on comment writing steps
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
#' my_fields <- set_pgfields(iris,
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
#' overwrite = TRUE)
#' }
#'
add_pgcomments <- function(
  conn,
  schema = "public",
  tbl_name = NULL,
  tbl.comments = NULL,
  field.comments = NULL,
  verbose = TRUE,
  overwrite = FALSE
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

  # helper functions --------------------------------------------------------
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
    if (verbose) message(paste0("COMMENT ON TABLE '", tbl_name,"' completed"))
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

    purrr::map(unlist(field_cl), function(comment) {
      qry <- DBI::dbSendQuery(
        conn = conn,
        statement = comment)
      qry
      DBI::dbClearResult(qry)})

    if (verbose)  message(paste0("COMMENT ON COLUMN ", paste("'",names(field.comments),"'", collapse = ", " , sep = "")," completed"))
  }


  # WHEN tbl.comments EXISTS do...(when you want to change the table comments)
  if (!is.null(tbl.comments)) {
    # check if comments for table exists
    tblcomment_val <- dbGetQuery(con_local,
                                 DBI::sqlInterpolate(con_local,
                                                     sql = "SELECT obj_description(?schematable::regclass) AS tbl_comment;",
                                                     schematable = paste0(schema, ".", tbl_name)))[["tbl_comment"]]
    if (is.na(tblcomment_val)) { #if comment doesn't exist
      if (verbose) message("comment doesn't exist")
    }
    if (!is.na(tblcomment_val)) { #if comment exists
      if (verbose) message("comment exists")
    }

    # check when overwrite = FALSE and comment exists... STOP
    if (!overwrite & !is.na(tblcomment_val)) stop(paste0("comments exist on ", tbl_name, " table and overwrite = FALSE"))

    if (overwrite | (!overwrite & is.na(tblcomment_val))) {
      # write table comment HERE
      write_tbl_comments(tbl.comments, schema, tbl_name, conn) #helper function
    }

  }

  # WHEN field.comments EXISTS do...(when you want to change the field comments)
  if (!is.null(field.comments)) {

    # does the field comment vector have name? name is used to match to variable
    if (is.null(names(field.comments)))
      stop("field comments requires names")

    # if field comment vector has names, do they match to existing field names in table?
    if (!any(names(field.comments) %in% DBI::dbListFields(conn = conn, tbl_name)))
      stop("field comment names must match at least one field")


    # if field comment vector has MATCHING names, then write field comments
    field_names <- dbListFields(conn = conn, tbl_name)

    write_field_comments(field.comments = field.comments[field_names[field_names %in% names(field.comments)]],
                         schema, #takes field names
                         tbl_name,
                         conn)
  }

}

