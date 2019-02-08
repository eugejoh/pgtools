#' Write \code{data.frames} to Postgres
#'
#' This function takes a \code{data.frame} or a \code{list} of \code{data.frames} and writes them to a
#' specified \code{schema} using your database connection \code{conn}.
#' Field types should be specified, it not
#' the default values from \code{\link{dbDataType}()} will be used.
#'
#' This function is essentially a wrapper for \code{\link{dbWriteTable}()}. See details in \code{RPostgres}
#' documentation under \code{postgres-tables}.
#'
#' If \code{input} is a \code{list} of data frames, dimensions of \code{field.types}, \code{tbl.comments}, and
#' \code{field.comments} must match \code{input}.
#'
#' @param input a \code{data.frame} or \code{list} of data frames.
#' @param field.types a named \code{character} vector or a named \code{list} of named \code{character} vectors.
#' @param conn a object inheriting from \code{DBIDriver} or \code{DBIConnection}.
#' @param schema an optional argument to specify the desired database schema location, default is \code{public}
#' @param tbl_name a required option if \code{nchar_df} argument is a single \code{data.frame}
#' @param tbl.comments an optional argument to include a comment for the table being written, if a \code{list} it must be named
#' @param field.comments an optional argument to include comments for each field type within the table being written , if a \code{list} it must be named
#' @param ... other arguments passed to \code{\link{dbWriteTable}()}.
#'
#' @return \code{write_pgtable()} returns \code{TRUE} invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' nchar_df <- get_nchar(iris)
#'
#' my_fields <- set_pgfields(nchar_df,
#' default = FALSE,
#' conn = DBI::dbConnect(RSQL::SQLite(), ":memory:"))
#'
#' write_pgtable(input = iris,
#' field.types = my_fields,
#' conn =DBI::dbConnect(RSQL::SQLite(), ":memory:"),
#' tbl_name = "iris")
#' }
#'
write_pgtable <- function(
  input,
  field.types = NULL,
  conn = NULL,
  schema = "public",
  tbl_name = NULL,
  tbl.comments = NULL,
  field.comments = NULL,
  ...
  ) {

  if (inherits(input, "list")) {
    purrr::map(names(input), function(tab) {
      DBI::dbWriteTable(
        conn = conn,
        name = DBI::Id(schema = schema, table = tab),
        value = input[[tab]],
        field.types = field.types[[tab]],
        overwrite = TRUE,
        ...)
      message(paste0("WRITE TABLE for ", tab, " completed"))
    })

    if (exists(tbl.comments)) {
      # add dimension checks
      tbl_cl <- purrr::map(names(input), function(tab) {
        DBI::sqlInterpolate(conn = conn,
                            sql = "COMMENT ON TABLE ?schema.?table IS ?comment",
                            schema = DBI::dbQuoteIdentifier(conn = conn,
                                                            x = DBI::SQL(schema)),
                            table = DBI::dbQuoteIdentifier(conn = conn,
                                                           x = DBI::SQL(tab)),
                            comment = tbl.comments[[tab]])
        })

      purrr::map(unlist(tbl_cl), function(comment) {
        qry <- DBI::dbSendQuery(
          conn = conn,
          statement = comment)
        qry
        DBI::dbClearResult(qry)})
    }

    if (exists(field.comments)) {
      # add dimenion checks
      field_cl <- purrr::map(names(input), function(tab) {
      purrr::map_chr(names(field.comments[[tab]]), function(fields) {
        DBI::sqlInterpolate(conn = conn,
                            sql = "COMMENT ON COLUMN ?schema.?table.?fields IS ?comment",
                            schema = DBI::dbQuoteIdentifier(conn = conn,
                                                            x = DBI::SQL(schema)),
                            table = DBI::dbQuoteIdentifier(conn = conn,
                                                           x = DBI::SQL(tab)),
                            fields = DBI::dbQuoteIdentifier(conn = conn,
                                                            x = DBI::SQL(fields)),
                            comment = field.comments[[tab]][[fields]])
      })
    })

    purrr::map(unlist(field_cl), function(comment) {
      qry <- DBI::dbSendQuery(
        conn = conn,
        statement = comment)
      qry
      DBI::dbClearResult(qry)})
    }


  }

  if (inherits(input, "data.frame")) {
    if (missing(tbl_name)) tbl_name <- deparse(substitute(input))
    DBI::dbWriteTable(
      conn = conn,
      name = DBI::Id(schema = schema, table = tbl_name),
      value = input,
      field.types = field.types,
      overwrite = TRUE,
      ...)
    message(paste0("WRITE TABLE for ", tbl_name, " completed"))

    if (exists(tbl.comments)) {
      if (!is.character(tbl.comments)) stop("tbl.comments must be character")

      tbl_cl <- DBI::sqlInterpolate(conn = conn,
                          sql = "COMMENT ON TABLE ?schema.?table IS ?comment",
                          schema = DBI::dbQuoteIdentifier(conn = conn,
                                                          x = DBI::SQL(schema)),
                          table = DBI::dbQuoteIdentifier(conn = conn,
                                                         x = DBI::SQL(tbl_name)),
                          comment = DBI::dbQuoteIdentifier(conn = conn,
                                                           tbl.comments))

      qry <- DBI::dbSendQuery(
          conn = conn,
          statement = tbl_cl)
      qry
      DBI::dbClearResult(qry)



    }

    if (exists(field.comments)) {
      # add dimension checks, must be same as ncol(input)

      field_cl <- purrr::map_char(names(field.comments), function(fields) {
        DBI::sqlInterpolate(conn = conn,
                            sql = "COMMENT ON COLUMN ?schema.?table.?fields IS ?comment",
                            schema = DBI::dbQuoteIdentifier(conn = conn,
                                                            x = DBI::SQL(schema)),
                            table = DBI::dbQuoteIdentifier(conn = conn,
                                                           x = DBI::SQL(tbl_name)),
                            fields = DBI::dbQuoteIdentifier(conn = conn,
                                                            x = DBI::SQL(fields)),
                            comment = field.comments[[fields]])
      })

      purrr::map(unlist(field_cl), function(comment) {
        qry <- DBI::dbSendQuery(
          conn = conn,
          statement = comment)
        qry
        DBI::dbClearResult(qry)})
      }

    }

}