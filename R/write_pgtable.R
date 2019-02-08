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
#' @param clean_vars an optional \code{logical} argument that automatically cleans field names to be compatible with Postgres (removes upper-case and periods)
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
#' conn = DBI::dbConnect(RSQL::SQLite(), ":memory:"),
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
  clean_vars = FALSE,
  ...
  ) {

  .all_to_lower <- function(x) {
    if (! is.null(names(x)))
      names(x) <- tolower(names(x))
    if (is.list(x) & ! is.data.frame(x))
      x <- lapply(x, function(z) {
        z <- .all_to_lower(z)
        names(z) <- gsub("\\.", "_", names(z))
        return(z)
      })
    x
  }


  if (clean_vars) {
    if (inherits(input, "list")) {
      input <- .all_to_lower(input)
      tbl.comments <- gsub("\\.", "_", tolower(tbl.comments))
      field.comments <- lapply(field.comments, function(x) {
        setNames(x, gsub("\\.", "_", tolower(names(x))))
      })
    }

    if (inherits(input, "data.frame")) {
      names(input) <- tolower(colnames(input))
      names(input) <- gsub("\\.", "_", colnames(input))
    }


  }


  if (inherits(input, "list")) {
    if (any(grepl("^[[:upper:]]+$", unlist(lapply(input, colnames))))) {
      stop("coerce field names to lower case")
    }
    if (any(grepl("\\.+", unlist(lapply(input, colnames))))) {
      stop("remove periods in field names")
    }

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

    if (!missing(tbl.comments)) {
      # add dimension checks
      tbl_cl <- purrr::map(names(input), function(tab) {
        DBI::sqlInterpolate(conn = conn,
                            sql = "COMMENT ON TABLE ?schema.?table IS ?comment",
                            schema = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(schema)),
                            table = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(tab)),
                            comment = tbl.comments[[tab]])
      })



      purrr::map(tbl_cl, function(comment) {
        qry <- DBI::dbSendQuery(
          conn = conn,
          statement = comment)
        qry
        DBI::dbClearResult(qry)})
      message(paste0("COMMENT ON TABLE completed"))
    }

    if (!missing(field.comments)) {
      # add dimension checks
      field_cl <- purrr::map(names(input), function(tab) {
      purrr::map_chr(names(field.comments[[tab]]), function(field) {
        DBI::sqlInterpolate(conn = conn,
                            sql = "COMMENT ON COLUMN ?schema.?table.?fields IS ?comment",
                            schema = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(schema)),
                            table = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(tab)),
                            fields = DBI::dbQuoteIdentifier(conn = conn, x = DBI::SQL(field)),
                            comment = field.comments[[tab]][[field]])
      })
    })

    purrr::map(unlist(field_cl), function(comment) {
      qry <- DBI::dbSendQuery(
        conn = conn,
        statement = comment)
      qry
      DBI::dbClearResult(qry)})
    message(paste0("COMMENT ON COLUMN completed"))
    }


  }

  if (inherits(input, "data.frame")) {
    if (any(grepl("^[[:upper:]]+$", names(input)))) {
      stop("coerce field names to lower case")
    }
    if (any(grepl("^\\.+$", names(input)))) {
      stop("remove periods in field names")
    }

    if (missing(tbl_name)) tbl_name <- deparse(substitute(input))
    DBI::dbWriteTable(
      conn = conn,
      name = DBI::Id(schema = schema, table = tbl_name),
      value = input,
      field.types = field.types,
      overwrite = TRUE,
      ...)
    message(paste0("WRITE TABLE for ", tbl_name, " completed"))

    if (!missing(tbl.comments)) {
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

    if (!missing(field.comments)) {
      # add dimension checks, must be same as ncol(input)

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
      message(paste0("COMMENT ON COLUMN completed"))
      }

    }

}