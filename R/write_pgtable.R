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
#' @param verbose a \code{logical} argument whether to display messages on database writing steps
#' @param ... other arguments passed to \code{\link{dbWriteTable}()}.
#'
#' The \code{clean_vars} argument does not clean table names for list objects. The names of the dataframes inside the input list must be changed manually.
#'
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
  conn,
  input,
  field.types = NULL,
  schema = "public",
  tbl_name = NULL,
  tbl.comments = NULL,
  field.comments = NULL,
  clean_vars = FALSE,
  verbose = TRUE,
  ...
  ) {

  # helper functions
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

  if (missing(conn)) stop("must specify db connection")

  #check if schema exists
  if (nrow(DBI::dbGetQuery(conn,
    DBI::sqlInterpolate(conn,
    "SELECT nspname
    FROM pg_catalog.pg_namespace
    WHERE nspname = ?schema_name",
    schema_name = schema))) < 1) stop("schema does not exist")

  # clean variable/field names if TRUE
  if (clean_vars) {
    # for list
    if (inherits(input, "list")) {
      input <- .all_to_lower(input) #lower case recursively

      if (!is.null(field.comments)) {
        if (is.null(unlist(lapply(field.comments, names))))
          stop("field comments names don't exist")

        field.comments <- lapply(field.comments, function(x) {
          names(x) <- gsub("\\.", "_", tolower(names(x)))
          return(x)
        })

        names(field.comments) <- gsub("\\.", "_", tolower(names(field.comments)))

      }

    }

    # for data frame
    if (inherits(input, "data.frame")) {
      names(input) <- gsub("\\.", "_", tolower(names(input)))
      if (!is.null(field.comments)) {
        names(field.comments) <- gsub("\\.", "_", tolower(names(field.comments)))
      }

      if (!is.null(field.types)) names(field.types) <- names(input) #need this if names change, bc field.types in dbWriteTable has is.na() >1

      # if field comments exist, but no names -> STOP
      if (!is.null(field.comments)) {
        if (is.null(names(field.comments))) {
          stop("field.comments requires names 1")
        } else {
          # if field comments exist, with names -> clean like field names
          names(field.comments) <- gsub("\\.", "_", tolower(names(field.comments)))

          if (!all(names(field.comments) %in% names(input)))
            stop("field.comment names don't match input names")
          }
        }
    }

    if (verbose) message("clean table names/variables")
  }


  if (inherits(input, "list")) {

    if (is.null(names(input))) stop("required names for list input")

    if (any(grepl("[[:upper:]]+|\\.+", names(input))))
      stop("table names are either uppercase or has a period")

    if (any(grepl("[[:upper:]]+", unlist(lapply(input, colnames)))))
      stop("coerce field names to lower case")

    if (any(grepl("\\.", unlist(lapply(input, colnames)))))
      stop("remove periods in field names")

    if (!is.null(tbl_name)) {
      if (!all(names(input) == tbl_name))
        stop("table names conflict, leave `tbl_name` as NULL")
    }

    if (!is.null(tbl.comments)) {
      if (length(tbl.comments) != length(names(input)))
        stop("table comments length not equal to tables")

      if (is.null(names(tbl.comments)))
        stop("table comment names required")

      if (!all(names(tbl.comments) == names(input)))
        stop("table comments names differ from table names")

      write_tbl_comments <- TRUE
    }

    if (!is.null(field.comments)) {
      if (is.null(unlist(lapply(field.comments, names)))) {
            stop("field comments require names")
          }

      if (!all(unlist(lapply(field.comments, length)) == unlist(lapply(input, ncol))))
        warning("field comments length differ from existing fields")
    }

    nombres <- names(input)

    purrr::map(nombres, function(tab) {
      DBI::dbWriteTable(
        conn = conn,
        name = DBI::Id(schema = schema, table = tab),
        value = input[[tab]],
        field.types = field.types[[tab]],
        ...)
      if (verbose) message(paste0("WRITE TABLE for ", tab, " completed"))
    })

    if (!is.null(tbl.comments)) {
      tbl_cl <- purrr::map(nombres, function(tab) {
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
      if (verbose) message(paste0("COMMENT ON TABLE completed"))
    }

    if (!is.null(field.comments)) {

      field_cl <- purrr::map(nombres, function(tab) {
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
    if (verbose) message(paste0("COMMENT ON COLUMN completed"))

    }
  }


  if (inherits(input, "data.frame")) {

    if (is.null(tbl_name)) assign("tbl_name", deparse(substitute(input)))
    if (grepl("[[:upper:]]+|\\.+", tbl_name)) stop("tbl_name contains either an uppercase or period")

    if (any(grepl("[[:upper:]]+", names(input)))) {
      stop("coerce field names to lower case")
    }
    if (any(grepl("\\.+", names(input)))) {
      stop("remove periods in field names")
    }

    if ((grepl("[[:upper:]]+", tbl_name))) stop("table name contains a period")

    if ((grepl("\\.+", tbl_name))) stop("table name contains a period")

    DBI::dbWriteTable(
      conn = conn,
      name = DBI::Id(schema = schema, table = as.character(tbl_name)),
      value = input,
      field.types = field.types,
      ...)

    if (verbose) message(paste0("WRITE TABLE for ", tbl_name, " completed"))

    if (!is.null(tbl.comments)) {
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
      if (verbose) message(paste0("COMMENT ON TABLE completed"))
    }


    if (!is.null(field.comments)) {

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
      if (verbose) message(paste0("COMMENT ON COLUMN completed"))
      }

    }

}