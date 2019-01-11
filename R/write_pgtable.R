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
#' @param input a \code{data.frame} or \code{list} of data frames.
#' @param field.types a named \code{character} vector or a named \code{list} of named \code{character} vectors.
#' @param conn a object inheriting from \code{DBIDriver} or \code{DBIConnection}.
#' @param schema an optional argument to specify the desired database schema location, default is \code{public}
#' @param tbl_name a require option if \code{nchar_df} argument is a \code{data.frame}
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


  }

  if (inherits(input, "data.frame")) {
    if (missing(tbl_name)) stop("requires tbl_name to be provided")
    DBI::dbWriteTable(
      conn = conn,
      name = DBI::Id(schema = schema, table = tbl_name),
      value = input,
      field.types = field.types,
      overwrite = TRUE,
      ...)
    message(paste0("WRITE TABLE for ", tbl_name, " completed"))

  }
}