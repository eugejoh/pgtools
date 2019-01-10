#' Generate SQL to CREATE TABLE
#'
#' This function uses the output from \code{nchar_df()} to generate a SQL \code{CREATE TABLE} statement
#' that can be used to create the skeleton table in Postgres. The result can be copied and pasted for use.
#'
#' NOTE: \code{\link{write_pg}()} does not use the SQL statement to write to PostgreSQL, but solely uses the result
#' from \code{set_pgfields()}
#'
#' @param pg_fields a named \code{character} vector or a named \code{list} of named \code{character} vectors
#' @param schema an optional argument to specify the desired schema for \code{CREATE TABLE}
#' @param pkey a \code{character} string specifying the primary for the Postgres (PRIMARY KEY and CONSTRAINT)
#' @param tbl_name a require option if \code{nchar_df} argument is a \code{data.frame}
#' @param export a \code{logical} option export the result as an binary file
#' @param path a \code{file path} option to specify the write location of the binary file
#' @param ... other arguments passed to \code{\link{glue_sql}()}
#'
#' @return results in a SQL statement to \code{CREATE TABLE}. See \code{DBI::SQL}
#'
#' @importFrom purrr map map_chr
#' @importFrom glue glue_sql
#' @export
#'
#' @examples
#' nchar_df <- get_nchar(iris)
#'
#' my_pgfields <- set_pgfields(nchar_df, conn = local_con_test)
#'
#' get_sql_create(my_pg_fields, pkey = "Species", tbl_name = "iris")
get_sql_create <- function(
  pg_fields, #named character vector with variables/fields and respective data/field types
  schema = "public", #specify name of schema to write to
  pkey = NULL, #character string for primary key, must be the same for all tables if in a list
  tbl_name = NULL, #table name required when nchar_df is a data.frame
  export = FALSE, #export result as data frame to file
  path = NULL, #specify path for export
  ...) { #other arguents passed to glue::glue_sql

  if (missing(pg_fields)) stop("requires input to be provided")

  if (inherits(pg_fields, "list")) {
    if (!any(pkey %in% purrr::map_chr(pg_fields, names))) stop("requires pkey to be provided")
    out <- purrr::map(names(pg_fields),
        function(nombres) {
          glue::glue_sql("CREATE TABLE ", schema, ".", nombres, " (",
                         paste0(names(pg_fields[[nombres]]), " ", pg_fields[[nombres]], ", ", collapse = " "),
                         " CONSTRAINT ", paste0(nombres,"_pkey"), " PRIMARY KEY (", pkey, ")", ");", ...)
        })

    names(out) <- names(pg_fields)

  }

  if (inherits(pg_fields, "character")) {
    if (!any(pkey %in% names(pg_fields))) stop("requires pkey to be provided")

    nombres <- names(pg_fields)

    out <- glue::glue_sql("CREATE TABLE ", schema, ".", tbl_name, " (",
               paste0(names(pg_fields), " ", paste0(pg_fields, ", "), collapse = " "),
               " CONSTRAINT ", paste0(tbl_name, "_pkey"), " PRIMARY KEY (", pkey, ")", ");", ...)
    }
  return(out)
  }

