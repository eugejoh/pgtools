#' Generate SQL to CREATE TABLE
#'
#' This function uses the output from \code{nchar_df()} to generate a SQL \code{CREATE TABLE} statement
#' that can be used to create the skeleton table in Postgres. The result can be copied and pasted for use.
#'
#' NOTE: \code{\link{write_pg}()} does not use the SQL statement to write to PostgreSQL, but solely uses the result
#' from \code{set_pgfields()}
#'
#' @param nchar_df the \code{data.frame} or \code{list} of \code{data.frames} from \code{set_pgfields()}
#' @param export a \code{logical} option export the result as an binary file
#' @param path a \code{file path} option to specify the write location of the binary file
#' @param pkey a \code{character} string specifying the primary for the Postgres (PRIMARY KEY and CONSTRAINT)
#' @param tbl_name a require option if \code{nchar_df} argument is a \code{data.frame}
#' @param schema an optional argument to specify the desired schema for \code{CREATE TABLE}
#' @param ... other arguments passed to \code{glue_sql()}
#'
#' @return results in a SQL statement to \code{CREATE TABLE}. See \code{DBI::SQL}
#'
#' @importFrom purrr map map2
#' @importFrom glue glue glue_collapse glue_data
#' @importFrom tidyr unite
#'
#' @export
#'
#' @examples
#' nchar_df <- get_nchar(iris)
#'
#' my_pgfields <- set_pgfields(nchar_df, conn = local_con_test)
#'
#' get_sql_create(my_pg_fields, pkey = "Species", tbl_name = "iris")
get_sql_create <- function(
  nchar_df, #`get_nchar()` output
  export = FALSE, #export result as data frame to file
  path = NULL, #specify path for export
  pkey = NULL, #character string for primary key, must be the same for all tables if in a list
  tbl_name = NULL, #table name required when nchar_df is a data.frame
  schema = "public", #specify name of schema to write to
  ...) {

  if (missing(nchar_df)) stop("requires input to be provided")

  if (inherits(nchar_df, "list")) {
    if (sum(unlist(purrr::map(nchar_df, ~grepl("rowname|pg_type", colnames(.))))) != 2*length(nchar_df))
      stop ("missing approprpiate column names")
    if (sum(unlist(purrr::map(nchar_df, ~grepl(pkey, .$rowname)))) != length(nchar_df))
      stop("primary key variable must be in 'rowname' column")

  } else if (inherits(nchar_df, "data.frame")) {
    if (sum(grepl("rowname|pg_type", colnames(nchar_df))) < 1)
      stop("missing appropriate column names")
    if (!any(pkey %in% nchar_df$rowname))
      stop("primary key variable must be in 'rowname' column")
    if (missing(tbl_name))
      stop("name for table must be provided")
  }

  if (inherits(nchar_df, "list")) {
    out <- purrr::map(nchar_df, ~.[,c("rowname", "pg_type")]) %>%
      purrr::map(~tidyr::unite(., col = "sql_dtype", rowname, pg_type, sep = " ")) %>%
      purrr::map(~glue::glue_data(., "{sql_dtype},")) %>%
      purrr::map2(., names(nchar_df),
                  function(vec, nombre) {
                    c(vec, glue::glue("CONSTRAINT ", nombre, "_pkey", " PRIMARY KEY ", "(", pkey, ")"))
                  }) %>%
      purrr::map2(., names(.),
                  function(tab, nombre) {
                    glue::glue_sql("CREATE TABLE ", schema, ".", nombre, " (",
                               glue::glue_collapse(tab, sep = " "), ");", ...)
                  })

  } else if (inherits(nchar_df, "data.frame")) {
    out <- nchar_df[, c("rowname", "pg_type")] %>%
      tidyr::unite(., col = "sql_dtype", rowname, pg_type, sep = " ") %>%
      glue::glue_data(., "{sql_dtype},") %>%
      c(., glue::glue("CONSTRAINT ", tbl_name, "_pkey", " PRIMARY KEY ", "(", pkey, ")")) %>%
      glue::glue_collapse(., sep = " ") %>%
      glue::glue_sql("CREATE TABLE ", schema, ".", tbl_name, " (", ., ");", ...)

  }

  return(out)

}

