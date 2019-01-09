#' Set or Assign Postgres Field Types
#'
#' This function applies a Postgres field type based on either 1. The default output from \code{\link{DBI::dbDataType}()}
#' or 2. A preset condition that maps \code{R} classes to a specific Postgres field types.
#'
#' Factor to character varying (n)
#' integer to smallint
#' numeric to real
#' character to character varying (n)
#'
#' Care should be taken for fields that should be free text \code{text} or do not conform to the standard
#' applied by this function when \code{default = TRUE}. Manual adjustment may be required.
#'
#' @param input a \code{data.frame} or \code{list} of data frames
#' @param nchar_df the output from \code{\link{get_nchar}()}. Running \code{\link{get_nchar}()}
#' may take a long time and should be saved as an object for use in this function
#' @param default a \code{logical} option, default method uses \code{\link{DBI::dbDataType}()}
#' @param conn a object inheriting from \code{DBIDriver} or \code{DBIConnection}
#'
#' @return returns a named \code{character} vector or \code{list} of named \code{character} vectors that will be used
#' to specify Postgres table field types when writing to the database.
#'
#' @importFrom purrr map map2
#' @importFrom DBI dbDataType
#' @importFrom dplyr left_join mutate if_else
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#'
#' nchar_df <- get_nchar(iris)
#'
#' set_pgfields(nchar_df, conn = local_con_test)
#'
set_pgfields <- function(
  input, #dataframe OR list of dataframes of tables
  nchar_df, #dataframe OR list of dataframes from `get_nchar`
  default = FALSE, #use of DBI::dbDataType when TRUE or custom assigned when FALSE
  conn = NULL #database connection
) {

  if (missing(input)) stop("requires input to be provided")
  if (missing(nchar_df)) stop("requires input to be provided, see `get_nchar`")

  # helper functions
  .non_default_pgtypes <- function(dat) {

    .add_dtype <- function(type = NULL) { #converts `class` column to Postgres data type
      ou <- dplyr::if_else(type == "factor", "character varying",
                           dplyr::if_else(type == "integer", "smallint",
                                          dplyr::if_else(type == "numeric", "real",
                                                         dplyr::if_else(type == "character", "character varying",
                                                                        "not assigned"))))
      return(ou)
    }
    .add_ndtype <- function( #option of adding (n) to `character` and `character varying` Postgres data types
      col = NULL,
      type = c("character", "character varying"),
      num = NULL) {

      .add_n <- function(type, n) paste0(type, "(", n, ")") # helper paste function

      if (type == "character") ou <- dplyr::if_else(col == "character", .add_n(col, num), col)

      if (type == "character varying") ou <- dplyr::if_else(col == "character varying", .add_n(col, num), col)

      return(ou)
    }
    out <- dat %>%
      dplyr::mutate(pg_type = .add_dtype(type = class)) %>%
      dplyr::mutate(pg_type = .add_ndtype(col = pg_type, num = nchar_max, type = "character")) %>%
      dplyr::mutate(pg_type = .add_ndtype(col = pg_type, num = nchar_max, type = "character varying")) %>%
      dplyr::mutate(pg_type = ifelse(rowname == "serial_id", "integer", pg_type))
    return(out)
  }


  if (inherits(input, "list")) {

    if (default) {
      if (missing(conn)) stop("requires conn to be provided")
      nchar_df3 <- purrr::map2(nchar_df,
                               DBI::dbDataType(conn, input),
                               function(tab, default_type) {dplyr::mutate(dat, pg_type = default_type)})
    } else {
      nchar_df3 <- purrr::map(nchar_df, .non_default_pgtypes)
    }
  }

  if (inherits(input, "data.frame")) {

    if (default) {
      if (missing(conn)) stop("requires conn to be provided")
      nchar_df3 <- dplyr::left_join(
        nchar_df,
        tibble::rownames_to_column(data.frame(pg_type = DBI::dbDataType(conn, input))),
        by = "rowname")
    } else {
      nchar_df3 <- .non_default_pgtypes(nchar_df)
    }

  }

  return(nchar_df3)

}