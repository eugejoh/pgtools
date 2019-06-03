#' Set or Assign Postgres Field Types
#'
#' This function applies a Postgres field type based on either 1. The default output from \code{DBI::dbDataType()}
#' or 2. A preset condition that maps \code{R} classes to a specific Postgres field types.
#'
#' Factor to character varying (n)
#' integer to smallint
#' numeric to real
#' character to character varying (n)
#'
#' It is suggested to run \code{\link{get_nchar}()} prior to using \code{set_pgfields()} as \code{\link{get_nchar}()} is
#' a computationally expensive task.
#'
#'
#' @param input a \code{data.frame} or \code{list} of data frames
#' @param nchar_df the output from \code{\link{get_nchar}()}. If \code{nchar_df} is \code{NULL}, then \code{set_pgfields()}
#' will call \code{\link{get_nchar}()} to calculate appropriate element lengths.
#' @param default a \code{logical} option, default = \code{TRUE} uses \code{DBI::dbDataType()}
#' @param conn a object inheriting from \code{DBIDriver} or \code{DBIConnection}
#'
#' @return returns a named \code{character} vector or \code{list} of named \code{character} vectors that will be used
#' to specify Postgres table field types when writing to the database.
#'
#' @importFrom dplyr "%>%"
#' @export
#'
#' @examples
#' \dontrun{
#' data("iris")
#' nchar_df <- get_nchar(iris)
#'
#' set_pgfields(nchar_df, conn = local_con_test)
#' }
set_pgfields <- function(
  input,
  nchar_df,
  default = FALSE,
  conn = NULL
) {

  if (missing(input)) stop("requires input to be provided")
  if (missing(nchar_df)) nchar_df <- get_nchar(input)

  # helper functions
  .non_default_pgtypes <- function(dat = NULL, input) {

    if (nrow(input) >= 32767) pg_int <- "integer" else pg_int <- "smallint"

    .add_dtype <- function(type = NULL) {
      ou <- dplyr::if_else(type == "factor", "character varying",
                           dplyr::if_else(type == "integer", pg_int,
                                          dplyr::if_else(type == "numeric", "numeric",
                                                         dplyr::if_else(type == "character", "character varying",
                                                                        "not assigned"))))
      return(ou)
    }
    .add_ndtype <- function(
      col = NULL,
      type = c("character", "character varying"),
      num = NULL) {

      .add_n <- function(type, n) paste0(type, "(", n, ")")

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

  .final_out <- function(tab) {
    pg_type_vec <- as.character(tab[["pg_type"]])
    names(pg_type_vec) <- tab$rowname
    return(pg_type_vec)
  }

  if (inherits(input, "list")) {

    if (default) {
      if (missing(conn)) stop("requires conn to be provided")
      nchar_df3 <- purrr::map2(nchar_df,
                               DBI::dbDataType(conn, input),
                               function(tab, default_type) {dplyr::mutate(dat, pg_type = default_type)})
      } else {
        nchar_df3 <- purrr::map(nchar_df, .non_default_pgtypes(dat = ., input = input))
        }

    out <- purrr::map(nchar_df3, .final_out)

  }

  if (inherits(input, "data.frame")) {

    if (default) {
      if (missing(conn)) stop("requires conn to be provided")
      nchar_df3 <- dplyr::left_join(
        nchar_df,
        tibble::rownames_to_column(data.frame(pg_type = DBI::dbDataType(conn, input))),
        by = "rowname")
      } else {
        nchar_df3 <- .non_default_pgtypes(dat = nchar_df, input = input)
      }

    out <- .final_out(nchar_df3)

  }

  return(out)

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("pg_type", "nchar_max", "rowname", "dat"))