#' Title
#'
#' @param input
#' @param default
#' @param conn
#'
#' @return
#' @export
#'
#' @examples
set_pgfields <- function(
  input, #dataframe OR list of dataframes
  default = FALSE, #use of DBI::dbDataType when TRUE or custom assigned when FALSE
  conn = NULL #database connection
  ) {

}