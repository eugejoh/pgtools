#' Test Recent Table Write to Postgres
#'
#' @param x the original table
#' @param y the imported table residing in Postgres
#' @param order_var a \code{character} string specifying variable to order both tables
#'
#' @return console output of tests, see \code{Description}
#' @export
#'
#' @examples
test_pgtable <- function(
  x, #original table
  y, #imported table (this will be done automatically, if not specified)
  order_var = NULL #ordering column in both x and y
  ) {

}