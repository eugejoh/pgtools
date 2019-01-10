#' Title
#'
#' @param input
#' @param field.types
#' @param conn
#' @param schema
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write_pgtable <- function(
  input, #value to write to dbWriteTable
  field.types = NULL, #field types to use in dbWriteTable (this will be an required input)
  conn = NULL, #connection to database
  schema = NULL, #specify schema, optional
  ...
  ) {

}