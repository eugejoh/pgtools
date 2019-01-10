#' Obtain element lengths from a data frame
#'
#' This function calculates the length of every element within a data frame for later use
#' to map Postgres field types when writing to the database.
#'
#' The output for the specified summary statistic is based on the \code{nchar()} function.
#' NOTE: that \code{\link{nchar}()} counts the literal length of an element or the element coerced to a character.
#' See \code{\link{nchar}()}.
#'
#' @param input a \code{data.frame} or \code{list} of data frames
#' @param export a \code{logical} option export the result as an binary file
#' @param path a \code{file path} option to specify the write location of the binary file
#'
#' @return returns a \code{data.frame} or \code{list} of data frames with rows being the input columns and
#' respective summary statistics and classes for each column
#'
#' @importFrom purrr map map_dbl pmap
#' @importFrom tibble rownames_to_column
#' @importFrom readr write_rds
#' @export
#'
#' @examples
#' get_nchar(iris, type = "mean")
#'
get_nchar <- function(
  input, #dataframe OR list of dataframes
  export = FALSE, #export result as data frame to file
  path = NULL #specify path for export
  ) {

  if (missing(input)) stop("requires input to be provided")

  .make_tbl <- function(a, b, c, d, cl) {
    tibble::rownames_to_column(data.frame(
      nchar_min = a,
      nchar_max = b,
      nchar_median = c,
      nchar_mean = round(d, 4),
      nchar_diff = round(d-c, 4),
      nchar_lgl = ifelse(d-c == 0, TRUE, FALSE),
      class = cl)
      )
  }

  .convert_class <- function(x) {
    dplyr::mutate_if(x, is.factor, as.character)
  }

  .keep_old_class <- function(y) {
    purrr::map_chr(y, class)
  }

  if (inherits(input, "list")) {
    classes <- purrr::map(input, ~.keep_old_class(.))
    input <- purrr::map(input, .convert_class)

    max_nchar <- purrr::map(input, ~purrr::map_dbl(.x, ~max(nchar(.x), na.rm=TRUE)))
    min_nchar <- purrr::map(input, ~purrr::map_dbl(.x, ~min(nchar(.x), na.rm=TRUE)))
    med_nchar <- purrr::map(input, ~purrr::map_dbl(.x, ~median(nchar(.x), na.rm=TRUE)))
    mean_nchar <- purrr::map(input, ~purrr::map_dbl(.x, ~mean(nchar(.x), na.rm=TRUE)))

    nchar_df <- purrr::pmap(list(min_nchar, max_nchar, med_nchar, mean_nchar, classes),
                            .make_tbl)

  } else if (inherits(input, "data.frame")) {
    classes <- .keep_old_class(input)
    input <- .convert_class(input)

    max_nchar <- purrr::map_dbl(input, ~max(nchar(.), na.rm=TRUE))
    min_nchar <- purrr::map_dbl(input, ~min(nchar(.), na.rm=TRUE))
    med_nchar <- purrr::map_dbl(input, ~median(nchar(.), na.rm=TRUE))
    mean_nchar <- purrr::map_dbl(input, ~mean(nchar(.), na.rm=TRUE))


    nchar_df <- .make_tbl(min_nchar, max_nchar, med_nchar, mean_nchar, classes)

  }

  if (export) {
    if (missing(path)) {
      path <- getwd()
    }
    readr::write_rds(x = nchar_df, path = path)
    message(paste0("Export to: ", path))
  }

  return(nchar_df)

}

