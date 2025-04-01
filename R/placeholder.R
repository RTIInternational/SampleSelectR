#' Placeholder function
#'
#' Evaluate if functions are being processed correctly by R CMD CHECK.
#'
#' @param input some sort of input
#'
#' @return always returns the value a data frame with value x 1 through 9
placeholder <- function(input) {
  x <- NULL
  do_something_tidytable <- data.frame(x=9:1) |> tidytable::arrange(x)
  return(do_something_tidytable)
}
