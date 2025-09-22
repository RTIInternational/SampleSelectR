#' Simple random sampling function
#'
#' Draws a simple random sample of size n.
#'
#' @param frame A data frame, data.table, or tibble from which to draw the sample. No default.
#'
#' @param n The sample size to draw from the frame. Must be a positive integer. No default.
#'
#' @param outall A logical value indicating whether to return the entire frame with a selection indicator or just the sample. Default is FALSE.
#'
#' @param curstrat A character string indicating the current stratum. Used only for printing messages when n = N. Default is NULL.
#'
#' @return A tidytable object containing the entire frame with a selection indicator or just the sample, dependent on the value of outall.
#' Selection probability and sampling weight are also included. The sample size, n, and the population size, N, are printed to the console.
#'
#'
#' @examples
#' # Random sample of 200 universities, only sampled rows returned
#' ipeds |>
#'   tidytable::filter(!is.na(ENRTOT)) |>
#'   srs(n = 200, outall = FALSE)
#'
#' # Return full dataset with selection indicators
#' ipeds |>
#'   tidytable::filter(!is.na(ENRTOT)) |>
#'   srs(n = 200, outall = TRUE)
#'

#' @export

srs <- function(frame, n, outall = FALSE, curstrat = NULL) {
  check_frame_type(frame)
  check_n(n, frame, curstrat, n_le_N = TRUE)
  check_outall(outall)

  N <- nrow(frame)

  # Take the srs and create sampling-related columns
  selectedVector <- sample(x = N, size = n, replace = FALSE)
  frame <- frame |>
    tidytable::mutate(
      rowNum = tidytable::row_number(),
      SelectionProbability = n / N,
      SamplingWeight = ifelse(.data$rowNum %in% selectedVector, N / n, NA),
      SelectionIndicator = ifelse(.data$rowNum %in% selectedVector, TRUE, FALSE)
    ) |>
    tidytable::select(-tidytable::all_of("rowNum"))


  # Output to screen
  Sampling_Output(n, N, curstrat = curstrat)

  # Return only the sample or the frame with selection indicator based on value of outall
  if (!outall) {
    sample <- frame |>
      tidytable::filter(.data$SelectionIndicator) |>
      tidytable::select(-tidytable::all_of("SelectionIndicator"))
    return(sample)
  } else if (outall) {
    return(frame)
  }
}
