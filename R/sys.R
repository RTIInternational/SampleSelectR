#' Systematic Sampling Without Replacement
#'
#' Draws a systematic sample of size `n` from a data frame. Each unit has an equal probability of being selected.
#'
#' @param frame a `data.frame`, `tibble`, or `data.table` containing the sampling frame. Must have at least one row.
#' @param n Integer. The desired sample size. Must be less than or equal to the number of rows in `frame`.
#' @param curstrat Character or NULL. Optional stratum name for printing messages.
#' @param outall logical indicator for whether full frame is returned or just the sample
#'
#' @return a `data.table` with the original columns plus:
#' \describe{
#'   \item{SelectionProbability}{Equal to n / N for all units.}
#'   \item{SamplingWeight}{Equal to N / n for all units.}
#'   \item{SelectionIndicator}{TRUE if selected, FALSE otherwise.-only included if \code{outall=TRUE}}
#'   \item{NumberHits}{1 if selected, 0 otherwise.}
#'   \item{ExpectedHits}{Equal to SelectionProbability.}
#' }
#'
#'
#' @references Kalton, G. (1983). *Introduction to Survey Sampling*. SAGE Publications. https://doi.org/10.4135/9781412984683
#' @examples
#'
#' # Sort by REGION, DIVISION, and Pop_Tot, then take a sample
#' puma_2023 |>
#'   tidytable::arrange(Region, Division, Pop_Tot) |>
#'   sys(n = 50, outall = FALSE)
#'
#' # Return full dataset with selection indicators
#' puma_2023 |>
#'   tidytable::arrange(Region, Division, Pop_Tot) |>
#'   sys(n = 50, outall = TRUE)
#'

#'
#' @export

sys <- function(frame, n, curstrat = NULL, outall = FALSE) {
  check_frame_type(frame)
  check_n(n, frame, curstrat, n_le_N = TRUE)
  check_outall(outall)

  N <- nrow(frame)

  # Sampling method

  k <- N / n # Sampling interval

  r <- runif(1, 1, k) # We use a random start between 1 and k

  selectedVector <- floor(r + k * (0:(n - 1))) # Selected row indices

  # We make sure that selected indices are within frame range

  selectedVector <- selectedVector[selectedVector <= N]

  # Creating variables accordingly
  frame <- frame |>
    tidytable::mutate(
      numrow = tidytable::row_number(),
      SelectionIndicator = .data$numrow %in% selectedVector,
      SelectionProbability = n / N,
      SamplingWeight = ifelse(.data$SelectionIndicator, N / n, NA)
    ) |>
    tidytable::select(-tidytable::all_of("numrow"))

  # Output to screen
  Sampling_Output(n, N, k = k, r = r, curstrat = curstrat)

  # Return only selected rows and make sure the selected sample is a data.frame, tibble, or data.table

  if (outall) {
    return(frame)
  } else {
    sample_output <- frame |>
      tidytable::filter(.data$SelectionIndicator) |>
      tidytable::select(-tidytable::all_of("SelectionIndicator"))

    return(sample_output)
  }
}
