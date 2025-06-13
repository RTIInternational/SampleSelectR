
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
#'
#' @export










sys <- function(frame, n, curstrat = NULL,outall=FALSE) {


  if (!is.data.frame(frame)) {
    stop("`frame` must be a data.frame, tibble, or data.table.")
  }

  N <- nrow(frame)
  if (N < 1) {
    stop("Sampling frame must have at least one row.")
  }

  if (!is.numeric(n) || length(n) != 1 || n != as.integer(n)) {
    stop("`n` must be a single integer.")
  }

  if (!is.null(curstrat) && (!is.character(curstrat) || length(curstrat) < 1)) {
    stop("`curstrat` must be NULL or a character vector of length >= 1.")
  }

  if (n > N) {
    stop("Sample size `n` cannot be greater than number of units in frame (`N`).")
  }

  # Get a message if it is a  full sample


  if (n == N) {
    msg <- sprintf("You are sampling %d observations from a frame with %d rows", n, N)
    if (!is.null(curstrat)) {
      msg <- paste0(msg, " in stratum ", curstrat)
    }
    message(msg)


  }


  # Check for valid values of outall
  if (length(outall) != 1) {
    stop("outall must be a single logical value of length 1.")
  }

  if (!(outall %in% c(TRUE, FALSE))) {
    stop("outall must be either TRUE or FALSE, case sensitive.")
  }


  # Sampling method


  k <- N / n  # Sampling interval


  r <- runif(1, 1, k)  # We use a random start between 1 and k



  selectedVector <- floor(r + k * (0:(n - 1)))  # Selected row indices

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
    tidytable::select(-.data$numrow)



  # Output msg
  if (!is.null(curstrat)) {
    cat("Sample size:", n, "in", curstrat, "\n")
    cat("Frame size:", N, "in", curstrat, "\n")
    cat("Sampling interval (k):", k, "in", curstrat, "\n")
    cat("Random start (r):", r, "in", curstrat, "\n")
  } else {
    cat("Sample size:", n, "\n")
    cat("Frame size:", N, "\n")
    cat("Sampling interval (k):", k, "\n")
    cat("Random start (r):", r, "\n")
  }


  # Return only selected rows and make sure the selected sample is a data.frame, tibble, or data.table


  if (outall) {
    return(frame)
  } else {
    sample_output <- frame |>
      tidytable::filter(.data$SelectionIndicator) |>
      tidytable::select(-.data$SelectionIndicator)

    return(sample_output)
  }


}



