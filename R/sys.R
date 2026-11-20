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
  mosvar <- make.names(c(names(frame), "mos"), unique = TRUE)[ncol(frame) + 1]
  frame[mosvar] <- 1
  outdat <- sys_pps(frame, n = n, mos = mosvar, outall = outall, curstrat = curstrat)
  outdat[[mosvar]] <- NULL
  return(outdat)
}
