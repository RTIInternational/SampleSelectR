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
#' @export

srs <- function(frame, n, outall=FALSE, curstrat=NULL)
{
  # Check inputs
  check_frame_type(frame)
  check_n(n, frame, curstrat, n_le_N=TRUE)

  # Check for valid values of outall
  if (length(outall) != 1) {
    stop("outall must be a single logical value of length 1.")
  }

  if (!(outall %in% c(TRUE, FALSE))) {
    stop("outall must be either TRUE or FALSE, case sensitive.")
  }

  # Take the srs and create sampling-related columns
  N <- nrow(frame)
  selectedVector <- sample(x=N, size=n, replace=FALSE)
  frame <- frame |> tidytable::mutate(rowNum = tidytable::row_number(),
                            SelectionProbability = n/N,
                            SamplingWeight = ifelse(.data$rowNum %in% selectedVector, N/n, NA),
                            SelectionIndicator = ifelse(.data$rowNum %in% selectedVector, TRUE, FALSE)) |>
                    tidytable::select(-.data$rowNum)


  # Print the sample and population size to console w/ mention of stratum, if applicable
  if (is.null(curstrat)) {
    print(paste0("Sample size n = ", n))
    print(paste0("Population size N = nrow(frame) = ", N))
  }
  else {
    print(paste0("Sample size n = ", n, " in stratum ", curstrat))
    print(paste0("Population size N = nrow(frame) = ", N, " in stratum ", curstrat))
  }

  # Return only the sample or the frame with selection indicator based on value of outall
  if (!outall) {
    sample <- frame |> tidytable::filter(.data$SelectionIndicator) |>
                       tidytable::select(-.data$SelectionIndicator)
    return(sample)
  }

  else if (outall) {
    return(frame)
  }

}


