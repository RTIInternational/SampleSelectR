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
  # Check that frame is a valid data frame or adjacent
  if (!(inherits(frame,"data.frame")) &&
      !(inherits(frame,"data.table")) &&
      !(inherits(frame,"tibble")) ) {
    stop("The frame must be a data frame, data.table, or tibble.")
  }

  # Check that frame has at least one column
  if (ncol(frame) < 1) {
    stop("The frame must have at least one column.")
  }

  # Check if n is positive integer and has length of 1
  if (length(n) != 1) {
    stop("n must be a single integer of length 1.")
  }

  if (!(inherits(n,"numeric"))){
    if (inherits(n,"character") && !is.na(as.integer(n))){
      n <- as.integer(n)
    }
    else {
      stop("n must be an integer.")
    }
  }

  if (n <= 0 || n != round(n)) {
    stop("n must be a positive integer.")
  }

  # Calculate N from the frame and throw error if N < n
  N <- nrow(frame)
  if (n > N) {
    stop(paste0("Sample size n = ", n, " must be less than or equal to population size N = ", N, "."))
  }

  # Also check that the frame has at least one row
  if (N < 1) {
    stop("The frame must have at least one row.")
  }

  # Check value of curstrat is NULL or a character string w/ length of 1
  if (!is.null(curstrat)) {
    if (!is.character(curstrat)) {
      stop("curstrat must be a character string or NULL.")
    }

    if (length(curstrat) != 1) {
      stop("curstrat must be a single character value of length 1 or NULL.")
    }
  }

  # Return message if n = N
  if (n == N) {
    if (is.null(curstrat)) {
      message(paste0("You are sampling ", n, " observations from a frame with ", N, " rows."))
    }
    else {
      message(paste0("You are sampling ", n, " observations from a frame with ", N, " rows in stratum ", curstrat, "."))
    }
  }

  # Check for valid values of outall
  if (length(outall) != 1) {
    stop("outall must be a single logical value of length 1.")
  }

  if (!(outall %in% c(TRUE, FALSE))) {
    stop("outall must be either TRUE or FALSE, case sensitive.")
  }

  # Take the srs and create sampling-related columns
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


