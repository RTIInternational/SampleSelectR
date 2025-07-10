
#' Check if the frame is a valid data structure
#'
#' Ensures that the input frame is a data.frame, data.table, or tibble.
#'

#' @param frame An object to be validated as a sampling frame.
#' @return Stops with an error if invalid. Invisibly returns TRUE.
#' @keywords internal


check_frame_type <- function(frame){
  if (!(inherits(frame,"data.frame")) &&
      !(inherits(frame,"data.table")) &&
      !(inherits(frame,"tibble")) ) {
    stop("The frame must be a data frame, data.table, or tibble.")
  }
}



#' Validate the sample size `n`
#'
#' Performs a set of checks on the sample size, including numeric type, bounds, and optional strata.

#' @param n Desired sample size.
#' @param frame Data frame or data table of the sampling frame.
#' @param curstrat Optional character vector indicating the stratum.
#' @param n_le_N Logical. Should `n` be forced to be less than or equal to `N`?
#' @return Stops with an error or outputs a message if `n == N`. Returns nothing.
#' @keywords internal

check_n <- function(n, frame, curstrat, n_le_N=FALSE){

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

  # Get a message if it is a  full sample

  if (n == N) {
    msg <- sprintf("You are sampling %d observations from a frame with %d rows", n, N)
    if (!is.null(curstrat)) {
      msg <- paste0(msg, " in stratum ", curstrat)
    }
    message(msg)

  }

  if (n_le_N){
    if (n > N) {
      stop("Sample size `n` cannot be greater than number of units in frame (`N`).")
    }
  }

}



#' Validate the 'outall' argument
#'
#' Checks that the outall argument is a single logical value.

#' @param outall A logical value indicating whether to return the full frame or only selected rows.
#' @return Stops with an error if invalid.
#' @keywords internal

check_outall <- function(outall) {
  if (length(outall) != 1 || !is.logical(outall)) {
    stop("Argument 'outall' must be a single logical value (TRUE or FALSE).")
  }
}




#' Validate the measure of size (MOS) variable
#'
#' Ensures that the MOS variable exists in the frame, is numeric, and contains no missing or negative values.
#' @param mos A string or symbol representing the column name of the measure of size.
#' @param frame The sampling frame as a data.frame or similar object.
#' @return Stops execution if validation fails. Invisibly returns TRUE.
#' @keywords internal


check_string_mos <- function(mos, frame) {
  string_mos <- as.character(mos)
  assert_frame <- as.data.frame(frame)

  if (!(string_mos %in% colnames(assert_frame))) {
    stop(paste0("There is no column on the frame with the name '", string_mos, "'."))
  }

  mos_vector <- assert_frame[[string_mos]]

  if (!is.numeric(mos_vector)) {
    stop(paste0("The vector '", string_mos, "' must be numeric."))
  }

  if (any(is.na(mos_vector))) {
    stop(paste0("The vector '", string_mos, "' must have no missing values."))
  }

  if (any(mos_vector < 0)) {
    stop(paste0("The vector '", string_mos, "' must have all positive values."))
  }

  invisible(TRUE)
}




#' Output sampling summary to the console
#' Displays sample size, frame size, and optionally the sampling interval and random start.

#' @param n Sample size.
#' @param N Frame size.
#' @param k Optional sampling interval.
#' @param r Optional random start.
#' @param curstrat Optional stratum label.
#' @return Prints output to the console. No return value.
#' @keywords internal


Sampling_Output <- function(n, N, k = NULL, r = NULL, curstrat = NULL) {
  prefix <- if (!is.null(curstrat)) paste0(" in ", curstrat) else ""

  cat("Sample size:", n, prefix, "\n")
  cat("Frame size:", N, prefix, "\n")

  if (!is.null(k)) {
    cat("Sampling interval (k):", k, prefix, "\n")
  }
  if (!is.null(r)) {
    cat("Random start (r):", r, prefix, "\n")
  }
}


