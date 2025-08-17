#' Check if the frame is a valid data structure
#'
#' Ensures that the input frame is a data.frame, data.table, or tibble.
#'
#' @param frame An object to be validated as a sampling frame.
#' @return Stops execution if validation fails. Invisibly returns TRUE.
#' @keywords internal
check_frame_type <- function(frame) {
  if (!(inherits(frame, "data.frame")) &&
    !(inherits(frame, "data.table")) &&
    !(inherits(frame, "tibble"))) {
    stop("The frame must be a data frame, data.table, or tibble.")
  }
  invisible(TRUE)
}



#' Validate the sample size `n`
#'
#' Performs a set of checks on the sample size, including numeric type, bounds, and optional strata.
#' @param n Desired sample size.
#' @param frame Data frame or data table of the sampling frame.
#' @param curstrat Optional character vector indicating the stratum.
#' @param n_le_N Logical. Should `n` be forced to be less than or equal to `N`?
#' @return Stops execution if validation fails or outputs a message if `n == N`. Invisibly returns TRUE.
#' @keywords internal
check_n <- function(n, frame, curstrat, n_le_N = FALSE) {
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

  if (n_le_N) {
    if (n > N) {
      stop("Sample size `n` cannot be greater than number of units in frame (`N`).")
    }
  }

  invisible(TRUE)
}

#' Validate the 'outall' argument
#'
#' Checks that the outall argument is a single logical value.
#' @param outall A logical value indicating whether to return the full frame or only selected rows.
#' @return Stops execution if validation fails. Invisibly returns TRUE.
#' @keywords internal
check_outall <- function(outall) {
  if (length(outall) != 1 || !is.logical(outall)) {
    stop("Argument 'outall' must be a single logical value (TRUE or FALSE).")
  }
  invisible(TRUE)
}

#' Validate the measure of size (MOS) variable
#'
#' Ensures that the MOS variable exists in the frame, is numeric, and contains no missing or negative values.
#' @param mos A string or symbol representing the column name of the measure of size.
#' @param frame The sampling frame as a data.frame or similar object.
#' @return Stops execution if validation fails. Invisibly returns TRUE.
#' @keywords internal
check_string_mos <- function(mos, frame) {
  if (!is.character(mos)) {
    stop("The `mos` parameter must be a character string.")
  }
  assert_frame <- as.data.frame(frame)

  if (!(mos %in% colnames(assert_frame))) {
    stop(paste0("There is no column on the frame with the name '", mos, "'."))
  }

  mos_vector <- assert_frame[[mos]]

  if (!is.numeric(mos_vector)) {
    stop(paste0("The vector '", mos, "' must be numeric."))
  }

  if (any(is.na(mos_vector))) {
    stop(paste0("The vector '", mos, "' must have no missing values."))
  }

  if (any(mos_vector < 0)) {
    stop(paste0("The vector '", mos, "' must have all non-negative values."))
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
#' @return Prints output to the console. Invisibly returns NULL
#' @keywords internal
Sampling_Output <- function(n, N, k = NULL, r = NULL, curstrat = NULL) {
  if (!is.null(curstrat)) {
    prefix <- "--"
    cat("Stratum:", curstrat, "\n")
  } else {
    prefix <- ""
  }

  cat(prefix, "Frame size: ", N, "\n", sep = "")
  cat(prefix, "Sample size: ", n, "\n", sep = "")

  if (!is.null(k)) {
    cat(prefix, "Sampling interval (k): ", k, "\n", sep = "")
  }
  if (!is.null(r)) {
    cat(prefix, "Random start (r): ", r, "\n", sep = "")
  }

  invisible(NULL)
}
