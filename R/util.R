check_frame_type <- function(frame){
  if (!(inherits(frame,"data.frame")) &&
      !(inherits(frame,"data.table")) &&
      !(inherits(frame,"tibble")) ) {
    stop("The frame must be a data frame, data.table, or tibble.")
  }
}

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



# message to print out output


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


