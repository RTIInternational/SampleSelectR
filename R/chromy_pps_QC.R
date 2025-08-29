#' Select a sequential PPS sample
#'
#' Draws a sequential sample of size n.
#' Each unit's probability of selection is proportional to its size measure.
#' This is a minimum replacement method as discussed in Chromy (1979).
#'
#' @param frame The input data frame for the function to work on.
#'
#' @param n The sample size, the parameter expects an integer of length 1.  The function will check if n is less than or equal to the number of rows in the input frame.
#'
#' @param mos The measure of size, the parameter expects a character string to indicate the variable to be used as the measure of size. The variable must exist on the frame and be a non-missing and non-negative numeric variable.
#'
#' @param outall Output all records or selected records.  If outall is TRUE, then all records are returned and the following variables are created: SelectionIndicator, SamplingWeight, NumberHits, and ExpectedHits.  If outall is FALSE, then the selected records are returned and the following variables are created:  SamplingWeight, NumberHits, ExpectedHits.
#'
#' @param curstrat A character variable that specifies the current strata, only used as an assertion for the n == N test.
#'
#' @aliases seq_pps
#'
#' @examples
#' # PPS sample of counties using population size as MOS
#' # LA county will be selected two or three times based on expected hits
#' # Cook, Harris, and Maricopa will be selected one or two times based on expected hits
#'
#' county_2023 |>
#'   tidytable::select(GEOID, Name, Pop_Tot) |>
#'   chromy_pps(n = 75, mos = "Pop_Tot") |>
#'   tidytable::arrange(desc(ExpectedHits))
#'
#' county_2023 |>
#'   tidytable::select(GEOID, Name, Pop_Tot) |>
#'   chromy_pps(n = 75, mos = "Pop_Tot", outall = TRUE) |>
#'   tidytable::arrange(desc(ExpectedHits))
#'
#' @return Returns an object of type tidytable that contains
#' the weight, expected hits (selection probability for nonreplacement designs), and
#' number of hits plus all original variables.
#' Include a SelectionIndicator variable if outall=TRUE
#'
#' @references Chromy, J. R. (1979). “Sequential Sample Selection Methods.”
#' In \emph{Proceedings of the Survey Research Methods Section}, 401–406.
#' Washington, DC: American Statistical Association.
#' \url{http://www.asasrms.org/Proceedings/papers/1979_081.pdf}
#' @export
chromy_pps <- function(frame, n, mos, outall = FALSE, curstrat = NULL) {
  check_frame_type(frame)
  check_n(n, frame, curstrat, n_le_N = FALSE)
  check_outall(outall)
  check_string_mos(mos, frame)

  N <- nrow(frame)
  exphits <- n * frame[[mos]] / sum(frame[[mos]])

  frame_hits <-
    frame |>
    tidytable::mutate(
      ExpectedHits = exphits,
      NumberHits = chromy_inner(exphits),
      SelectionIndicator = .data$NumberHits > 0,
      SamplingWeight = ifelse(.data$SelectionIndicator, 1 / .data$ExpectedHits, NA),
    )

  # Output to screen
  Sampling_Output(n, N, curstrat = curstrat)

  if (outall) {
    return(frame_hits)
  } else {
    sample_out <- frame_hits |>
      tidytable::filter(.data$SelectionIndicator) |>
      tidytable::select(-.data$SelectionIndicator)

    return(sample_out)
  }
}

#' Select a sequential PPS sample given expected number of hits
#'
#' @param exphits A vector of non-negative values
#'
#' @return Returns a vector the same length of exphits with the number of random hits for each unit
#' @keywords internal
#' @references Chromy, J. R. (1979). “Sequential Sample Selection Methods.”
#' In \emph{Proceedings of the Survey Research Methods Section}, 401–406.
#' Washington, DC: American Statistical Association.
#' \url{http://www.asasrms.org/Proceedings/papers/1979_081.pdf}
chromy_inner <- function(exphits) {
  if (!is.numeric(exphits)) {
    stop("exphits must be numeric")
  }

  if (abs(sum(exphits) - round(sum(exphits))) > .Machine$double.eps^0.5) {
    stop("sum of exphits, the sample size, must be an integer")
  }

  if (any(exphits < 0)) {
    stop("Each value of exphits must be non-negative")
  }

  # Randomly move starting point
  N <- length(exphits)
  exphits_og <- exphits
  randstart <- sample(N, 1)
  if (!randstart %in% c(1, N)) {
    reord <- c(seq(randstart, N), seq(1, randstart - 1))
  } else if (randstart == 1) {
    reord <- seq(1, N)
  } else if (randstart == N) {
    reord <- c(N, seq(1, N - 1))
  }
  unreord <- order(reord)
  exphits <- exphits[reord]

  # Set-up vectors of I, F, lower bound of hits, and a uniform random number
  exphitscum <- cumsum(exphits)
  I <- floor(exphitscum)
  F <- exphitscum - I
  hits <- rep(0, N)
  r <- runif(N)

  # See Table 1 and 2 in http://www.asasrms.org/Proceedings/papers/1979_081.pdf
  # Use Table 1 if any exphits > 1 (PMR) - minimum replacement
  # Use Table 2 if all exphits < 1 (PNR) - non replacement

  tabuse <- ifelse(any(exphits > 1), 1, 2)

  PriorSum <- 0

  for (idx in seq_len(N)) {
    if (idx == 1L) {
      Fprev <- 0.0
      Iprev <- 0.0
    } else {
      Fprev <- F[idx - 1]
      Iprev <- I[idx - 1]
    }

    Fcur <- F[idx]

    if (tabuse == 1) {
      compvars <- matrix(
        c(
          0, 0,
          (Fcur - Fprev) / (1 - Fprev), 1,
          0, Fcur / Fprev
        ),
        nrow = 3, byrow = TRUE
      )
    } else {
      compvars <- matrix(
        c(
          1, 0,
          (Fcur - Fprev) / (1 - Fprev), 0,
          1, Fcur / Fprev
        ),
        nrow = 3, byrow = TRUE
      )
    }

    if (PriorSum == Iprev) {
      colsel <- 1
    } else if (PriorSum == (Iprev + 1)) {
      colsel <- 2
    } else {
      stop("Condition doesn't make sense (col)")
    }

    if (Fcur > Fprev & Fcur > 0) { # F(i)>F(i-1)>0
      rowsel <- 2
      #PL:  Added >= to handle the case if Fprev and Fcur are equal
    } else if (Fprev >= Fcur & Fcur > 0) { # F(i-1)>F(i)>0
      rowsel <- 3
    } else if (Fcur == 0) { # F(i) = 0
      rowsel <- 1
    } else {
      stop("Condition doesn't make sense (row)")
    }

    if (idx == N) {
      NewSum <- I[idx] # Needed to add this for a corner case
    } else if (r[idx] < compvars[rowsel, colsel]) {
      NewSum <- I[idx] + 1
    } else {
      NewSum <- I[idx]
    }

    hits[idx] <- NewSum - PriorSum

    PriorSum <- NewSum
    if (PriorSum == I[N]) {
      break
    }
  }

  return(hits[unreord])
}
