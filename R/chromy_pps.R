#' Select a sequential PPS sample
#'
#' Draws a sequential sample of size n.
#' Each unit’s probability of selection is proportional to its size measure.
#' This is a minimum replacement method as discussed in Chromy (1979).
#'
#' @param frame The input data frame for the function to work on.
#'
#' @param n The sample size, the parameter expects an integer of length 1.  The function will check if n is less than or equal to the number of rows in the input frame.
#'
#' @param mos The measure of size, the parameter expects a character string to indicate the variable to be use as the measure of size.  The variable must exists on the frame and be non-missing and non-negative numeric variable.
#'
#' @param outall Output all records or selected records.  If outall is TRUE, then all records are return and the following variables are created: SelectionIndicator, SamplingWeight, NumberHits, and ExpectedHits.  If outall is FALSE, then the selected records are return and the following variables are created:  SamplingWeight, NumberHits, ExpectedHits.
#'
#' @param curstrat A character variable that specifies the current strata, only used as an assertion for the n == N test.
#'
#' @aliases sequential_pps
#'
#' @return Returns an object of type tidytable that contains the weight, selection probability, number of hits, etc plus all original variables.
#'
#' @references Chromy, J. R. (1979). “Sequential Sample Selection Methods.”
#' In \emph{Proceedings of the Survey Research Methods Section}, 401–406.
#' Washington, DC: American Statistical Association.
#' \url{http://www.asasrms.org/Proceedings/papers/1979_081.pdf}
#' @export
chromy_pps <- function(frame, n, mos, outall = FALSE, curstrat = NULL) {
  check_frame_type(frame)
  check_n(n, frame, curstrat, n_le_N = TRUE)
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
chromy_inner <- function(exphits){

  # Randomly move starting point
  N <- length(exphits)
  exphits_og <- exphits
  randstart <- sample(N, 1)
  if (!randstart %in% c(1, N)){
    reord <- c(seq(randstart, N), seq(1, randstart-1))
  } else if (randstart==1){
    reord <- seq(1, N)
  } else if (randstart==N){
    reord <- c(N, seq(1, N-1))
  }
  unreord <- order(reord)
  exphits <- exphits[reord]

  # Set-up vectors of I, F, lower bound of hits, and a uniform random number
  exphitscum <- cumsum(exphits)
  I <- floor(exphitscum)
  F <- exphitscum-I
  hits <- floor(exphits)
  r <- runif(N)

  PriorSum <- 0

  for (idx in seq_len(N)){
    if (idx==1L){
      Fprev <- 0.0
      Iprev <- 0.0
    } else{
      Fprev <- F[idx-1]
      Iprev <- I[idx-1]
    }

    # See Table 2 in http://www.asasrms.org/Proceedings/papers/1979_081.pdf
    # Doing Case 1 last as it is least likely and loop will go faster

    if (F[idx] > Fprev & F[idx] > 0){ # Condition 2
      if (PriorSum==Iprev){ # First column
        if (r[idx] < (F[idx]-Fprev)/ (1-Fprev)){
          hits[idx] <- I[idx] +1- PriorSum
        } else{
          hits[idx] <- I[idx]-PriorSum
        }
      } else{ # Second column
        hits[idx] <- I[idx] + 1-PriorSum
      }
    } else if (Fprev>F[idx] & F[idx] > 0){ # Condition 3 F(i-1)>F(i)>0
      if (PriorSum==Iprev){ # First column
        hits[idx] <- I[idx]-PriorSum
      } else{ # Second column
        if (r[idx] < F[idx]/Fprev){
          hits[idx] <- I[idx] +1- PriorSum
        } else{
          hits[idx] <- I[idx]- PriorSum
        }
      }
    } else { # Condition 1
      # do nothing
    }
    PriorSum <- sum(hits[1:idx])
  }

  return(hits[unreord])
}
