#' Select a systematic PPS sample
#'
#' Draws a sequential sample of size n. Each unit’s probability of selection is proportional to its size measure. This is a minimum replacement method.
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
#' @return Returns an object of type tidytable that contains the weight, selection probability, number of hits, etc plus all original variables.
chromy_pps <- function(frame, n, mos, outall=FALSE, curstrat=NULL){
  # TODO: Add in asserts

  N <- nrow(frame)
  mosv <- n*frame[[mos]]/sum(frame[[mos]])
  moscuml <- cumsum(mosv)
  I <- floor(moscuml)
  F <- moscuml-I

  chromy_inner <- function(F, I){
    N <- length(F)
    hits <- rep(0, N)
    r <- runif(N)

    for (i in seq_len(N)){
      if (i==1L){
        Fprev <- 0.0
        Iprev <- 0.0
      } else{
        Fprev <- F[i-1]
        Iprev <- I[i-1]
      }
      PriorSum <- sum(hits)

      if (F[i]==0){ # Condition 1
        hits[i] <- 0
      } else if (F[i] > Fprev){ # Condition 2
        if (PriorSum==Iprev){ # First column
          if (r[i] < (F[i]-Fprev)/ (1-Fprev)){
            hits[i] <- I[i] +1- PriorSum
          } else{
            hits[i] <- I[i]-PriorSum
          }
        } else{ # Second column
          hits[i] <- I[i] + 1-PriorSum
        }
      } else{ # Condition 3
        if (PriorSum==Iprev){ # First column
          hits[i] <- I[i]-PriorSum
        } else{ # Second column
          if (r[i] < F[i]/Fprev){
            hits[i] <- I[i] +1- PriorSum
          } else{
            hits[i] <- I[i]- PriorSum
          }
        }

      }
    }
    hits
  }

  frame_hits <-
    frame |>
    tidytable::mutate(
      ExpectedHits = mosv,
      NumberHits = chromy_inner(F, I),
      SelectionIndicator = .data$NumberHits > 0,
      SamplingWeight = ifelse(.data$SelectionIndicator, 1/.data$ExpectedHits, NA),
    )

  if (outall){
    return(frame_hits)
  } else{
    sample_out <- frame_hits |>
      tidytable::filter(.data$SelectionIndicator) |>
      tidytable::select(-.data$SelectionIndicator)

    return(sample_out)
  }

}
