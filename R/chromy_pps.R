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

  #Create a string version of the variable
  string_mos <- as.character(mos)
  symbol_mos <- rlang::parse_expr(string_mos)

  N <- nrow(frame)
  moscuml <- cumsum(n*frame[[mos]]/sum(frame[[mos]]))
  I <- floor(moscuml)
  F <- moscuml-I
  r <- runif(N)
  chromy_inner <- function(N, F, I, r){
    hits <- rep(0L, N)
    for (i in 1:N){
      Fprev = ifelse(i==1, 0, F[i-1])
      Iprev = ifelse(i==1, 0, I[i-1])
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
    return(hits)
  }

  frame_hits <-
    frame |>
    tidytable::mutate(
      ExpectedHits = n * ( !!(symbol_mos)/ sum(!!(symbol_mos))),
      NumberHits = chromy_inner(N, F, I, r),
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
