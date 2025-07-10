#' Select a systematic PPS sample
#'
#' Draws a systematic sample of size n. Each unit’s probability of selection is proportional to its size measure.
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
sys_pps <- function(frame, n, mos, outall=FALSE, curstrat=NULL){

  # Check inputs
  check_frame_type(frame)
  check_n(n, frame, curstrat, n_le_N=FALSE)
  check_outall(outall)

  #Create R objects
  N <- nrow(frame)

  check_frame_type(frame)

  #Get the order of the variables
  CONST_ORDER_FRAME_VARS <- rlang::parse_exprs(colnames(frame))


  #Create assert_frame for various tests
  assert_frame <- as.data.frame(frame)

  #Create a string version of the variable
  string_mos <- as.character(mos)
  symbol_mos <- rlang::parse_expr(string_mos)



  check_string_mos(mos, frame)



  #Passes assertion, proceed with rest of function - Need to use assert_frame, given syntax may not work if not data.frame
  cumulativeSize <- cumsum(assert_frame[,string_mos])

  #Assuming this is the total
  totalSize <- cumulativeSize[N]

  #Create variables in dataset
  #Also to make variable rowNum to sort data later for merging with frame
  tbd_data_1 <- frame |>
    tidytable::mutate(
                  rowNum = tidytable::row_number(),
                   ExpectedHits = n * ( !!(symbol_mos)/ totalSize),
                   SamplingWeight = .data$ExpectedHits ^ -1
                   )

  #Create the sampling intervals
  k <- totalSize / n

  #Draw a random number
  r <- stats::runif(1, 0, k)

  selectedSizePoints <- r + k * (0:(n-1))

  sizeIntervals <- c(0, cumulativeSize)

  selectedVector <- findInterval(selectedSizePoints, sizeIntervals)

  #Using selectedVector, get the total counts of each index
  selectedVector_counts <- selectedVector |>
    as.data.frame() |>
    tidytable::count(selectedVector) |>
    #Rename to NumberHits
    tidytable::rename(NumberHits = n)

  tbd_data_2 <- tbd_data_1 |>
    tidytable::left_join(selectedVector_counts, by=c("rowNum" = "selectedVector")) |>
    #Need to zero filled NumberHits
    tidytable::mutate(
      NumberHits = tidytable::replace_na(.data$NumberHits, replace=0),
      SelectionIndicator = .data$rowNum %in% selectedVector,
      #Make SamplingWeight to be NA if not selected
      SamplingWeight = tidytable::case_when(
        SelectionIndicator == TRUE ~ .data$SamplingWeight,
        TRUE ~ NA_real_)) |>
    #Sort the data to be in order
    tidytable::arrange(.data$rowNum) |>
    #Keep selected variables
    tidytable::select(
      #Original variables
      !!!(CONST_ORDER_FRAME_VARS),
      #New created variables in order
      .data$SelectionIndicator, .data$SamplingWeight, .data$NumberHits, .data$ExpectedHits)

  #Using tbd_data_2, need to create the returndata based on the parameter outall
  if(outall == FALSE){
    returndata <- tbd_data_2 |>
      tidytable::filter(.data$SelectionIndicator == TRUE) |>
      tidytable::select(-.data$SelectionIndicator)

  }else{
    returndata <- tbd_data_2
  }

  #Output to screen

  Sampling_Output(n, N, k = k, r = r, curstrat = curstrat)

  #Return the data
  return(returndata)

}
