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

  #Create R objects
  N <- nrow(frame)

  #Get the order of the variables
  CONST_ORDER_FRAME_VARS <- rlang::parse_exprs(colnames(frame))

  #Create assert_frame for various tests
  assert_frame <- as.data.frame(frame)

  #Create a string version of the variable
  string_mos <- as.character(mos)
  symbol_mos <- rlang::parse_expr(string_mos)

  #Check for various conditions

  #Check if n has length of 1
  if(length(n) > 1){
    stop(paste0("n has a length of ", length(n), ".  n must have a length of 1."))
  #Check if n > N
  }else if(n > N){
    stop(paste0("n is ", n, " and the number of rows in the frame is ", N, ". n must be less than or equal to ", N, "."))
  #Check if sampling frame have 1 or more rows
  }else if(N <= 0){
    stop("The frame must have 1 or more rows.")
  #Test the mos parameter variable is in the frame
  }else if( !(string_mos %in% colnames(frame)) ){
      stop(paste0("There is no column on the frame with the name ", string_mos, "."))
  ###########Test the mos parameter variable is numeric    #######################
  }else if( !(typeof(assert_frame[,string_mos]) %in% c("double", "integer"))){
    stop(paste0("The vector ", string_mos, " must be numeric."))
  ###########Test the mos parameter has no missing values   #######################
  }else if( any(is.na(assert_frame[,string_mos])) == TRUE){
    stop(paste0("The vector ", string_mos, " must have no missing values."))
  ###########Test the mos parameter has negative values   #######################
  }else if( any(assert_frame[,string_mos] < 0) == TRUE){
    stop(paste0("The vector ", string_mos, " must have all positive values."))
  #Test if curstrat is NULL or character with length >= 1
  }else if(!is.null(curstrat)){
    if(!is.character(curstrat)){
      stop("Parameter curstrat must be NULL or a character vector.")
    }else if(is.character(curstrat) & length(curstrat) <= 0){
      stop("Parameter curstrat is a character vector with length 0.")
    }
  }

  #Write message if n is equal to N
  if(n==N){
    #Write message depending on whether curstrat is used
    if(is.null(curstrat)){
      message(paste0("You are sampling ", n, " observations from a frame with ", nrow(frame), " rows."))
    }else{
      message(paste0("You are sampling ", n, " observations from a frame with ", nrow(frame), " rows in stratum ", curstrat, "."))
    }
  }


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

  if(!is.null(curstrat)){print("Stratum :", curstrat)}
  print(paste0("The sample size is ", n, "."))
  print(paste0("The number of rows in the frame is ", N, "."))
  print(paste0("The sampling interval is ", k, "."))
  print(paste0("The random start is ", r, "."))

  #Return the data
  return(returndata)

}
