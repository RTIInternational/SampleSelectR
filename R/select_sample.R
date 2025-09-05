#' Sample selection function
#'
#' Selects a random sample using a specified method and sample size. Selection can also optionally be stratified and/or include a measure of
#' size (mos) if a PPS method is used.
#'
#' @param frame A data frame, data.table, or tibble from which to draw the sample. No default.
#'
#' @param method The desired sampling method. Valid options are "srs", "sys_eq", "sys_pps", and "chromy_pps". No default.
#'
#' @param n The sample size to draw from the frame. If strata is NULL, must be a positive integer. If strata is not NULL, must be a
#' data.frame, tibble, or data.table with columns for each stratification variable as the same type and variable names as the frame
#' plus a column with the sample size (\code{sample_size}) which is a positive integer. No default.
#'
#' @param outall A logical value indicating whether to return the entire frame with a selection indicator or just the sample. Default is FALSE.
#'
#' @param strata A vector of characters with variable names of strata. Default is NULL.
#'
#' @param mos A character string defining the variable name on the frame for the measure of size. If not NULL, must have method = c("sys_pps", "chromy_pps").
#'            If NULL, must have method=c("srs", "sys"). Default is NULL.
#'
#' @param sort_vars A character string indicating the variables that should be used to sort the frame. If not NULL, cannot have method = "srs".
#'                  Default is NULL.
#'
#' @param sort_method A character string defining the method to implicitly sort the frame. Valid options are "serpentine" and "nest". Must coincide
#'                    with sort_var; i.e., both must be NULL or both must be not NULL. Default is NULL.
#'
#' @return A tidytable object containing the entire frame with a selection indicator or just the sample, dependent on the value of outall.
#' Selection probability and sampling weight are also included. May include various summary messages to the console when applicable for
#' certain sampling methods.
#'
#' @export

select_sample <- function(frame, method, n, outall = FALSE, strata = NULL, mos = NULL, sort_vars = NULL, sort_method = NULL) {
  #### Begin Assertion Checks ####

  check_frame_type(frame)
  # Check that frame has at least one column
  if (ncol(frame) < 1) {
    stop("The frame must have at least one column.")
  }
  # Check that frame has at least on row
  if (nrow(frame) < 1) {
    stop("The frame must have at least one row.")
  }


  # Check that method is a character string of length 1
  if (!is.character(method) || length(method) != 1) {
    stop("The sampling method must be a character string of length 1.")
  }
  # Check that method is valid sampling option
  method <- tolower(method)
  if (!(method %in% c("srs", "sys_eq", "sys_pps", "chromy_pps", "seq_pps", "sys"))) {
    stop("The sampling method must be one of 'srs', 'sys_eq', 'sys_pps', or 'chromy_pps'.")
  }


  # Check that strata is a vector of character strings
  if (!is.null(strata)) {
    if (!is.character(strata) || length(strata) < 1) {
      stop("If strata is not NULL, it must be a vector of character strings of variable names.")
    }
    # Check that each strata appears as a column in the frame
    if (!all(strata %in% names(frame))) {
      stop("If strata is not NULL, all strata variables must be present on the frame.")
    }
  }


  # Check that n is a positive integer if strata is NULL
  if (is.null(strata)) {
    # Check if n needs to/can be coerced to an integer
    if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != round(n)) {
      stop("Since strata is NULL, n must be a positive integer of length 1.")
    }
  } else {
    # Check that n is a data frame, tibble, or data.table if strata is not NULL
    if (!(inherits(n, "data.frame")) &&
        !(inherits(n, "data.table")) &&
        !(inherits(n, "tibble"))) {
      stop("Since strata is not NULL, n must be a data frame, data.table, or tibble.")
    }


    # Check that there is a 1:1 correspondence between the distinct strata on frame and the values on the sample size table, except for sample_size
    if (!all(strata %in% names(n))) {
      stop("Since strata is not NULL, all strata variables must be present on the sample size table, n.")
    }

    get_distinct_str <- function(dat){
      dat |>
        tidytable::distinct(tidytable::all_of(strata)) |>
        tidytable::select(tidytable::all_of(strata))
    }

    check_frame <- get_distinct_str(frame)
    check_n <- get_distinct_str(n)

    class_frame <- lapply(check_frame, class) |> unlist()
    class_n <- lapply(check_n, class) |> unlist()

    if (nrow(n) != nrow(check_n)) {
      stop("There are repeated strata values on the sample size table. Must be unique and no repeats.")
    }

    if (!all(class_frame == class_n)){
      diffc <- names(class_frame)[which(class_frame != class_n)] |>
        paste(collapse=", ")
      stop(paste0("The class for strata ", diffc, " on the sampling frame and the sample size are not the same."))
    }

    check_notn <- check_frame |>
      tidytable::anti_join(check_n, by=strata)
    check_notframe <- check_n |>
      tidytable::anti_join(check_frame, by=strata)

    if (nrow(check_notframe) >0 || nrow(check_notn) >0){
      make_mismatch_text <- function(dat){
        dat |>
          tidytable::mutate(row=tidytable::row_number()) |>
          tidytable::pivot_longer(cols=-row) |>
          tidytable::mutate(txt=paste(.data$name, .data$value, sep="=")) |>
          tidytable::summarise(
            txt=paste(.data$txt, collapse=", "),
            .by="row"
          ) |>
          tidytable::summarise(
            txt=paste(.data$txt, collapse=";")
          ) |>
          tidytable::pull(.data$txt)
      }

      txtframe <- txtn <- NULL
      if (nrow(check_notframe) >0){
        txtframe <- make_mismatch_text(check_notframe)
      }
      if (nrow(check_notn) > 0){
        txtn <- make_mismatch_text(check_notn)
      }

      if (!is.null(txtframe) && !is.null(txtn)){
        stop(paste("There are strata values on frame but not sample (", txtn, ") and values on sample but not frame (", txtframe, ")."))
      } else if (!is.null(txtframe)){
        stop(paste("There are strata values on sample but not frame (", txtframe, ")."))
      } else if (!is.null(txtn)){
        stop(paste("There are strata values on frame but not sample (", txtn, ")."))
      }
    }

    # Check that sample_size is present on the sample size table, n
    if (!("sample_size" %in% names(n))) {
      stop("Since strata is not NULL, the sample size table, n,  must have a column named 'sample_size'.")
    }
    # Additionally, check that there are no missing values on entire n frame
    if (any(is.na(n$sample_size))) {
      stop("Since strata is not NULL, the sample size table, n, must not have any missing values.")
    }
    # Check that sample_size is a positive integer for each value
    if (!(is.numeric(n$sample_size)) || min(n$sample_size) <= 0 || sum(round(n$sample_size)) != sum(n$sample_size) || sum(is.na(n$sample_size)) > 0) {
      stop("Since strata is not NULL, each value of sample_size must be a positive, non-missing integer.")
    }



  }


  # Check for valid values of outall
  if (length(outall) != 1 && !is.logical(outall)) {
    stop("outall must be a single logical value of length 1.")
  }
  if (!(outall %in% c(TRUE, FALSE))) {
    stop("outall must be either TRUE or FALSE, case sensitive. Cannot be missing or NULL.")
  }


  # Check that mos is NULL or a character string of length 1
  if (!is.null(mos)) {
    # Check that method is valid for mos
    if (!(method %in% c("sys_pps", "chromy_pps", "seq_pps"))) {
      stop("Since mos is not NULL, sampling method must be either 'sys_pps' or 'chromy_pps'.")
    }
    if (!is.character(mos) || length(mos) != 1) {
      stop("Since mos is not NULL, it must be a character string of length 1.")
    }
    # Check that mos is a column in the frame
    if (!(mos %in% names(frame))) {
      stop("Since mos is not NULL, it must be a variable on the frame.")
    }
    # Check that mos is numeric and has no missing values
    if (!is.numeric(frame[[mos]]) || sum(is.na(frame[[mos]])) > 0) {
      stop("Since mos is not NULL, it must be numeric with no missing values.")
    }
  } else {
    # If mos is NULL, check that method is valid for no mos
    if (!(method %in% c("srs", "sys_eq", "sys"))) {
      stop("Since mos is NULL, sampling method must be either 'srs' or 'sys_eq'.")
    }
  }


  # Check that sort_vars is a list of character strings that appear as variables on the frame
  if (!is.null(sort_vars)) {
    if (!is.character(sort_vars) || length(sort_vars) < 1) {
      stop("Since sort_vars is not NULL, it must be a vector of character strings of variable names from the sampling frame.")
    }
    # Check that each sort_var appears as a column in the frame
    if (!all(sort_vars %in% names(frame))) {
      stop("Since sort_vars is not NULL, all sorting variables must be present on the sampling frame.")
    }
    # Check that, if there are strata variables, there is no overlap between sort_vars and strata
    if (!is.null(strata)) {
      if (any(sort_vars %in% strata)) {
        stop("Since sort_vars is not NULL, it must not overlap with the strata variables.")
      }
    }
    # Check that sampling method is not srs
    if (method == "srs") {
      stop("Since sort_vars is not NULL, sampling method cannot be 'srs'; i.e., sampling method must be either 'sys_eq', 'sys_pps', or 'chromy_pps'.")
    }
  } else {
    # Check if sampling method is srs. If not, display message stating that no sorting will be done as there are no sorting variables
    if (method != "srs") {
      message("No sorting variables are provided so frame is assumed to be already sorted for systematic sampling.")
    }
  }


  # Check that sort_method is a character string of length 1
  if (!is.null(sort_method)) {
    if (!is.character(sort_method) || length(sort_method) != 1) {
      stop("The sorting method must be a character string of length 1.")
    }
    # Check that sort_method is valid sorting option
    sort_method <- tolower(sort_method)
    if (!(sort_method %in% c("serpentine", "nest"))) {
      stop("The sorting method must be one of 'serpentine' or 'nest'.")
    }
    # Check that sort_vars is not NULL
    if (is.null(sort_vars)) {
      stop("Since sort_method is not NULL, sort_vars must also not be NULL.")
    }
  } else {
    # Check that sort_vars is also NULL
    if (!is.null(sort_vars)) {
      stop("Since sort_method is NULL, sort_vars must also be NULL.")
    }
  }

  #### End Assertion Checks ####

  # Stratified Sampling
  if (is.null(strata)) {
    frame <- frame |>
      tidytable::mutate(.strata = 1)

    nvec <- n

    n <- data.frame(.strata = 1, sample_size = nvec) |>
      tidytable::mutate(strlab = NA)

    strata <- ".strata"
    strataog <- NULL
  } else {
    strataog <- strata
    strnm <- tidytable::map(strata, ~ paste(.x, n[[.x]], sep = " = ")) |>
      unlist() |>
      matrix(nrow = length(strata), byrow = TRUE) |>
      apply(2, paste, collapse = ", ")
    n <- n |>
      tidytable::mutate(strlab = strnm)
  }

  # First, split the frame into multiple strata frames if strata is not NULL
  strata_frames <-
    frame |>
    tidytable::group_by(tidytable::all_of(strata)) |>
    tidytable::nest() |>
    tidytable::full_join(n, by = strata) |>
    tidytable::ungroup()

  # Sort, if necessary
  if (is.null(sort_method)) {
    # do nothing
  } else if (sort_method == "serpentine") {
    strata_frames <- strata_frames |>
      tidytable::mutate(data = tidytable::map(.data$data, ~ serp_sort(.x, sort_vars)))
  } else if (sort_method == "nest") {
    strata_frames <- strata_frames |>
      tidytable::mutate(data = tidytable::map(.data$data, ~ tidytable::arrange(.x, tidytable::pick(sort_vars))))
  }

  # Finally, pass along to helper sampling function based on method

  inplist <- list(frame = strata_frames$data, n = strata_frames$sample_size, curstrat = strata_frames$strlab)

  if (method == "srs") {
    if (!is.null(strataog)) {
      samptmp <- inplist |>
        tidytable::pmap(.f = function(frame, n, curstrat) srs(frame, n, curstrat, outall = outall))
    } else {
      samptmp <- list(srs(inplist$frame[[1]], inplist$n, outall = outall))
    }
  } else if (method %in% c("sys_eq", "sys")) {
    if (!is.null(strataog)) {
      samptmp <- inplist |>
        tidytable::pmap(.f = function(frame, n, curstrat) sys(frame, n, curstrat, outall = outall))
    } else {
      samptmp <- list(sys(inplist$frame[[1]], inplist$n, outall = outall))
    }
  } else if (method == "sys_pps") {
    if (!is.null(strataog)) {
      samptmp <- inplist |>
        tidytable::pmap(.f = function(frame, n, curstrat) sys_pps(frame, n, curstrat, outall = outall, mos = mos))
    } else {
      samptmp <- list(sys_pps(inplist$frame[[1]], inplist$n, outall = outall, mos = mos))
    }
  } else if (method %in% c("chromy_pps", "seq_pps")) {
    if (!is.null(strataog)) {
      samptmp <- inplist |>
        tidytable::pmap(.f = function(frame, n, curstrat) chromy_pps(frame, n, curstrat, outall = outall, mos = mos))
    } else {
      samptmp <- list(chromy_pps(inplist$frame[[1]], inplist$n, outall = outall, mos = mos))
    }
  }

  sampout <- strata_frames |>
    tidytable::select(tidytable::all_of(strataog)) |>
    tidytable::mutate(sample = samptmp) |>
    data.table() |>
    tidytable::unnest(sample)

  # Return final dataframe
  return(sampout)
}
