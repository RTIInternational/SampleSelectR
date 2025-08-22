#' Serpentine sort function
#'
#' Sorts the rows in a given data frame in a hierarchical fashion given a list of variables (...)
#'
#' @param .data A data frame. No default.
#'
#' @param ... Variables to sort by - must be listed as character variables. No default.
#'
#' @param naorder Logical value for whether to put NA values at the end of the sorted data frame. Default value is TRUE and treats
#'                NA values as the largest values when sorting. A value of FALSE treats NA Values as the smallest values when sorting.
#'
#' @param sortID Logical value that indicates whether the sort group information used to implement the sorting should be added to .data.
#'               By default, sort group is FALSE which means the sort group information is not added to .data. When TRUE, and there are
#'               k variables to sort by, a set of k-1 sort group variables with names sortSerpj (j=1 to k-1) are added to .data.
#'
#' @return A copy of .data that is hierarchically sorted and, if specified, includes the sort group variables created for sorting
#'
#' @export

serp_sort <- function(.data, ..., naorder = TRUE, sortID = FALSE) {
  # Check that naorder is a logical value of length = 1
  if (!(inherits(naorder, "logical")) || length(naorder) != 1) {
    stop("naorder must be a logical value of length 1.")
  }

  # Check that sortorder is a logical value of length = 1
  if (!(inherits(sortID, "logical")) || length(sortID) != 1) {
    stop("sortID must be a logical value of length 1.")
  }

  # Check that frame is a valid data frame
  if (!(inherits(.data, "data.frame"))) {
    stop(".data must be a data frame.")
  }

  # Check that frame has at least one column and at least one row
  if (ncol(.data) < 1) {
    stop(".data must have at least one column.")
  }
  if (nrow(.data) < 1) {
    stop(".data must have at least one row.")
  }

  # Pull variables to sort by from ... and check for validity
  vars <- unlist(list(...))
  if (is.null(vars) || length(vars) < 1 || !is.character(vars)) {
    stop("At least one variable name must be specified to sort by, as a character.")
  }

  for (var in vars) {
    # Check that each variable in vars is a valid column name in .data
    if (!(var %in% colnames(.data))) {
      stop(paste("The variable", var, "is not a valid column name in the data frame."))
    }
  }

  j <- length(vars)
  sort_vars <- vector("list", length = j)
  for (i in seq_along(vars)) {
    sort_vars[[i]] <- vars[seq_len(i)]
  }

  # Sort .data by first variable in list of column names (...)
  .data <- .data[order(.data[[vars[1]]], na.last = ifelse(naorder, TRUE, FALSE)), ]

  # Use helper function to create first sort group variable, called sortSerp1
  .data <- create_group_variable(.data, sort_vars[[1]], "sortSerp1")

  # If only one variable to sort by, then return this data frame
  if (j == 1) {
    if (!sortID) {
      .data <- .data |> tidytable::select(-tidytable::starts_with("sortSerp"))
    }
    return(.data)
  }

  # Sort the next variable in alternating ascending and descending order based on first sort group variable
  for (i in 2:j) {
    # Sort variable j in alternating ascending and descending order based on sort group variable produced by sorting on variables 1 to (j-1), within grouping
    # If sortSerpj is odd, sort in ascending order, if even, sort in descending order

    if (i == 2) {
      sd_i2 <- data.frame() # Initialize empty data frame
      k <- max(.data[[paste0("sortSerp", i - 1)]]) # Take max number of sort groups for following loop
      for (l in 1:k) {
        .data2 <- .data[.data[[paste0("sortSerp", i - 1)]] == l, ] # Splitting data frame into groups to alternate sorting order

        # Create last variable for each sort group
        if (naorder & l %% 2 == 0) { # Want NA values as largest and sort is descending - > put them first
          last <- FALSE
        } else if (naorder & l %% 2 != 0) { # Want NA values as largest and sort is ascending -> put them last
          last <- TRUE
        } else if (!naorder & l %% 2 == 0) { # Want NA values as smallest and sort is descending -> put them last
          last <- TRUE
        } else if (!naorder & l %% 2 != 0) { # Want NA values as smallest and sort is ascending -> put them first
          last <- FALSE
        }

        .data3 <- .data2[order(.data2[[vars[i]]], decreasing = ifelse(l %% 2 == 0, TRUE, FALSE), na.last = last), ] # Sort based on even/odd group
        assign(paste0("sorted_data", l), .data3) # Create data frame for each sorted group
        sd_i2 <- tidytable::bind_rows(sd_i2, get(paste0("sorted_data", l))) # Combine all sorted data frames into one
      }
    } else {
      assign(paste0("sd_i", i), data.frame()) # Initialize empty data frame
      usethis <- get(paste0("sd_i", i - 1)) # Use most recent sorted data frame for sorting
      k <- max(usethis[[paste0("sortSerp", i - 1)]])

      for (l in 1:k) {
        usethis2 <- usethis[usethis[[paste0("sortSerp", i - 1)]] == l, ]

        # Create last variable for each sort group
        if (naorder & l %% 2 == 0) { # Want NA values as largest and sort is descending - > put them first
          last <- FALSE
        } else if (naorder & l %% 2 != 0) { # Want NA values as largest and sort is ascending -> put them last
          last <- TRUE
        } else if (!naorder & l %% 2 == 0) { # Want NA values as smallest and sort is descending -> put them last
          last <- TRUE
        } else if (!naorder & l %% 2 != 0) { # Want NA values as smallest and sort is ascending -> put them first
          last <- FALSE
        }

        usethis3 <- usethis2[order(usethis2[[vars[i]]], decreasing = ifelse(l %% 2 == 0, TRUE, FALSE), na.last = last), ]
        assign(paste0("sd_l", l), usethis3)
        assign(paste0("sd_i", i), tidytable::bind_rows(get(paste0("sd_i", i)), get(paste0("sd_l", l)))) # Combine all sorted data frames into one
      }
      rm(list = ls(pattern = paste0("sd_l", "\\d+"))) # Remove all sd_l data frames for next iteration of loop
    }

    # Create a grouping variable based on sorting variables 1 to j
    md <- get(paste0("sd_i", i)) # Get the current sorted data frame
    assign(paste0("sd_i", i), create_group_variable(md, sort_vars[[i]], paste0("sortSerp", i)))
  }

  final <- get(paste0("sd_i", j)) # Take last sorted data frame as final

  # Remove sortSerp variables per sortID value
  if (!sortID) {
    final <- final |> tidytable::select(-tidytable::starts_with("sortSerp"))
  }

  return(final)
}
