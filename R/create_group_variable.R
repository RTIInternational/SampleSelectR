#' Create grouping variable
#'
#' Given a data frame and a vector of variables, creates a new variable that assigns a unique ID to each group of rows that have the same values
#' for the given variables. The function will return the data frame with the new id variable added.
#'
#' @param .data A data frame, tibble, or data.table. No default.
#'
#' @param groupvars A vector of character variables which are the names of the columns to sort on. No default.
#'
#' @param groupid ID grouping variable name. No default.
#'
#' @return A copy of the input data with the addition of the grouping ID variable
#'
#' @keywords internal

create_group_variable <- function(.data, groupvars, groupid) {
  # Check that groupid is not a column name in .data
  if (groupid %in% colnames(.data)) {
    stop(paste("The variable", groupid, "is already a column name in .data."))
  }

  # Create grouping variable based on current group id from groupvars
  .data2 <- .data |>
    tidytable::group_by(tidytable::all_of(groupvars)) |>
    tidytable::mutate(!!groupid := tidytable::cur_group_id()) |>
    tidytable::ungroup()

  return(.data2)
}
