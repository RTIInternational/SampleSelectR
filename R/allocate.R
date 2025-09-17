#' Sample allocation
#'
#' Compute the proportional, power, Neyman, and optimal sample allocations.
#'
#' @section Method:
#' The \emph{allocate} function allocates a sample size \emph{n} on \emph{H} strata using one of the following allocation methods:
#' \enumerate{
#'   \item Proportional allocation \[\code{n.samp, N.h, allocation = "proportional"}\]
#'         \deqn{n_h = n \times \frac{N_h}{\sum\limits_{h=1}^H N_h}}
#'         where \cr
#'         \eqn{n}: total sample size to be allocated (function input is \code{n.samp}), and \cr
#'         \eqn{N_h}: population size of stratum \emph{h} (function input is \code{N.h}).
#'   \item Power allocation \[\code{n.samp, N.h, power, allocation = "power"}\]
#'         \deqn{n_h = n \times \frac{N_h^\alpha}{\sum\limits_{h=1}^H N_h^\alpha}}
#'          where \cr
#'          \eqn{\alpha}: a power value to control over-under-sampling with \eqn{0 \le \alpha \le 1} (function input is \code{power}).
#'   \item Neyman allocation \[\code{n.samp, N.h, S.h, allocation = "neyman"}\]
#'         \deqn{n_h = n \times \frac{N_h S_h}{\sum\limits_{h=1}^H N_h S_h}}
#'         where \cr
#'         \eqn{S_h}:  standard deviation of stratum \emph{h} (function input is \code{S.h}).
#'    \item Optimal allocation
#'           \itemize{
#'             \item cost-constrained \[\code{N.h, S.h, c.h, cost, allocation = "optimal"}\]
#'                   \deqn{n_h = (C−c_0) \times \frac{N_h S_h / \sqrt{c_h}}{\sum\limits_{h=1}^H N_h S_h \sqrt{c_h}}}
#'             where \cr
#'             \eqn{c_h}: cost per unit in stratum \emph{h} (function input is \code{c.h}), and \cr
#'             \eqn{(C – c_0)}: total variable cost (function input is \code{cost})
#'             \item precision-constrained \[\code{N.h, S.h, c.h, variance, allocation = "optimal"}\]
#'                   \deqn{n_h = N_h S_h / \sqrt{c_h} \times \frac{\sum\limits_{h=1}^H N_h S_h \sqrt{c_h}}{V_0 \left(\sum\limits_{h=1}^H  N_h \right)^2 + \sum\limits_{h=1}^H N_h S_h^2}}
#'             where \cr
#'             \eqn{V_0}: fixed variance target for estimated mean (function input is \code{variance})
#'           }
#' }
#'
#' The table below presents the relevant inputs for each type; when irrelevant inputs are entered, an error message will be displayed.
#'
#' \tabular{lllllllll}{
#'   \strong{allocation} \tab \strong{N.h} \tab \strong{n.samp} \tab \strong{S.h} \tab \strong{c.h} \tab \strong{cost} \tab \strong{variance} \tab \strong{lbound} \tab \strong{power} \cr
#'   proportional                   \tab ✓ \tab ✓ \tab  \tab  \tab  \tab  \tab ✓ \tab \cr
#'   power                          \tab ✓ \tab ✓ \tab  \tab  \tab  \tab  \tab ✓ \tab ✓\cr
#'   neyman                         \tab ✓ \tab ✓ \tab ✓ \tab ✓ \tab  \tab  \tab ✓ \tab \cr
#'   optimal: cost-constrained      \tab ✓ \tab  \tab ✓ \tab ✓ \tab ✓ \tab  \tab ✓ \tab \cr
#'   optimal: precision-constrained \tab ✓ \tab  \tab ✓ \tab ✓ \tab  \tab ✓ \tab ✓ \tab
#' }
#'
#' @param allocation type of allocation, must be one of \code{"proportional"}, \code{"power"}, \code{"neyman"}, or \code{"optimal"}.
#' @param n.samp total sample size to be allocated (positive integer of length 1). \cr\cr
#'        required for the following allocation types: proportional, power, and Neyman, and \code{NULL} otherwise.
#' @param N.h vector of population stratum sizes (\eqn{N_h}, all positive values), for example \code{c(150, 600, 250)}. \cr\cr
#' required for all allocation types.
#' @param S.h vector of stratum unit standard deviations (positive values same length as \code{N.h}) (\eqn{S_h}). \cr\cr
#' required for the following allocation types: Neyman, and optimal, and \code{NULL} otherwise.
#' @param c.h vector of cost per unit in stratum h (positive values same length as \code{N.h}) (\eqn{c_h}). \cr\cr
#' required for the optimal allocation only, and \code{NULL} otherwise.
#' @param cost total variable cost (positive value) \eqn{(C – c_0)}. \cr\cr
#' required for the cost-constrained optimal allocation only, and \code{NULL} otherwise.
#' @param variance fixed variance target for estimated mean (positive value) (\eqn{V_0}). \cr\cr
#' required for the precision-constrained optimal allocation only, and \code{NULL} otherwise.
#' @param power power value for power allocation (\eqn{0 \le \alpha \le 1}). \cr\cr
#' required for the power allocation only, and \code{NULL} otherwise.
#' @param lbound minimum stratum-level (positive integer of length 1). Default value is 2.
#' @return Integer vector same length of N.h final allocation
#' @export
#' @examples
#' # The first step is getting a frame summary
#' #  Summarize the IPEDS dataset by OBEREG
#' # - N: number of universities per region
#' # - SD_ENRTOT: standard deviation of total enrollment per region
#' # - Filter out rows with missing ENRTOT to ensure accurate variance estimates
#'
#' ipeds_summary <- ipeds |>
#'   tidytable::filter(!is.na(ENRTOT)) |>
#'   tidytable::group_by(OBEREG) |>
#'   tidytable::summarize(
#'     N = tidytable::n(),
#'     SD_ENRTOT = stats::sd(ENRTOT)
#'   ) |>
#'   tidytable::ungroup()
#'
#' # Example of proportional allocation
#' ipeds_summary |>
#'   tidytable::mutate(
#'     n = allocate("proportional", N.h = N, n.samp = 500)
#'   )
#'
#' # Example of power allocation
#' ipeds_summary |>
#'   tidytable::mutate(
#'     n = allocate("power", N.h = N, power = 0.5, n.samp = 500)
#'   )
#'
#' # Example of Neyman allocation
#' ipeds_summary |>
#'   tidytable::mutate(
#'     n = allocate("neyman", N.h = N, n.samp = 500, S.h = SD_ENRTOT)
#'   )
#'
#' # Example of Neyman allocation with a lower bound of 5
#' ipeds_summary |>
#'   tidytable::mutate(
#'     n = allocate("neyman", N.h = N, n.samp = 500, S.h = SD_ENRTOT, lbound = 5)
#'   )
allocate <- function(allocation, N.h, n.samp = NULL, S.h = NULL, c.h = NULL, cost = NULL, variance = NULL, power = NULL, lbound = 2) {
  allocation <- match.arg(allocation, c("proportional", "power", "neyman", "optimal"))

  ######
  # Check inputs
  .problems <- NULL # Initialize list of problems found with inputs
  .addProblem <- function(parameter, condition, problems = .problems, allocation = NULL) { # Function to simplify addition to problems found with inputs to our running list (.problems)
    if (is.null(parameter)) { # No parameter given
      problem <- condition
    } else if (parameter %in% c("allocation", "n.samp", "N.h", "S.h", "c.h", "cost", "variance", "power", "lbound")) {
      problem <- paste0("The ", parameter, " parameter ", condition)
    }
    problemsNew <- c(problems, problem)
    return(problemsNew)
  }

  .condition <- paste0('must be specified for allocation=="', allocation, '"')
  # n.samp parameter
  if (allocation %in% c("proportional", "power", "neyman") & is.null(n.samp)) {
    .problems <- .addProblem(parameter = "n.samp", condition = .condition)
  } else if (!allocation %in% c("proportional", "power", "neyman") & !is.null(n.samp)) {
    warning("The n.samp parameter should be NULL", call. = FALSE)
  }
  # N.h parameter
  if (allocation %in% c("proportional", "power", "neyman", "optimal") & is.null(N.h)) {
    .problems <- .addProblem(parameter = "N.h", condition = .condition)
  }
  # S.h parameter
  if (allocation %in% c("neyman", "optimal") & is.null(S.h)) {
    .problems <- .addProblem(parameter = "S.h", condition = .condition)
  } else if (!allocation %in% c("neyman", "optimal") & !is.null(S.h)) {
    warning("The S.h parameter should be NULL", call. = FALSE)
  }
  # c.h parameter
  if (allocation %in% c("optimal") & is.null(c.h)) {
    .problems <- .addProblem(parameter = "c.h", condition = .condition)
  } else if (!allocation %in% c("optimal") & !is.null(c.h)) {
    warning("The c.h parameter should be NULL", call. = FALSE)
  }
  # power parameter
  if (allocation %in% c("power") & is.null(power)) {
    .problems <- .addProblem(parameter = "power", condition = .condition)
  } else if (!allocation %in% c("power") & !is.null(power)) {
    warning("The power parameter should be NULL", call. = FALSE)
  }
  # optimal allocation: only 1 of cost or variance should be provided
  if (allocation == "optimal" & sum(is.null(cost), is.null(variance)) != 1) {
    .problems <- .addProblem(parameter = NULL, condition = paste0('Exactly one of the cost and variance parameters should be supplied for allocation=="', allocation, '"'))
  } else if (!allocation %in% c("optimal") & !(is.null(cost) & is.null(variance))) {
    if (!is.null(cost)) {
      warning("The cost parameter should be NULL", call. = FALSE)
    }
    if (!is.null(variance)) {
      warning("The variance parameter should be NULL", call. = FALSE)
    }
  }

  ###
  # Check for miscellaneous unexpected parameter values

  # n.samp
  .condition <- "must be a positive integer of length 1"
  if (!is.null(n.samp) & allocation %in% c("proportional", "power", "neyman")) {
    if (!all(length(n.samp) == 1 & (typeof(n.samp) %in% c("integer") | (typeof(n.samp) == "double" & round(n.samp) == n.samp)) & n.samp > 0)) {
      .problems <- .addProblem(parameter = "n.samp", condition = .condition)
    }
  }
  # lbound
  if (!(length(lbound) == 1 & (typeof(lbound) %in% c("integer") | (typeof(lbound) == "double" & round(lbound) == lbound)) & lbound > 0)) {
    .problems <- .addProblem(parameter = "lbound", condition = .condition)
  }

  .condition <- "must be a vector of positive values (integers or non-integers)"
  # N.h
  if (!is.null(N.h)) {
    if (!(length(N.h) >= 1 & typeof(N.h) %in% c("integer", "double") & all(N.h > 0))) {
      .problems <- .addProblem(parameter = "N.h", condition = .condition)
    }
  }
  .condition <- paste0(.condition, " that are the same length as N.h")
  # S.h
  if (!is.null(S.h) & allocation %in% c("neyman", "optimal")) {
    if (!(length(S.h) >= 1 & typeof(S.h) %in% c("integer", "double") & all(S.h > 0) & length(S.h) == length(N.h))) {
      .problems <- .addProblem(parameter = "S.h", condition = .condition)
    }
  }
  # c.h
  if (!is.null(c.h) & allocation %in% c("neyman", "optimal")) {
    if (!(length(c.h) >= 1 & typeof(c.h) %in% c("integer", "double") & all(c.h > 0) & length(c.h) == length(N.h))) {
      .problems <- .addProblem(parameter = "c.h", condition = .condition)
    }
  }
  # cost
  .condition <- "must be a positive value (integer or non-integer)"
  if (!is.null(cost) & allocation %in% "optimal") {
    if (!all(length(cost) == 1 & typeof(cost) %in% c("integer", "double") & cost > 0)) {
      .problems <- .addProblem(parameter = "cost", condition = .condition)
    }
  }
  # variance
  if (allocation == "optimal" & !is.null(variance)) {
    if (!all(length(variance) == 1 & typeof(variance) %in% c("integer", "double") & variance > 0 & length(variance) == 1)) {
      .problems <- .addProblem(parameter = "variance", condition = .condition)
    }
  }

  # power parameter
  .condition <- "must be a positive value between 0 and 1, inclusive"
  if (allocation == "power" & !is.null(power)) {
    if (!(length(power) == 1 & typeof(power) %in% c("integer", "double") & 0 <= power & power <= 1)) {
      .problems <- .addProblem(parameter = "power", condition = .condition)
    }
  }


  if (allocation %in% c("proportional", "power", "neyman")) {
    if (!(is.null(lbound) | is.null(N.h) | is.null(n.samp))){
      if (!all(lbound * length(N.h) <= n.samp)) {
        .problems <- c(.problems, "lbound*length(N.h) must be less than or equal to n.samp")
      }
    }
  }

  if (!(is.null(N.h) | is.null(n.samp))){
    if (length(.problems)==0){
      if (sum(N.h) < n.samp){
        warning("sum(N.h) is less than n.samp")
      }
    }
  }

  if (allocation %in% c("optimal")) {
    if (!is.null(cost) & length(.problems) == 0){
      if (sum(lbound*c.h) > cost){
        warning("No solution exists for specified total cost (sum(lbound*c.h) > cost)")
      }
    }
  }
  ###
  # Aggregate problems and stop the program if necessary
  if (length(.problems) > 0) {
    if (length(.problems) > 1) {
      .problems <- paste0(
        1:length(.problems),
        ": ",
        .problems
      )
    }
    stop(
      "\n",
      paste0(.problems, collapse = "\n")
    )
  }

  ######
  # Moving onto the actual allocation
  N <- sum(N.h)
  if (allocation == "proportional") {
    allocations <- n.samp * N.h / N
  } else if (allocation == "power") {
    N.h.powered <- N.h**power
    N.powered <- sum(N.h.powered)
    allocations <- n.samp * N.h.powered / N.powered
  } else if (allocation == "neyman") {
    propNum <- N.h * S.h # Numerator
    propDen <- sum(propNum) # Denominator
    allocations <- n.samp * propNum / propDen
  } else if (allocation == "optimal") {
    if (!is.null(cost)) { # Cost-constrained
      propNum <- N.h * S.h / sqrt(c.h)
      propDen <- sum(N.h * S.h * sqrt(c.h))
      allocations <- cost * propNum / propDen
    } else if (!is.null(variance)) { # Precision-constrained
      propNum <- sum(N.h * S.h * sqrt(c.h))
      propDen <- variance * sum(N.h)**2 + sum(N.h * S.h**2)
      allocations <- N.h * S.h / sqrt(c.h) * propNum / propDen
    }
  }
  # Calculate the total (raw) sample size
  n <- max(ceiling(sum(allocations)),length(N.h)*lbound)

  sizes <- allocations

  num_groups <- length(N.h)

  # Ensure each allocation is at least lbound
  adjusted_allocations <- pmax(allocations, lbound)

  # Calculate the sum of the adjusted allocations
  total_allocated <- sum(adjusted_allocations)

  # Calculate the difference from the total n
  difference <- n - total_allocated

  # If difference is positive, distribute it proportionally
  if (difference != 0) {
    remaining_sizes <- sizes
    remaining_sizes <- adjusted_allocations-lbound
    remaining_total <- sum(remaining_sizes)

    if (remaining_total > 0) {
      additional_allocations <- difference * remaining_sizes / remaining_total
    } else {
      additional_allocations <- rep(0, num_groups)
    }

    adjusted_allocations <- adjusted_allocations + additional_allocations
  }

  # Round the adjusted allocations
  rounded_allocations <- round(adjusted_allocations)

  # Step 5: Adjust the rounded allocations to ensure the sum equals n
  total_allocated <- sum(rounded_allocations)
  difference <- n - total_allocated
  # Adjust the allocations by adding/subtracting the difference
  while (difference != 0) {
    i <- sample(1:num_groups, 1) # Randomly select an index
    if (difference > 0) {
      rounded_allocations[i] <- rounded_allocations[i] + 1
      difference <- difference - 1
    } else {
      if (rounded_allocations[i] > lbound) {
        rounded_allocations[i] <- rounded_allocations[i] - 1
        difference <- difference + 1
      }
    }
  }

  # Prep for outputting
  if (allocation == "proportional") {
    inputs <- list("N.h" = N.h)
  } else if (allocation == "power") {
    inputs <- list("N.h" = N.h, "power" = power)
  } else if (allocation == "neyman") {
    inputs <- list("N.h" = N.h, "S.h" = S.h)
  } else if (allocation == "optimal" & !is.null(cost)) {
    inputs <- list("N.h" = N.h, "S.h" = S.h, "c.h" = c.h, "cost" = cost)
  } else if (allocation == "optimal" & !is.null(variance)) {
    inputs <- list("N.h" = N.h, "S.h" = S.h, "c.h" = c.h, "variance" = variance)
  } else {
    inputs <- list(N.h, S.h, c.h, cost, variance, power)
  }
  outputs <- as.integer(rounded_allocations)
  if (allocation == "optimal") {
    n.print <- n
  } else {
    n.print <- n.samp
  }
  message(paste0("Sample allocation of ", n.print, " using ", allocation, " with the relevant inputs:"))
  for (i in 1:length(inputs)) {
    message(paste0("  ",
                   names(inputs)[i],
                   " = ",
                   paste0(inputs[[i]], collapse = ", "),
                   collapse = ""
    ))
  }
  message()
  message("Output:")
  message(paste0(outputs,
                 collapse = ", "
  ))
  return(outputs)
}




