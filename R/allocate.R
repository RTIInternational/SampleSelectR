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
#'                   \deqn{n_h = (C - c_0) \times \frac{N_h S_h / \sqrt{c_h}}{\sum\limits_{h=1}^H N_h S_h \sqrt{c_h}}}
#'             where \cr
#'             \eqn{c_h}: cost per unit in stratum \emph{h} (function input is \code{c.h}), and \cr
#'             \eqn{(C - c_0)}: total variable cost (function input is \code{cost})
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
#'   proportional                   \tab X \tab X \tab  \tab  \tab  \tab  \tab X \tab \cr
#'   power                          \tab X \tab X \tab  \tab  \tab  \tab  \tab X \tab X\cr
#'   neyman                         \tab X \tab X \tab X \tab \tab  \tab  \tab X \tab \cr
#'   optimal: cost-constrained      \tab X \tab  \tab X \tab X \tab X \tab  \tab X \tab \cr
#'   optimal: precision-constrained \tab X \tab  \tab X \tab X \tab  \tab X \tab X \tab
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
#' @param cost total variable cost (positive value) \eqn{(C - c_0)}. \cr\cr
#' required for the cost-constrained optimal allocation only, and \code{NULL} otherwise.
#' @param variance fixed variance target for estimated mean (positive value) (\eqn{V_0}). \cr\cr
#' required for the precision-constrained optimal allocation only, and \code{NULL} otherwise.
#' @param power power value for power allocation (\eqn{0 \le \alpha \le 1}). \cr\cr
#' required for the power allocation only, and \code{NULL} otherwise.
#' @param lbound minimum stratum-level sample size (positive integer of length 1). Default value is 2. If N.h < lbound for a stratum, the sample size will be limited to N.h.
#' @param outputs
#' character vector representing whether to output:\cr
#'  \enumerate{
#'    \item the raw allocations before accounting for N.h, lbound, and n.samp if needed \[\code{"raw"}\],
#'    \item the continuous version after accounting for the above \[\code{"adjusted"}\], and/or
#'    \item the rounded version of the above \[\code{"rounded"}\]
#'  } \cr
#'  Default is to only return the final rounded version \[\code{"rounded"}\]. \cr
#'  If one version is requested, the result will be a numeric vector. Otherwise, the result will be a named list matching the requested outputs.
#' @return If one output type is requested, a numeric vector of allocations.
#' If multiple output types are requested, a named list containing the requested
#' allocation vectors.
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

allocate <- function(
  allocation,
  N.h,
  n.samp = NULL,
  S.h = NULL,
  c.h = NULL,
  cost = NULL,
  variance = NULL,
  power = NULL,
  lbound = 2,
  outputs = "rounded"
) {
  allocation <- match.arg(
    allocation,
    c("proportional", "power", "neyman", "optimal")
  )
  outputs <- match.arg(
    outputs,
    c("raw", "adjusted", "rounded"),
    several.ok = TRUE
  )

  ######
  # Check inputs
  .problems <- NULL # Initialize list of problems found with inputs
  .addProblem <- function(
    parameter,
    condition,
    problems = .problems,
    allocation = NULL
  ) {
    # Function to simplify addition to problems found with inputs to our running list (.problems)
    if (is.null(parameter)) {
      # No parameter given
      problem <- condition
    } else if (
      parameter %in%
        c(
          "allocation",
          "n.samp",
          "N.h",
          "S.h",
          "c.h",
          "cost",
          "variance",
          "power",
          "lbound"
        )
    ) {
      problem <- paste0("The ", parameter, " parameter ", condition)
    }
    problemsNew <- c(problems, problem)
    return(problemsNew)
  }

  .condition <- paste0('must be specified for allocation=="', allocation, '"')
  # n.samp parameter
  if (allocation %in% c("proportional", "power", "neyman") & is.null(n.samp)) {
    .problems <- .addProblem(parameter = "n.samp", condition = .condition)
  } else if (
    !allocation %in% c("proportional", "power", "neyman") & !is.null(n.samp)
  ) {
    warning("The n.samp parameter should be NULL", call. = FALSE)
  }
  # N.h parameter
  if (
    allocation %in%
      c("proportional", "power", "neyman", "optimal") &
      is.null(N.h)
  ) {
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
    .problems <- .addProblem(
      parameter = NULL,
      condition = paste0(
        'Exactly one of the cost and variance parameters should be supplied for allocation=="',
        allocation,
        '"'
      )
    )
  } else if (
    !allocation %in% c("optimal") & !(is.null(cost) & is.null(variance))
  ) {
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
    if (
      !all(
        length(n.samp) == 1 &
          (typeof(n.samp) %in%
            c("integer") |
            (typeof(n.samp) == "double" & round(n.samp) == n.samp)) &
          n.samp > 0
      )
    ) {
      .problems <- .addProblem(parameter = "n.samp", condition = .condition)
    }
  }
  # lbound
  if (
    !(length(lbound) == 1 &
      (typeof(lbound) %in%
        c("integer") |
        (typeof(lbound) == "double" & round(lbound) == lbound)) &
      lbound > 0)
  ) {
    .problems <- .addProblem(parameter = "lbound", condition = .condition)
  }

  .condition <- "must be a vector of positive values (integers or non-integers)"
  # N.h
  if (!is.null(N.h)) {
    if (
      !(length(N.h) >= 1 &
        typeof(N.h) %in% c("integer", "double") &
        all(N.h > 0))
    ) {
      .problems <- .addProblem(parameter = "N.h", condition = .condition)
    }
  }
  .condition <- paste0(.condition, " that are the same length as N.h")
  # S.h
  if (!is.null(S.h) & allocation %in% c("neyman", "optimal")) {
    if (
      !(length(S.h) >= 1 &
        typeof(S.h) %in% c("integer", "double") &
        all(S.h > 0) &
        length(S.h) == length(N.h))
    ) {
      .problems <- .addProblem(parameter = "S.h", condition = .condition)
    }
  }
  # c.h
  if (!is.null(c.h) & allocation %in% c("optimal")) {
    if (
      !(length(c.h) >= 1 &
        typeof(c.h) %in% c("integer", "double") &
        all(c.h > 0) &
        length(c.h) == length(N.h))
    ) {
      .problems <- .addProblem(parameter = "c.h", condition = .condition)
    }
  }
  # cost
  .condition <- "must be a positive value (integer or non-integer)"
  if (!is.null(cost) & allocation %in% "optimal") {
    if (
      !all(
        length(cost) == 1 & typeof(cost) %in% c("integer", "double") & cost > 0
      )
    ) {
      .problems <- .addProblem(parameter = "cost", condition = .condition)
    }
  }
  # variance
  if (allocation == "optimal" & !is.null(variance)) {
    if (
      !all(
        length(variance) == 1 &
          typeof(variance) %in% c("integer", "double") &
          variance > 0
      )
    ) {
      .problems <- .addProblem(parameter = "variance", condition = .condition)
    }
  }

  # power parameter
  .condition <- "must be a positive value between 0 and 1, inclusive"
  if (allocation == "power" & !is.null(power)) {
    if (
      !(length(power) == 1 &
        typeof(power) %in% c("integer", "double") &
        0 <= power &
        power <= 1)
    ) {
      .problems <- .addProblem(parameter = "power", condition = .condition)
    }
  }

  if (allocation %in% c("proportional", "power", "neyman")) {
    if (!(is.null(lbound) | is.null(N.h) | is.null(n.samp))) {
      if (length(.problems) == 0) {
        if (sum(pmin(rep(lbound, length(N.h)), N.h)) > n.samp) {
          .problems <- c(.problems, "The requested sample size (n.samp) is too small to satisfy the lower-bound requirements across all strata.")
        }
      }
    }
  }

  if (!(is.null(N.h) | is.null(n.samp))) {
    if (length(.problems) == 0) {
      if (sum(N.h) < n.samp) {
        .problems <- c(.problems, "sum(N.h) is less than n.samp")
      }
    }
  }

  if (allocation %in% c("optimal")) {
    if (!is.null(cost) & length(.problems) == 0) {
      if (sum(pmin(rep(lbound, length(N.h)), N.h) * c.h) > cost) {
        .problems <- c(
          .problems,
          "The specified cost limit is too small to satisfy the minimum required allocation across strata."
        )
      }
    }
  }

  if (allocation %in% c("proportional", "power", "neyman", "optimal")) {
    if (!is.null(N.h)) {
      if (any(lbound > N.h)) {
        message("lbound > N.h for at least one stratum")
      }
    }
  }

  # outputs parameter
  .condition <- 'must be one or more of: "raw", "adjusted", "rounded"'
  if (!all(outputs %in% c("raw", "adjusted", "rounded"))) {
    .problems <- .addProblem(parameter = "outputs", condition = .condition)
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
  #Fixed-total continuous adjustment:
  # * Respects lower bound (lbound)
  # * Respects upper bound (N.h)
  # * Sums to sample size (n.samp)
  # * Unrounded sample sizes
  .adjust_fixed_total <- function(weights, n.samp, lbound, N.h) {
    h <- length(weights)

    # Raw proportional target
    raw <- n.samp * weights / sum(weights)

    #If raw already respects both bounds, use it directly.
    if (all(raw >= lbound) && all(raw <= N.h)) {
      return(as.numeric(raw))
    }

    alloc <- rep(NA_real_, h)
    fixed <- rep(FALSE, h) #Treat stratum fixed (TRUE) vs. free (FALSE)

    repeat {
      active <- !fixed
      remaining <- n.samp - sum(alloc[fixed])

      if (sum(weights[active]) <= 0) {
        stop("No feasible solution: cannot distribute remaining sample.")
      }

      proposed <- remaining * weights[active] / sum(weights[active])

      below <- proposed < lbound
      above <- proposed > N.h[active]

      # No new violations among the currently-active strata: done
      if (!any(below | above)) {
        alloc[active] <- proposed
        break
      }

      active_idx <- which(active)

      if (any(below)) {
        idx <- active_idx[below]
        alloc[idx] <- lbound
        fixed[idx] <- TRUE
      }
      if (any(above)) {
        idx <- active_idx[above]
        alloc[idx] <- N.h[idx] # If N.h < lbound, use N.h. This condition must go second
        fixed[idx] <- TRUE
      }

      if (all(fixed)) {
        break
      }
    }

    alloc
  }

  ######
  # Calculate stratified mean variance

  .stratified_mean_variance <- function(n.h, N.h, S.h, N = sum(N.h)) {
    W.h <- N.h / N
    sum(W.h^2 * (1 - n.h / N.h) * S.h^2 / n.h)
  }

  ######
  # Precision-constrained continuous adjustment:
  # * Respects lower bound (lbound)
  # * Respects upper bound (N.h)
  # * Hits variance target
  # * Unrounded sample sizes

  .adjust_precision_constrained <- function(N.h, S.h, c.h, variance, lbound) {
    h <- length(N.h)
    lbound.h <- pmin(rep(lbound, h), N.h)

    # First consider a solution at the lower bound
    V.lower <- .stratified_mean_variance(lbound.h, N.h, S.h)

    if (variance >= V.lower) {
      warning(
        "The lower-bound allocation already satisfies the variance target; returning lower-bound allocation."
      )
      return(lbound.h)
    }

    N <- sum(N.h)
    a.h <- N.h * S.h / sqrt(c.h)

    # Initialize strata variables
    adjusted_allocations <- rep(NA_real_, h)
    fixed <- rep(FALSE, h) #Treat stratum fixed (TRUE) vs. free (FALSE)

    repeat {
      # Compute variance contribution from fixed strata
      if (any(fixed)) {
        V.fixed <- .stratified_mean_variance(
          n.h = adjusted_allocations[fixed],
          N.h = N.h[fixed],
          S.h = S.h[fixed],
          N = N
        )
      } else {
        V.fixed <- 0
      }

      # If all strata are fixed, break
      if (all(fixed)) {
        V.total <- .stratified_mean_variance(adjusted_allocations, N.h, S.h)
        if (abs(V.total - variance) > 1e-10) {
          warning(
            "No feasible allocation exactly meets the variance target under the imposed bounds."
          )
        }
        break
      }

      # Recompute scaling constant over free strata
      free <- !fixed
      A.free <- sum(N.h[free] * S.h[free] * sqrt(c.h[free]))
      B.free <- sum(N.h[free] * S.h[free]^2)

      denom <- N^2 * (variance - V.fixed) + B.free

      if (denom <= 0) {
        stop(
          "No feasible allocation solution for the requested variance after applying bounds."
        )
      }

      k.free <- A.free / denom

      # Candidate allocations for free strata
      candidate <- k.free * a.h[free]

      # Check for violations
      below <- candidate < lbound.h[free]
      above <- candidate > N.h[free]

      if (!any(below | above)) {
        adjusted_allocations[free] <- candidate
        break
      }

      # Fix violating strata
      free_idx <- which(free)

      if (any(below)) {
        idx <- free_idx[below]
        adjusted_allocations[idx] <- lbound.h[idx]
        fixed[idx] <- TRUE
      }

      if (any(above)) {
        idx <- free_idx[above]
        adjusted_allocations[idx] <- N.h[idx]
        fixed[idx] <- TRUE
      }
    }
    return(adjusted_allocations)
  }

  ######
  # Precision-constrained rounding

  .round_precision_constrained <- function(
    adjusted_allocations,
    N.h,
    S.h,
    c.h,
    variance,
    lbound
  ) {
    rounded_allocations <- floor(adjusted_allocations + 1e-9)
    rounded_allocations <- pmax(rounded_allocations, lbound)
    rounded_allocations <- pmin(rounded_allocations, N.h)
    rounded_allocations <- as.integer(rounded_allocations)

    frac <- adjusted_allocations - floor(adjusted_allocations + 1e-9)

    while (
      .stratified_mean_variance(rounded_allocations, N.h, S.h) > variance
    ) {
      candidates <- which(rounded_allocations < N.h)

      if (length(candidates) == 0) {
        warning(
          "No feasible integer allocation found to satisfy variance target."
        )
        break
      }

      current_var <- .stratified_mean_variance(rounded_allocations, N.h, S.h)
      scores <- rep(-Inf, length(N.h))

      for (h in candidates) {
        trial <- rounded_allocations
        trial[h] <- trial[h] + 1L
        new_var <- .stratified_mean_variance(trial, N.h, S.h)
        var_gain <- current_var - new_var

        #leftover relative to adjusted continuous allocation
        leftover <- pmax(N.h[h] - adjusted_allocations[h], 0)

        #normalize leftover
        leftover_factor <- 1 + leftover / N.h[h]

        #fractional preference
        frac_factor <- 1 + frac[h]

        #combined score
        scores[h] <- (var_gain / c.h[h]) * leftover_factor * frac_factor
      }

      best_h <- which.max(scores)
      if (!is.finite(scores[best_h]) || scores[best_h] <= 0) {
        warning("Could not improve enough to satisfy variance constraint.")
        break
      }

      rounded_allocations[best_h] <- rounded_allocations[best_h] + 1L
    }

    return(rounded_allocations)
  }

  ######
  # Rounding fixed-totals

  .round_fixed_total <- function(adjusted_allocations, n.samp, N.h, lbound) {
    lbound.h <- pmin(rep(lbound, length(N.h)),N.h)

    # https://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum
    low_alloc <- floor(adjusted_allocations + 1e-9)
    indices <- utils::tail(order(adjusted_allocations-low_alloc), round(sum(adjusted_allocations)) - sum(low_alloc))
    low_alloc[indices] <- low_alloc[indices] + 1
    rounded_allocations <- as.integer(low_alloc)

    if (sum(rounded_allocations) != n.samp) {
      stop("Rounded allocation does not sum to n.samp.")
    }

    if (any(rounded_allocations < lbound.h)) {
      stop("Rounded allocation violates effective lower bounds.")
    }

    if (any(rounded_allocations > N.h)) {
      stop("Rounded allocation violates upper bounds.")
    }

    return(rounded_allocations)
  }

  ######
  # Moving onto the actual allocation
  N <- sum(N.h)
  if (allocation == "proportional") {
    raw_allocations <- n.samp * N.h / N

    weights <- N.h

    #Respect lower-bound, upper bound, and n.samp
    adjusted_allocations <- .adjust_fixed_total(
      weights,
      n.samp,
      lbound,
      N.h
    )

    rounded_allocations <- .round_fixed_total(adjusted_allocations, n.samp, N.h, lbound)
  } else if (allocation == "power") {
    N.h.powered <- N.h^power
    raw_allocations <- n.samp * N.h.powered / sum(N.h.powered)

    weights <- N.h.powered

    #Respect lower-bound, upper bound, and n.samp
    adjusted_allocations <- .adjust_fixed_total(
      weights,
      n.samp,
      lbound,
      N.h
    )

    rounded_allocations <- .round_fixed_total(adjusted_allocations, n.samp, N.h, lbound)
  } else if (allocation == "neyman") {
    propNum <- N.h * S.h # Numerator
    propDen <- sum(propNum) # Denominator
    raw_allocations <- n.samp * propNum / propDen

    weights <- propNum

    #Respect lower-bound, upper bound, and n.samp
    adjusted_allocations <- .adjust_fixed_total(
      weights,
      n.samp,
      lbound,
      N.h
    )

    rounded_allocations <- .round_fixed_total(adjusted_allocations, n.samp, N.h, lbound)
  } else if (allocation == "optimal") {
    if (!is.null(cost)) {
      # Cost-constrained
      propNum <- N.h * S.h / sqrt(c.h)
      propDen <- sum(N.h * S.h * sqrt(c.h))
      raw_allocations <- cost * propNum / propDen

      #Lower-bound adjusted version
      lbound.h <- pmin(rep(lbound, length(N.h)), N.h)
      baseline_cost <- sum(lbound.h * c.h)
      adjusted_target <- pmax(raw_allocations, lbound.h)
      adjusted_target <- pmin(adjusted_target, N.h)

      adjusted_cost <- sum(adjusted_target * c.h)

      if (adjusted_cost > cost) {
        #Rescale only the excess above lbound to stay on budget
        excess <- adjusted_target - lbound.h
        excess_cost <- sum(excess * c.h)
        rho <- if (excess_cost > 0) (cost - baseline_cost) / excess_cost else 0
        rho <- max(min(rho, 1), 0)
        adjusted_allocations <- lbound.h + rho * excess
      } else {
        adjusted_allocations <- adjusted_target
      }

      #Deterministic rounded version under budget
      rounded_allocations <- floor(adjusted_allocations + 1e-9)
      rounded_allocations <- pmax(rounded_allocations, lbound.h)
      rounded_allocations <- pmin(rounded_allocations, N.h)
      rounded_allocations <- as.integer(rounded_allocations)

      remaining_budget <- cost - sum(rounded_allocations * c.h)

      repeat {
        candidates <- which(
          rounded_allocations < N.h &
            (c.h <= remaining_budget)
        )
        if (length(candidates) == 0) {
          break
        }

        # prioritize largest fractional parts per unit cost
        gap <- pmax(
          adjusted_allocations[candidates] - rounded_allocations[candidates],
          0
        )
        leftover <- pmax(N.h[candidates] - adjusted_allocations[candidates], 0)

        score <- (gap / c.h[candidates]) *
          (1 + leftover / N.h[candidates])

        j <- candidates[which.max(score)]

        if (score[which.max(score)] <= 0) {
          break
        }

        rounded_allocations[j] <- rounded_allocations[j] + 1L
        remaining_budget <- cost - sum(rounded_allocations * c.h)
      }
    } else if (!is.null(variance)) {
      # Precision-constrained
      propNum <- sum(N.h * S.h * sqrt(c.h))
      propDen <- variance * sum(N.h)**2 + sum(N.h * S.h**2)
      raw_allocations <- N.h * S.h / sqrt(c.h) * propNum / propDen

      adjusted_allocations <- .adjust_precision_constrained(
        N.h,
        S.h,
        c.h,
        variance,
        lbound
      )

      rounded_allocations <- .round_precision_constrained(
        adjusted_allocations,
        N.h,
        S.h,
        c.h,
        variance,
        lbound
      )
    }
  }

  #Note: use the largest remainder (of extra sample left) [NOT NECESSARILY HOW MUCH IS CURRENTLY ALLOCATED)

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
  }

  output <- list()
  out.length <- 0
  if (any(outputs == "raw")) {
    out.length <- out.length + 1
    output[out.length] <- list(raw_allocations)
    names(output)[out.length] <- "raw"
  }
  if (any(outputs == "adjusted")) {
    out.length <- out.length + 1
    output[out.length] <- list(adjusted_allocations)
    names(output)[out.length] <- "adjusted"
  }
  if (any(outputs == "rounded")) {
    out.length <- out.length + 1
    output[out.length] <- list(rounded_allocations)
    names(output)[out.length] <- "rounded"
  }

  #output <- as.integer(rounded_allocations)
  if (allocation == "optimal") {
    n.print <- sum(rounded_allocations)
    if (!is.null(c.h)) {
      actual_cost <- sum(rounded_allocations * c.h)
      n.print <- n.print |>
        paste0(" (sample cost: ", round(actual_cost, digits = 1), ")")
    }
  } else {
    n.print <- n.samp
  }
  message(paste0(
    "Sample allocation of ",
    n.print,
    " using ",
    allocation,
    " with the relevant inputs:"
  ))
  for (i in seq_along(inputs)) {
    message(paste0(
      "  ",
      names(inputs)[i],
      " = ",
      paste0(inputs[[i]], collapse = ", "),
      collapse = ""
    ))
  }
  message()
  message("Output:")
  for (i in seq_along(output)) {
    message(paste0(
      " ",
      names(output)[i],
      " = ",
      paste0(output[[i]], collapse = ", "),
      collapse = ""
    ))
  }

  if (length(output) == 1) {
    output <- output[[1]]
  }

  return(output)
}
