# Validate the sample size `n`

Performs a set of checks on the sample size, including numeric type,
bounds, and optional strata.

## Usage

``` r
check_n(n, frame, curstrat, n_le_N = FALSE)
```

## Arguments

- n:

  Desired sample size.

- frame:

  Data frame or data table of the sampling frame.

- curstrat:

  Optional character vector indicating the stratum.

- n_le_N:

  Logical. Should `n` be forced to be less than or equal to `N`?

## Value

Stops execution if validation fails or outputs a message if `n == N`.
Invisibly returns TRUE.
