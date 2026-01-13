# Simple random sampling function

Draws a simple random sample of size n.

## Usage

``` r
srs(frame, n, outall = FALSE, curstrat = NULL)
```

## Arguments

- frame:

  A data frame, data.table, or tibble from which to draw the sample. No
  default.

- n:

  The sample size to draw from the frame. Must be a positive integer. No
  default.

- outall:

  A logical value indicating whether to return the entire frame with a
  selection indicator or just the sample. Default is FALSE.

- curstrat:

  A character string indicating the current stratum. Used only for
  printing messages when n = N. Default is NULL.

## Value

A tidytable object containing the entire frame with a selection
indicator or just the sample, dependent on the value of outall.
Selection probability and sampling weight are also included. The sample
size, n, and the population size, N, are printed to the console.

## Examples

``` r
# Random sample of 200 universities, only sampled rows returned
ipeds |>
  tidytable::filter(!is.na(ENRTOT)) |>
  srs(n = 200, outall = FALSE)
#> Frame size: 5914
#> Sample size: 200
#> # A tidytable: 200 × 18
#>    UNITID INSTNM      STABBR  FIPS OBEREG ICLEVEL SECTOR LOCALE DEGGRANT HLOFFER
#>     <dbl> <chr>       <chr>  <dbl> <fct>  <fct>   <fct>  <fct>  <fct>    <fct>  
#>  1 100690 Amridge Un… AL         1 South… Four o… Priva… City:… Degree-… Doctor…
#>  2 100812 Athens Sta… AL         1 South… Four o… Publi… Town:… Degree-… Master…
#>  3 101435 Huntingdon… AL         1 South… Four o… Priva… City:… Degree-… Master…
#>  4 101587 University… AL         1 South… Four o… Publi… Rural… Degree-… Doctor…
#>  5 102030 Bishop Sta… AL         1 South… At lea… Publi… City:… Degree-… Associ…
#>  6 102076 Snead Stat… AL         1 South… At lea… Publi… Town:… Degree-… Associ…
#>  7 105154 Mesa Commu… AZ         4 South… At lea… Publi… City:… Degree-… At lea…
#>  8 106795 Cossatot C… AR         5 South… At lea… Publi… Town:… Degree-… Associ…
#>  9 110404 California… CA         6 Far W… Four o… Priva… City:… Degree-… Doctor…
#> 10 110583 California… CA         6 Far W… Four o… Publi… City:… Degree-… Doctor…
#> # ℹ 190 more rows
#> # ℹ 8 more variables: ENRTOT <dbl>, EFUG <dbl>, EFUG1ST <dbl>, EFUGFT <dbl>,
#> #   EFGRAD <dbl>, EFGRADFT <dbl>, SelectionProbability <dbl>,
#> #   SamplingWeight <dbl>

# Return full dataset with selection indicators
ipeds |>
  tidytable::filter(!is.na(ENRTOT)) |>
  srs(n = 200, outall = TRUE)
#> Frame size: 5914
#> Sample size: 200
#> # A tidytable: 5,914 × 19
#>    UNITID INSTNM      STABBR  FIPS OBEREG ICLEVEL SECTOR LOCALE DEGGRANT HLOFFER
#>     <dbl> <chr>       <chr>  <dbl> <fct>  <fct>   <fct>  <fct>  <fct>    <fct>  
#>  1 100654 Alabama A … AL         1 South… Four o… Publi… City:… Degree-… Doctor…
#>  2 100663 University… AL         1 South… Four o… Publi… City:… Degree-… Doctor…
#>  3 100690 Amridge Un… AL         1 South… Four o… Priva… City:… Degree-… Doctor…
#>  4 100706 University… AL         1 South… Four o… Publi… City:… Degree-… Doctor…
#>  5 100724 Alabama St… AL         1 South… Four o… Publi… City:… Degree-… Doctor…
#>  6 100751 The Univer… AL         1 South… Four o… Publi… City:… Degree-… Doctor…
#>  7 100760 Central Al… AL         1 South… At lea… Publi… Town:… Degree-… Associ…
#>  8 100812 Athens Sta… AL         1 South… Four o… Publi… Town:… Degree-… Master…
#>  9 100830 Auburn Uni… AL         1 South… Four o… Publi… City:… Degree-… Doctor…
#> 10 100858 Auburn Uni… AL         1 South… Four o… Publi… City:… Degree-… Doctor…
#> # ℹ 5,904 more rows
#> # ℹ 9 more variables: ENRTOT <dbl>, EFUG <dbl>, EFUG1ST <dbl>, EFUGFT <dbl>,
#> #   EFGRAD <dbl>, EFGRADFT <dbl>, SelectionProbability <dbl>,
#> #   SamplingWeight <dbl>, SelectionIndicator <lgl>
```
