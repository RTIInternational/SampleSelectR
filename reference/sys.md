# Systematic Sampling Without Replacement

Draws a systematic sample of size `n` from a data frame. Each unit has
an equal probability of being selected.

## Usage

``` r
sys(frame, n, curstrat = NULL, outall = FALSE)
```

## Arguments

- frame:

  a `data.frame`, `tibble`, or `data.table` containing the sampling
  frame. Must have at least one row.

- n:

  Integer. The desired sample size. Must be less than or equal to the
  number of rows in `frame`.

- curstrat:

  Character or NULL. Optional stratum name for printing messages.

- outall:

  logical indicator for whether full frame is returned or just the
  sample

## Value

a `data.table` with the original columns plus:

- SelectionProbability:

  Equal to n / N for all units.

- SamplingWeight:

  Equal to N / n for all units.

- SelectionIndicator:

  TRUE if selected, FALSE otherwise.-only included if `outall=TRUE`

- NumberHits:

  1 if selected, 0 otherwise.

- ExpectedHits:

  Equal to SelectionProbability.

## References

Kalton, G. (1983). *Introduction to Survey Sampling*. SAGE Publications.
https://doi.org/10.4135/9781412984683

## Examples

``` r
# Sort by REGION, DIVISION, and Pop_Tot, then take a sample
puma_2023 |>
  tidytable::arrange(Region, Division, Pop_Tot) |>
  sys(n = 50, outall = FALSE)
#> Frame size: 2462
#> Sample size: 50
#> Sampling interval (k): 49.24
#> Random start (r): 18.4339
#> # A tidytable: 50 × 27
#>    GEOID   Name  State Region Division Pop_Tot Pop_Pct_White_NH Pop_Pct_Black_NH
#>    <chr>   <chr> <chr> <fct>  <fct>      <dbl>            <dbl>            <dbl>
#>  1 2300600 Andr… ME    North… New Eng…  112323             88.2             4.61
#>  2 2500904 Norf… MA    North… New Eng…  139589             64.4            12.8 
#>  3 3401105 Monm… NJ    North… Middle …  100369             74.5             3.82
#>  4 3603002 Rock… NY    North… Middle …  109404             65.0             8.22
#>  5 3400903 Midd… NJ    North… Middle …  117567             32.0            14.3 
#>  6 4201803 Alle… PA    North… Middle …  127233             88.2             3.32
#>  7 4203230 Phil… PA    North… Middle …  139415             68.4            11.0 
#>  8 4201200 Cent… PA    North… Middle …  158041             83.5             3.21
#>  9 4201701 Pitt… PA    North… Middle …  196078             58.1            24.8 
#> 10 1802401 Mari… IN    Midwe… East No…  109931             32.8            46.7 
#> # ℹ 40 more rows
#> # ℹ 19 more variables: Pop_Pct_AIAN_NH <dbl>, Pop_Pct_Asian_NH <dbl>,
#> #   Pop_Pct_NHPI_NH <dbl>, Pop_Pct_Other_NH <dbl>, Pop_Pct_Hispanic <dbl>,
#> #   HU_Tot <dbl>, HU_Pct_Occupied <dbl>, HU_Pct_Vacant <dbl>,
#> #   Pop_Pct_0004 <dbl>, Pop_Pct_0509 <dbl>, Pop_Pct_1014 <dbl>,
#> #   Pop_Pct_2544 <dbl>, Pop_Pct_4564 <dbl>, Pop_Pct_6574 <dbl>,
#> #   Pop_Pct_75plus <dbl>, Pop_Pct_1517 <dbl>, Pop_Pct_1824 <dbl>, …

# Return full dataset with selection indicators
puma_2023 |>
  tidytable::arrange(Region, Division, Pop_Tot) |>
  sys(n = 50, outall = TRUE)
#> Frame size: 2462
#> Sample size: 50
#> Sampling interval (k): 49.24
#> Random start (r): 43.60074
#> # A tidytable: 2,462 × 28
#>    GEOID   Name  State Region Division Pop_Tot Pop_Pct_White_NH Pop_Pct_Black_NH
#>    <chr>   <chr> <chr> <fct>  <fct>      <dbl>            <dbl>            <dbl>
#>  1 2500505 Worc… MA    North… New Eng…   99893             51.7            9.42 
#>  2 2500903 Norf… MA    North… New Eng…  101322             54.4            6.06 
#>  3 2500705 Esse… MA    North… New Eng…  104233             37.8           10.1  
#>  4 0920902 West… CT    North… New Eng…  104909             80.9            1.97 
#>  5 2501101 Plym… MA    North… New Eng…  105080             26.6           37.0  
#>  6 2500603 Midd… MA    North… New Eng…  105224             80.4            3.56 
#>  7 2500504 Worc… MA    North… New Eng…  105608             49.4           13.4  
#>  8 3300602 Grea… NH    North… New Eng…  106658             88.9            0.991
#>  9 0920301 Nort… CT    North… New Eng…  107291             87.3            1.52 
#> 10 2500606 Midd… MA    North… New Eng…  107565             74.7            1.73 
#> # ℹ 2,452 more rows
#> # ℹ 20 more variables: Pop_Pct_AIAN_NH <dbl>, Pop_Pct_Asian_NH <dbl>,
#> #   Pop_Pct_NHPI_NH <dbl>, Pop_Pct_Other_NH <dbl>, Pop_Pct_Hispanic <dbl>,
#> #   HU_Tot <dbl>, HU_Pct_Occupied <dbl>, HU_Pct_Vacant <dbl>,
#> #   Pop_Pct_0004 <dbl>, Pop_Pct_0509 <dbl>, Pop_Pct_1014 <dbl>,
#> #   Pop_Pct_2544 <dbl>, Pop_Pct_4564 <dbl>, Pop_Pct_6574 <dbl>,
#> #   Pop_Pct_75plus <dbl>, Pop_Pct_1517 <dbl>, Pop_Pct_1824 <dbl>, …

```
