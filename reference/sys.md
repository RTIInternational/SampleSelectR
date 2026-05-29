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
#> Random start (r): 20.94748
#> # A tidytable: 50 × 27
#>    GEOID   Name  State Region Division Pop_Tot Pop_Pct_White_NH Pop_Pct_Black_NH
#>    <chr>   <chr> <chr> <fct>  <fct>      <dbl>            <dbl>            <dbl>
#>  1 0920100 Nort… CT    North… New Eng…  112848             82.7            2.50 
#>  2 2300900 Cumb… ME    North… New Eng…  143447             89.6            0.995
#>  3 3401902 Unio… NJ    North… Middle …  101690             70.2            4.59 
#>  4 3600704 Cayu… NY    North… Middle …  109699             88.3            2.23 
#>  5 3401903 Unio… NJ    North… Middle …  117656             41.7           17.0  
#>  6 4200801 Luze… PA    North… Middle …  128152             87.6            2.87 
#>  7 3400501 Pass… NJ    North… Middle …  140397             29.5            5.08 
#>  8 3400601 Huds… NJ    North… Middle …  158078             30.4            7.32 
#>  9 3604112 NYC-… NY    North… Middle …  199120             20.4            7.43 
#> 10 1802402 Mari… IN    Midwe… East No…  110681             64.7           20.6  
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
#> Random start (r): 11.42111
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
