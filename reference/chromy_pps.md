# Select a sequential PPS sample

Draws a sequential sample of size n. Each unit's probability of
selection is proportional to its size measure. This is a minimum
replacement method as discussed in Chromy (1979).

## Usage

``` r
chromy_pps(frame, n, mos, outall = FALSE, curstrat = NULL)
```

## Arguments

- frame:

  The input data frame for the function to work on.

- n:

  The sample size, the parameter expects an integer of length 1. The
  function will check if n is less than or equal to the number of rows
  in the input frame.

- mos:

  The measure of size, the parameter expects a character string to
  indicate the variable to be used as the measure of size. The variable
  must exist on the frame and be a non-missing and non-negative numeric
  variable.

- outall:

  Output all records or selected records. If outall is TRUE, then all
  records are returned and the following variables are created:
  SelectionIndicator, SamplingWeight, NumberHits, and ExpectedHits. If
  outall is FALSE, then the selected records are returned and the
  following variables are created: SamplingWeight, NumberHits,
  ExpectedHits.

- curstrat:

  A character variable that specifies the current strata, only used as
  an assertion for the n == N test.

## Value

Returns an object of type tidytable that contains the weight, expected
hits (selection probability for nonreplacement designs), and number of
hits plus all original variables. Include a SelectionIndicator variable
if outall=TRUE

## References

Chromy, J. R. (1979). “Sequential Sample Selection Methods.” In
*Proceedings of the Survey Research Methods Section*, 401–406.
Washington, DC: American Statistical Association.
<http://www.asasrms.org/Proceedings/papers/1979_081.pdf>

## Examples

``` r
# PPS sample of counties using population size as MOS
# LA county will be selected two or three times based on expected hits
# Cook, Harris, and Maricopa will be selected one or two times based on expected hits

county_2023 |>
  tidytable::select(GEOID, Name, Pop_Tot) |>
  chromy_pps(n = 75, mos = "Pop_Tot") |>
  tidytable::arrange(desc(ExpectedHits))
#> Frame size: 3144
#> Sample size: 75
#> # A tidytable: 74 × 6
#>    GEOID Name                     Pop_Tot ExpectedHits NumberHits SamplingWeight
#>    <chr> <chr>                      <dbl>        <dbl>      <dbl>          <dbl>
#>  1 06037 Los Angeles County, Cal… 9848406        2.22           2          0.450
#>  2 17031 Cook County, Illinois    5185812        1.17           1          0.855
#>  3 48201 Harris County, Texas     4758579        1.07           1          0.931
#>  4 04013 Maricopa County, Arizona 4491987        1.01           1          0.987
#>  5 06073 San Diego County, Calif… 3282782        0.741          1          1.35 
#>  6 36047 Kings County, New York   2646306        0.597          1          1.67 
#>  7 32003 Clark County, Nevada     2293764        0.518          1          1.93 
#>  8 12011 Broward County, Florida  1946127        0.439          1          2.28 
#>  9 06085 Santa Clara County, Cal… 1903297        0.429          1          2.33 
#> 10 26163 Wayne County, Michigan   1773767        0.400          1          2.50 
#> # ℹ 64 more rows

county_2023 |>
  tidytable::select(GEOID, Name, Pop_Tot) |>
  chromy_pps(n = 75, mos = "Pop_Tot", outall = TRUE) |>
  tidytable::arrange(desc(ExpectedHits))
#> Frame size: 3144
#> Sample size: 75
#> # A tidytable: 3,144 × 7
#>    GEOID Name  Pop_Tot ExpectedHits NumberHits SelectionIndicator SamplingWeight
#>    <chr> <chr>   <dbl>        <dbl>      <dbl> <lgl>                       <dbl>
#>  1 06037 Los … 9848406        2.22           2 TRUE                        0.450
#>  2 17031 Cook… 5185812        1.17           2 TRUE                        0.855
#>  3 48201 Harr… 4758579        1.07           1 TRUE                        0.931
#>  4 04013 Mari… 4491987        1.01           1 TRUE                        0.987
#>  5 06073 San … 3282782        0.741          0 FALSE                      NA    
#>  6 06059 Oran… 3164063        0.714          1 TRUE                        1.40 
#>  7 12086 Miam… 2685296        0.606          0 FALSE                      NA    
#>  8 36047 King… 2646306        0.597          1 TRUE                        1.67 
#>  9 48113 Dall… 2603816        0.588          0 FALSE                      NA    
#> 10 06065 Rive… 2449909        0.553          0 FALSE                      NA    
#> # ℹ 3,134 more rows
```
