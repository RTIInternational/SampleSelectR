# Select a systematic PPS sample

Draws a systematic sample of size n. Each unit's probability of
selection is proportional to its size measure.

## Usage

``` r
sys_pps(frame, n, mos, outall = FALSE, curstrat = NULL)
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
  indicate the variable to be use as the measure of size. The variable
  must exists on the frame and be non-missing and non-negative numeric
  variable.

- outall:

  Output all records or selected records. If outall is TRUE, then all
  records are return and the following variables are created:
  SelectionIndicator, SamplingWeight, NumberHits, and ExpectedHits. If
  outall is FALSE, then the selected records are return and the
  following variables are created: SamplingWeight, NumberHits,
  ExpectedHits.

- curstrat:

  A character variable that specifies the current strata, only used as
  an assertion for the n == N test.

## Value

Returns an object of type tidytable that contains the weight, selection
probability, number of hits, etc plus all original variables.

## Examples

``` r
# PPS sample of 75 counties using Pop_Tot as the measure of size
# Return only the sampled counties
sys_pps(county_2023, mos = "Pop_Tot", n = 75, outall = FALSE)
#> Frame size: 3144
#> Sample size: 75
#> Sampling interval (k): 4431834
#> Random start (r): 2881639
#> # A tidytable: 74 × 28
#>    GEOID Name    State Region Division Pop_Tot Pop_Pct_White_NH Pop_Pct_Black_NH
#>    <chr> <chr>   <chr> <fct>  <fct>      <dbl>            <dbl>            <dbl>
#>  1 01089 Madiso… AL    South  East So…  397135             62.3            24.0 
#>  2 04013 Marico… AZ    West   Mountain 4491987             53.4             5.49
#>  3 04019 Pima C… AZ    West   Mountain 1049947             51.2             3.29
#>  4 06001 Alamed… CA    West   Pacific  1651949             28.2             9.63
#>  5 06023 Humbol… CA    West   Pacific   135418             70.2             1.12
#>  6 06037 Los An… CA    West   Pacific  9848406             25.2             7.54
#>  7 06059 Orange… CA    West   Pacific  3164063             37.7             1.52
#>  8 06065 Rivers… CA    West   Pacific  2449909             32.0             6.12
#>  9 06071 San Be… CA    West   Pacific  2187816             25.6             7.62
#> 10 06075 San Fr… CA    West   Pacific   836321             37.5             4.81
#> # ℹ 64 more rows
#> # ℹ 20 more variables: Pop_Pct_AIAN_NH <dbl>, Pop_Pct_Asian_NH <dbl>,
#> #   Pop_Pct_NHPI_NH <dbl>, Pop_Pct_Other_NH <dbl>, Pop_Pct_Hispanic <dbl>,
#> #   HU_Tot <dbl>, HU_Pct_Occupied <dbl>, HU_Pct_Vacant <dbl>,
#> #   Pop_Pct_0004 <dbl>, Pop_Pct_0509 <dbl>, Pop_Pct_1014 <dbl>,
#> #   Pop_Pct_2544 <dbl>, Pop_Pct_4564 <dbl>, Pop_Pct_6574 <dbl>,
#> #   Pop_Pct_75plus <dbl>, Pop_Pct_1517 <dbl>, Pop_Pct_1824 <dbl>, …

# Return the full dataset with selection indicators
sys_pps(county_2023, mos = "Pop_Tot", n = 75, outall = TRUE)
#> Frame size: 3144
#> Sample size: 75
#> Sampling interval (k): 4431834
#> Random start (r): 3011580
#> # A tidytable: 3,144 × 29
#>    GEOID Name    State Region Division Pop_Tot Pop_Pct_White_NH Pop_Pct_Black_NH
#>    <chr> <chr>   <chr> <fct>  <fct>      <dbl>            <dbl>            <dbl>
#>  1 01001 Autaug… AL    South  East So…   59285             71.7            20.0 
#>  2 01003 Baldwi… AL    South  East So…  239945             81.4             7.94
#>  3 01005 Barbou… AL    South  East So…   24757             43.7            46.9 
#>  4 01007 Bibb C… AL    South  East So…   22152             73.7            20.7 
#>  5 01009 Blount… AL    South  East So…   59292             85.0             1.26
#>  6 01011 Bulloc… AL    South  East So…   10157             21.1            71.2 
#>  7 01013 Butler… AL    South  East So…   18807             50.7            44.7 
#>  8 01015 Calhou… AL    South  East So…  116141             69.8            21.6 
#>  9 01017 Chambe… AL    South  East So…   34450             53.9            39.7 
#> 10 01019 Cherok… AL    South  East So…   25224             90.7             3.73
#> # ℹ 3,134 more rows
#> # ℹ 21 more variables: Pop_Pct_AIAN_NH <dbl>, Pop_Pct_Asian_NH <dbl>,
#> #   Pop_Pct_NHPI_NH <dbl>, Pop_Pct_Other_NH <dbl>, Pop_Pct_Hispanic <dbl>,
#> #   HU_Tot <dbl>, HU_Pct_Occupied <dbl>, HU_Pct_Vacant <dbl>,
#> #   Pop_Pct_0004 <dbl>, Pop_Pct_0509 <dbl>, Pop_Pct_1014 <dbl>,
#> #   Pop_Pct_2544 <dbl>, Pop_Pct_4564 <dbl>, Pop_Pct_6574 <dbl>,
#> #   Pop_Pct_75plus <dbl>, Pop_Pct_1517 <dbl>, Pop_Pct_1824 <dbl>, …
```
