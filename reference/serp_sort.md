# Serpentine sort function

Sorts the rows in a given data frame in a hierarchical fashion given a
list of variables (...)

## Usage

``` r
serp_sort(.data, ..., naorder = TRUE, sortID = FALSE)
```

## Arguments

- .data:

  A data frame. No default.

- ...:

  Variables to sort by - must be listed as character variables. No
  default.

- naorder:

  Logical value for whether to put NA values at the end of the sorted
  data frame. Default value is TRUE and treats NA values as the largest
  values when sorting. A value of FALSE treats NA Values as the smallest
  values when sorting.

- sortID:

  Logical value that indicates whether the sort group information used
  to implement the sorting should be added to .data. By default, sort
  group is FALSE which means the sort group information is not added to
  .data. When TRUE, and there are k variables to sort by, a set of k-1
  sort group variables with names sortSerpj (j=1 to k-1) are added to
  .data.

## Value

A copy of .data that is hierarchically sorted and, if specified,
includes the sort group variables created for sorting

## Examples

``` r
# Sort counties by Region, then Division, in a serpentine pattern
county_2023 |>
  serp_sort("Region", "Division")
#> # A tidytable: 3,144 × 25
#>    GEOID Name    State Region Division Pop_Tot Pop_Pct_White_NH Pop_Pct_Black_NH
#>    <chr> <chr>   <chr> <fct>  <fct>      <dbl>            <dbl>            <dbl>
#>  1 09110 Capito… CT    North… New Eng…  969029             60.9            11.5 
#>  2 09120 Greate… CT    North… New Eng…  326296             53.4            11.1 
#>  3 09130 Lower … CT    North… New Eng…  174983             80.0             4.55
#>  4 09140 Naugat… CT    North… New Eng…  452303             61.1            10.3 
#>  5 09150 Northe… CT    North… New Eng…   95829             87.2             1.68
#>  6 09160 Northw… CT    North… New Eng…  112848             82.7             2.50
#>  7 09170 South … CT    North… New Eng…  566803             59.6            13.0 
#>  8 09180 Southe… CT    North… New Eng…  279025             71.3             4.96
#>  9 09190 Wester… CT    North… New Eng…  621232             59.7             9.80
#> 10 23001 Andros… ME    North… New Eng…  112323             88.2             4.61
#> # ℹ 3,134 more rows
#> # ℹ 17 more variables: Pop_Pct_AIAN_NH <dbl>, Pop_Pct_Asian_NH <dbl>,
#> #   Pop_Pct_NHPI_NH <dbl>, Pop_Pct_Other_NH <dbl>, Pop_Pct_Hispanic <dbl>,
#> #   HU_Tot <dbl>, HU_Pct_Occupied <dbl>, HU_Pct_Vacant <dbl>,
#> #   Pop_Pct_0004 <dbl>, Pop_Pct_0509 <dbl>, Pop_Pct_1014 <dbl>,
#> #   Pop_Pct_2544 <dbl>, Pop_Pct_4564 <dbl>, Pop_Pct_6574 <dbl>,
#> #   Pop_Pct_75plus <dbl>, Pop_Pct_1517 <dbl>, Pop_Pct_1824 <dbl>

# Keep the intermediate sort-group columns (sortSerp1, sortSerp2)
county_2023 |>
  serp_sort("Region", "Division", sortID = TRUE)
#> # A tidytable: 3,144 × 27
#>    GEOID Name    State Region Division Pop_Tot Pop_Pct_White_NH Pop_Pct_Black_NH
#>    <chr> <chr>   <chr> <fct>  <fct>      <dbl>            <dbl>            <dbl>
#>  1 09110 Capito… CT    North… New Eng…  969029             60.9            11.5 
#>  2 09120 Greate… CT    North… New Eng…  326296             53.4            11.1 
#>  3 09130 Lower … CT    North… New Eng…  174983             80.0             4.55
#>  4 09140 Naugat… CT    North… New Eng…  452303             61.1            10.3 
#>  5 09150 Northe… CT    North… New Eng…   95829             87.2             1.68
#>  6 09160 Northw… CT    North… New Eng…  112848             82.7             2.50
#>  7 09170 South … CT    North… New Eng…  566803             59.6            13.0 
#>  8 09180 Southe… CT    North… New Eng…  279025             71.3             4.96
#>  9 09190 Wester… CT    North… New Eng…  621232             59.7             9.80
#> 10 23001 Andros… ME    North… New Eng…  112323             88.2             4.61
#> # ℹ 3,134 more rows
#> # ℹ 19 more variables: Pop_Pct_AIAN_NH <dbl>, Pop_Pct_Asian_NH <dbl>,
#> #   Pop_Pct_NHPI_NH <dbl>, Pop_Pct_Other_NH <dbl>, Pop_Pct_Hispanic <dbl>,
#> #   HU_Tot <dbl>, HU_Pct_Occupied <dbl>, HU_Pct_Vacant <dbl>,
#> #   Pop_Pct_0004 <dbl>, Pop_Pct_0509 <dbl>, Pop_Pct_1014 <dbl>,
#> #   Pop_Pct_2544 <dbl>, Pop_Pct_4564 <dbl>, Pop_Pct_6574 <dbl>,
#> #   Pop_Pct_75plus <dbl>, Pop_Pct_1517 <dbl>, Pop_Pct_1824 <dbl>, …
```
