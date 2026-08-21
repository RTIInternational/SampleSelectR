# Sample selection function

Selects a random sample using a specified method and sample size.
Selection can also optionally be stratified and/or include a measure of
size (mos) if a PPS method is used.

## Usage

``` r
select_sample(
  frame,
  method,
  n,
  outall = FALSE,
  strata = NULL,
  mos = NULL,
  sort_vars = NULL,
  sort_method = NULL
)
```

## Arguments

- frame:

  A data frame, data.table, or tibble from which to draw the sample. No
  default.

- method:

  The desired sampling method. Valid options are "srs", "sys_eq",
  "sys_pps", and "chromy_pps". No default.

- n:

  The sample size to draw from the frame. If strata is NULL, must be a
  positive integer. If strata is not NULL, must be a data.frame, tibble,
  or data.table with columns for each stratification variable as the
  same type and variable names as the frame plus a column with the
  sample size (`sample_size`) which is a positive integer. No default.

- outall:

  A logical value indicating whether to return the entire frame with a
  selection indicator or just the sample. Default is FALSE.

- strata:

  A vector of characters with variable names of strata. Default is NULL.

- mos:

  A character string defining the variable name on the frame for the
  measure of size. If not NULL, must have method = c("sys_pps",
  "chromy_pps"). If NULL, must have method=c("srs", "sys"). Default is
  NULL.

- sort_vars:

  A vector of characters indicating the variables that should be used to
  sort the frame. If not NULL, cannot have method = "srs". Default is
  NULL.

- sort_method:

  A character string defining the method to implicitly sort the frame.
  Valid options are "serpentine" and "nest". Must coincide with
  sort_var; i.e., both must be NULL or both must be not NULL. Default is
  NULL.

## Value

A tidytable object containing the entire frame with a selection
indicator or just the sample, dependent on the value of outall.
Selection probability and sampling weight are also included. May include
various summary messages to the console when applicable for certain
sampling methods.

## Examples

``` r

# SRS of 100 US counties, using geographic region as strata
# n is a data frame containing the strata values and corresponding desired sample size
# Sample size column must be titled 'sample_size'

n_df_srs <- data.frame(
  Region = as.factor(c("Northeast", "Midwest", "South", "West")),
  sample_size = c(25, 25, 25, 25)
)

county_2023 |>
  select_sample(method = "srs", n = n_df_srs, strata = "Region")
#> Stratum: Region = South 
#> --Frame size: 1422
#> --Sample size: 25
#> Stratum: Region = West 
#> --Frame size: 449
#> --Sample size: 25
#> Stratum: Region = Northeast 
#> --Frame size: 218
#> --Sample size: 25
#> Stratum: Region = Midwest 
#> --Frame size: 1055
#> --Sample size: 25
#> # A tidytable: 100 × 27
#>    Region GEOID Name    State Division Pop_Tot Pop_Pct_White_NH Pop_Pct_Black_NH
#>    <fct>  <chr> <chr>   <chr> <fct>      <dbl>            <dbl>            <dbl>
#>  1 South  05043 Drew C… AR    West So…   17143             64.9           26.7  
#>  2 South  05097 Montgo… AR    West So…    8571             89.7            0.840
#>  3 South  12011 Browar… FL    South A… 1946127             31.9           27.6  
#>  4 South  12109 St. Jo… FL    South A…  292243             78.6            4.93 
#>  5 South  13071 Colqui… GA    South A…   45907             54.3           22.8  
#>  6 South  13093 Dooly … GA    South A…   11026             40.9           47.7  
#>  7 South  13127 Glynn … GA    South A…   84987             62.1           24.5  
#>  8 South  13259 Stewar… GA    South A…    4978             24.6           59.7  
#>  9 South  13273 Terrel… GA    South A…    8941             36.4           59.6  
#> 10 South  21055 Critte… KY    East So…    8972             94.3            0.513
#> # ℹ 90 more rows
#> # ℹ 19 more variables: Pop_Pct_AIAN_NH <dbl>, Pop_Pct_Asian_NH <dbl>,
#> #   Pop_Pct_NHPI_NH <dbl>, Pop_Pct_Other_NH <dbl>, Pop_Pct_Hispanic <dbl>,
#> #   HU_Tot <dbl>, HU_Pct_Occupied <dbl>, HU_Pct_Vacant <dbl>,
#> #   Pop_Pct_0004 <dbl>, Pop_Pct_0509 <dbl>, Pop_Pct_1014 <dbl>,
#> #   Pop_Pct_2544 <dbl>, Pop_Pct_4564 <dbl>, Pop_Pct_6574 <dbl>,
#> #   Pop_Pct_75plus <dbl>, Pop_Pct_1517 <dbl>, Pop_Pct_1824 <dbl>, …


# Systematic sample of 250 US universities. Each unit has an equal probability of being selected
# Includes a nested sort of enrollment total within sector
# Returns all obs from original data frame with a selection indicator column

sample_sys_eq <- ipeds |>
  select_sample(
    method = "sys_eq", n = 250, outall = TRUE,
    sort_vars = c("SECTOR", "ENRTOT"), sort_method = "nest"
  )
#> Frame size: 5914
#> Sample size: 250
#> Sampling interval (k): 23.656
#> Random start (r): 4.707398

# For samples taken with outall = TRUE, the sample size can be verified by summing
# the SelectionIndicator column.

sample_sys_eq
#> # A tidytable: 5,914 × 19
#>    UNITID INSTNM      STABBR  FIPS OBEREG ICLEVEL SECTOR LOCALE DEGGRANT HLOFFER
#>     <dbl> <chr>       <chr>  <dbl> <fct>  <fct>   <fct>  <fct>  <fct>    <fct>  
#>  1 180203 Aaniiih Na… MT        30 Rocky… Four o… Publi… Rural… Degree-… Bachel…
#>  2 491297 University… WI        55 Great… Four o… Publi… Rural… Degree-… Bachel…
#>  3 200086 Nueta Hida… ND        38 Plain… Four o… Publi… Rural… Degree-… Bachel…
#>  4 219408 Sisseton W… SD        46 Plain… Four o… Publi… Rural… Degree-… Bachel…
#>  5 260372 Lac Courte… WI        55 Great… Four o… Publi… Rural… Degree-… Master…
#>  6 214607 Pennsylvan… PA        42 Mid E… Four o… Publi… Subur… Degree-… Master…
#>  7 366340 Stone Chil… MT        30 Rocky… Four o… Publi… Rural… Degree-… Bachel…
#>  8 200466 Sitting Bu… ND        38 Plain… Four o… Publi… Rural… Degree-… Master…
#>  9 434584 Ilisagvik … AK         2 Far W… Four o… Publi… Rural… Degree-… Bachel…
#> 10 243188 University… PR        72 Other… Four o… Publi… Rural… Degree-… Bachel…
#> # ℹ 5,904 more rows
#> # ℹ 9 more variables: ENRTOT <dbl>, EFUG <dbl>, EFUG1ST <dbl>, EFUGFT <dbl>,
#> #   EFGRAD <dbl>, EFGRADFT <dbl>, SelectionIndicator <lgl>,
#> #   SelectionProbability <dbl>, SamplingWeight <dbl>
sum(sample_sys_eq$SelectionIndicator)
#> [1] 250


# Systematic PPS sample of 250 US universities. Each unit's probability of selection
# is proportional to its size measure.
# Using enrollment total as MOS
# Includes a nested sort of enrollment total within sector

sample_sys_pps <- ipeds |>
  select_sample(
    method = "sys_pps", n = 250, mos = "ENRTOT",
    sort_vars = c("SECTOR", "ENRTOT"), sort_method = "nest"
  )
#> Frame size: 5914
#> Sample size: 250
#> Sampling interval (k): 78812
#> Random start (r): 2887.813

# For pps samples, it is possible for a single sampling unit to be selected multiple times
# due to a large mos value. This is especially true as desired sample size increases. The
# result is the final sample may not meet the desired sample size. To verify the pps sample,
# the NumberHits column can be summed and should total to the desired sample size.

sample_sys_pps
#> # A tidytable: 246 × 19
#>    UNITID INSTNM      STABBR  FIPS OBEREG ICLEVEL SECTOR LOCALE DEGGRANT HLOFFER
#>     <dbl> <chr>       <chr>  <dbl> <fct>  <fct>   <fct>  <fct>  <fct>    <fct>  
#>  1 241766 Puerto Ric… PR        72 Other… Four o… Publi… City:… Degree-… Master…
#>  2 180692 The Univer… MT        30 Rocky… Four o… Publi… Town:… Degree-… Postba…
#>  3 201432 Bowling Gr… OH        39 Great… Four o… Publi… Rural… Degree-… Bachel…
#>  4 204006 Miami Univ… OH        39 Great… Four o… Publi… Subur… Degree-… Master…
#>  5 243115 University… PR        72 Other… Four o… Publi… City:… Degree-… Master…
#>  6 161873 University… MD        24 Mid E… Four o… Publi… City:… Degree-… Doctor…
#>  7 237039 Whatcom Co… WA        53 Far W… Four o… Publi… City:… Degree-… Bachel…
#>  8 190646 CUNY Medga… NY        36 Mid E… Four o… Publi… City:… Degree-… Bachel…
#>  9 223506 Brazosport… TX        48 South… Four o… Publi… Subur… Degree-… Bachel…
#> 10 168263 Westfield … MA        25 New E… Four o… Publi… Subur… Degree-… Post-m…
#> # ℹ 236 more rows
#> # ℹ 9 more variables: ENRTOT <dbl>, EFUG <dbl>, EFUG1ST <dbl>, EFUGFT <dbl>,
#> #   EFGRAD <dbl>, EFGRADFT <dbl>, SamplingWeight <dbl>, NumberHits <int>,
#> #   ExpectedHits <dbl>
sum(sample_sys_pps$NumberHits)
#> [1] 250


# Sequential aka Chromy's method PPS sample of 500 PUMAs, using geographic region as strata
# Includes a serpentine sort of geographic division then state
# Using population total as MOS, each unit's probability of selection is proportional to its
# size measure.
# Note that there may be a discrepancy between the desired and final sample sizes. The final
# sample size can be verified by totaling NumberHits.

n_df_chr <- data.frame(
  Region = as.factor(c("Northeast", "Midwest", "South", "West")),
  sample_size = c(125, 125, 125, 125)
)

puma_2023 |>
  select_sample(
    method = "chromy_pps", n = n_df_chr, strata = "Region", mos = "Pop_Tot",
    sort_vars = c("Division", "State"), sort_method = "serpentine"
  )
#> Stratum: Region = South 
#> --Frame size: 952
#> --Sample size: 125
#> Stratum: Region = West 
#> --Frame size: 581
#> --Sample size: 125
#> Stratum: Region = Northeast 
#> --Frame size: 423
#> --Sample size: 125
#> Stratum: Region = Midwest 
#> --Frame size: 506
#> --Sample size: 125
#> # A tidytable: 500 × 28
#>    Region GEOID   Name  State Division Pop_Tot Pop_Pct_White_NH Pop_Pct_Black_NH
#>    <fct>  <chr>   <chr> <chr> <fct>      <dbl>            <dbl>            <dbl>
#>  1 South  1100102 Dist… DC    South A…  110612            22.5             50.8 
#>  2 South  1000200 Kent… DE    South A…  185043            57.6             26.4 
#>  3 South  1200903 Brev… FL    South A…  135228            73.9              5.37
#>  4 South  1201107 Brow… FL    South A…  106384             7.70            78.3 
#>  5 South  1201110 Brow… FL    South A…  115820            41.3              8.01
#>  6 South  1201500 Char… FL    South A…  195083            82.4              5.10
#>  7 South  1203105 Duva… FL    South A…  145113            65.5              9.45
#>  8 South  1204798 Hami… FL    South A…  105499            67.1             19.6 
#>  9 South  1205701 Hill… FL    South A…  153618            25.2             37.1 
#> 10 South  1205710 Hill… FL    South A…  123729            68.7              5.90
#> # ℹ 490 more rows
#> # ℹ 20 more variables: Pop_Pct_AIAN_NH <dbl>, Pop_Pct_Asian_NH <dbl>,
#> #   Pop_Pct_NHPI_NH <dbl>, Pop_Pct_Other_NH <dbl>, Pop_Pct_Hispanic <dbl>,
#> #   HU_Tot <dbl>, HU_Pct_Occupied <dbl>, HU_Pct_Vacant <dbl>,
#> #   Pop_Pct_0004 <dbl>, Pop_Pct_0509 <dbl>, Pop_Pct_1014 <dbl>,
#> #   Pop_Pct_2544 <dbl>, Pop_Pct_4564 <dbl>, Pop_Pct_6574 <dbl>,
#> #   Pop_Pct_75plus <dbl>, Pop_Pct_1517 <dbl>, Pop_Pct_1824 <dbl>, …
```
