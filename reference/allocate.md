# Sample allocation

Compute the proportional, power, Neyman, and optimal sample allocations.

## Usage

``` r
allocate(
  allocation,
  N.h,
  n.samp = NULL,
  S.h = NULL,
  c.h = NULL,
  cost = NULL,
  variance = NULL,
  power = NULL,
  lbound = 2
)
```

## Arguments

- allocation:

  type of allocation, must be one of `"proportional"`, `"power"`,
  `"neyman"`, or `"optimal"`.

- N.h:

  vector of population stratum sizes (\\N_h\\, all positive values), for
  example `c(150, 600, 250)`.  
    
  required for all allocation types.

- n.samp:

  total sample size to be allocated (positive integer of length 1).  
    
  required for the following allocation types: proportional, power, and
  Neyman, and `NULL` otherwise.

- S.h:

  vector of stratum unit standard deviations (positive values same
  length as `N.h`) (\\S_h\\).  
    
  required for the following allocation types: Neyman, and optimal, and
  `NULL` otherwise.

- c.h:

  vector of cost per unit in stratum h (positive values same length as
  `N.h`) (\\c_h\\).  
    
  required for the optimal allocation only, and `NULL` otherwise.

- cost:

  total variable cost (positive value) \\(C – c_0)\\.  
    
  required for the cost-constrained optimal allocation only, and `NULL`
  otherwise.

- variance:

  fixed variance target for estimated mean (positive value) (\\V_0\\).  
    
  required for the precision-constrained optimal allocation only, and
  `NULL` otherwise.

- power:

  power value for power allocation (\\0 \le \alpha \le 1\\).  
    
  required for the power allocation only, and `NULL` otherwise.

- lbound:

  minimum stratum-level (positive integer of length 1). Default value is
  2.

## Value

Integer vector same length of N.h final allocation

## Method

The *allocate* function allocates a sample size *n* on *H* strata using
one of the following allocation methods:

1.  Proportional allocation
    \[`n.samp, N.h, allocation = "proportional"`\] \$\$n_h = n \times
    \frac{N_h}{\sum\limits\_{h=1}^H N_h}\$\$ where  
    \\n\\: total sample size to be allocated (function input is
    `n.samp`), and  
    \\N_h\\: population size of stratum *h* (function input is `N.h`).

2.  Power allocation \[`n.samp, N.h, power, allocation = "power"`\]
    \$\$n_h = n \times \frac{N_h^\alpha}{\sum\limits\_{h=1}^H
    N_h^\alpha}\$\$ where  
    \\\alpha\\: a power value to control over-under-sampling with \\0
    \le \alpha \le 1\\ (function input is `power`).

3.  Neyman allocation \[`n.samp, N.h, S.h, allocation = "neyman"`\]
    \$\$n_h = n \times \frac{N_h S_h}{\sum\limits\_{h=1}^H N_h S_h}\$\$
    where  
    \\S_h\\: standard deviation of stratum *h* (function input is
    `S.h`).

4.  Optimal allocation

    - cost-constrained \[`N.h, S.h, c.h, cost, allocation = "optimal"`\]
      \$\$n_h = (C−c_0) \times \frac{N_h S_h /
      \sqrt{c_h}}{\sum\limits\_{h=1}^H N_h S_h \sqrt{c_h}}\$\$ where  
      \\c_h\\: cost per unit in stratum *h* (function input is `c.h`),
      and  
      \\(C – c_0)\\: total variable cost (function input is `cost`)

    - precision-constrained
      \[`N.h, S.h, c.h, variance, allocation = "optimal"`\] \$\$n_h =
      N_h S_h / \sqrt{c_h} \times \frac{\sum\limits\_{h=1}^H N_h S_h
      \sqrt{c_h}}{V_0 \left(\sum\limits\_{h=1}^H N_h \right)^2 +
      \sum\limits\_{h=1}^H N_h S_h^2}\$\$ where  
      \\V_0\\: fixed variance target for estimated mean (function input
      is `variance`)

The table below presents the relevant inputs for each type; when
irrelevant inputs are entered, an error message will be displayed.

|                                |         |            |         |         |          |              |            |           |
|--------------------------------|---------|------------|---------|---------|----------|--------------|------------|-----------|
| **allocation**                 | **N.h** | **n.samp** | **S.h** | **c.h** | **cost** | **variance** | **lbound** | **power** |
| proportional                   | ✓       | ✓          |         |         |          |              | ✓          |           |
| power                          | ✓       | ✓          |         |         |          |              | ✓          | ✓         |
| neyman                         | ✓       | ✓          | ✓       | ✓       |          |              | ✓          |           |
| optimal: cost-constrained      | ✓       |            | ✓       | ✓       | ✓        |              | ✓          |           |
| optimal: precision-constrained | ✓       |            | ✓       | ✓       |          | ✓            | ✓          |           |

## Examples

``` r
# The first step is getting a frame summary
#  Summarize the IPEDS dataset by OBEREG
# - N: number of universities per region
# - SD_ENRTOT: standard deviation of total enrollment per region
# - Filter out rows with missing ENRTOT to ensure accurate variance estimates

ipeds_summary <- ipeds |>
  tidytable::filter(!is.na(ENRTOT)) |>
  tidytable::group_by(OBEREG) |>
  tidytable::summarize(
    N = tidytable::n(),
    SD_ENRTOT = stats::sd(ENRTOT)
  ) |>
  tidytable::ungroup()

# Example of proportional allocation
ipeds_summary |>
  tidytable::mutate(
    n = allocate("proportional", N.h = N, n.samp = 500)
  )
#> Sample allocation of 500 using proportional with the relevant inputs:
#>   N.h = 7, 299, 971, 851, 468, 1467, 633, 216, 870, 132
#> 
#> Output:
#> 2, 25, 82, 72, 40, 124, 53, 18, 73, 11
#> # A tidytable: 10 × 4
#>    OBEREG                                                      N SD_ENRTOT     n
#>    <fct>                                                   <int>     <dbl> <int>
#>  1 U.S. Service schools                                        7     1680.     2
#>  2 New England (CT, ME, MA, NH, RI, VT)                      299    11800.    25
#>  3 Mid East (DE, DC, MD, NJ, NY, PA)                         971     5956.    82
#>  4 Great Lakes (IL, IN, MI, OH, WI)                          851     7537.    72
#>  5 Plains (IA, KS, MN, MO, NE, ND, SD)                       468     5830.    40
#>  6 Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA,…  1467     7293.   124
#>  7 Southwest (AZ, NM, OK, TX)                                633    11149.    53
#>  8 Rocky Mountains (CO, ID, MT, UT, WY)                      216    14784.    18
#>  9 Far West (AK, CA, HI, NV, OR, WA)                         870     7641.    73
#> 10 Other U.S. jurisdictions (AS, FM, GU, MH, MP, PR, PW, …   132     2981.    11

# Example of power allocation
ipeds_summary |>
  tidytable::mutate(
    n = allocate("power", N.h = N, power = 0.5, n.samp = 500)
  )
#> Sample allocation of 500 using power with the relevant inputs:
#>   N.h = 7, 299, 971, 851, 468, 1467, 633, 216, 870, 132
#>   power = 0.5
#> 
#> Output:
#> 6, 39, 70, 66, 49, 87, 57, 33, 67, 26
#> # A tidytable: 10 × 4
#>    OBEREG                                                      N SD_ENRTOT     n
#>    <fct>                                                   <int>     <dbl> <int>
#>  1 U.S. Service schools                                        7     1680.     6
#>  2 New England (CT, ME, MA, NH, RI, VT)                      299    11800.    39
#>  3 Mid East (DE, DC, MD, NJ, NY, PA)                         971     5956.    70
#>  4 Great Lakes (IL, IN, MI, OH, WI)                          851     7537.    66
#>  5 Plains (IA, KS, MN, MO, NE, ND, SD)                       468     5830.    49
#>  6 Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA,…  1467     7293.    87
#>  7 Southwest (AZ, NM, OK, TX)                                633    11149.    57
#>  8 Rocky Mountains (CO, ID, MT, UT, WY)                      216    14784.    33
#>  9 Far West (AK, CA, HI, NV, OR, WA)                         870     7641.    67
#> 10 Other U.S. jurisdictions (AS, FM, GU, MH, MP, PR, PW, …   132     2981.    26

# Example of Neyman allocation
ipeds_summary |>
  tidytable::mutate(
    n = allocate("neyman", N.h = N, n.samp = 500, S.h = SD_ENRTOT)
  )
#> Sample allocation of 500 using neyman with the relevant inputs:
#>   N.h = 7, 299, 971, 851, 468, 1467, 633, 216, 870, 132
#>   S.h = 1680.11385668608, 11800.2993217881, 5956.31998862919, 7536.59885863143, 5830.00927558341, 7293.44943657165, 11149.0070418081, 14783.8369003426, 7641.36426053871, 2981.44889034106
#> 
#> Output:
#> 2, 38, 62, 69, 29, 115, 76, 34, 71, 4
#> # A tidytable: 10 × 4
#>    OBEREG                                                      N SD_ENRTOT     n
#>    <fct>                                                   <int>     <dbl> <int>
#>  1 U.S. Service schools                                        7     1680.     2
#>  2 New England (CT, ME, MA, NH, RI, VT)                      299    11800.    38
#>  3 Mid East (DE, DC, MD, NJ, NY, PA)                         971     5956.    62
#>  4 Great Lakes (IL, IN, MI, OH, WI)                          851     7537.    69
#>  5 Plains (IA, KS, MN, MO, NE, ND, SD)                       468     5830.    29
#>  6 Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA,…  1467     7293.   115
#>  7 Southwest (AZ, NM, OK, TX)                                633    11149.    76
#>  8 Rocky Mountains (CO, ID, MT, UT, WY)                      216    14784.    34
#>  9 Far West (AK, CA, HI, NV, OR, WA)                         870     7641.    71
#> 10 Other U.S. jurisdictions (AS, FM, GU, MH, MP, PR, PW, …   132     2981.     4

# Example of Neyman allocation with a lower bound of 5
ipeds_summary |>
  tidytable::mutate(
    n = allocate("neyman", N.h = N, n.samp = 500, S.h = SD_ENRTOT, lbound = 5)
  )
#> Sample allocation of 500 using neyman with the relevant inputs:
#>   N.h = 7, 299, 971, 851, 468, 1467, 633, 216, 870, 132
#>   S.h = 1680.11385668608, 11800.2993217881, 5956.31998862919, 7536.59885863143, 5830.00927558341, 7293.44943657165, 11149.0070418081, 14783.8369003426, 7641.36426053871, 2981.44889034106
#> 
#> Output:
#> 5, 38, 62, 68, 29, 113, 75, 34, 71, 5
#> # A tidytable: 10 × 4
#>    OBEREG                                                      N SD_ENRTOT     n
#>    <fct>                                                   <int>     <dbl> <int>
#>  1 U.S. Service schools                                        7     1680.     5
#>  2 New England (CT, ME, MA, NH, RI, VT)                      299    11800.    38
#>  3 Mid East (DE, DC, MD, NJ, NY, PA)                         971     5956.    62
#>  4 Great Lakes (IL, IN, MI, OH, WI)                          851     7537.    68
#>  5 Plains (IA, KS, MN, MO, NE, ND, SD)                       468     5830.    29
#>  6 Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA,…  1467     7293.   113
#>  7 Southwest (AZ, NM, OK, TX)                                633    11149.    75
#>  8 Rocky Mountains (CO, ID, MT, UT, WY)                      216    14784.    34
#>  9 Far West (AK, CA, HI, NV, OR, WA)                         870     7641.    71
#> 10 Other U.S. jurisdictions (AS, FM, GU, MH, MP, PR, PW, …   132     2981.     5
```
