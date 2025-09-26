
# SampleSelectR

<!-- badges: start -->

[![R-CMD-check](https://github.com/rti-international/SampleSelectR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rti-international/SampleSelectR/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/RTIInternational/SampleSelectR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RTIInternational/SampleSelectR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

SampleSelectR is an R package developed by RTI International to support
the design and implementation of common survey sampling methods. It is
designed to make sample design and selection reproducible, efficient,
and transparent for survey statisticians and researchers.

Currently supported sampling methods include:

- Simple Random Sampling (SRS)

- Systematic Sampling (sys)

- Systematic Probability Proportional to Size (PPS) Sampling

- Sequential PPS Sampling (i.e., Chromy’s method)

Allocation methods include:

- Proportional

- Power

- Neyman

- Optimal

## Installation

You can install the development version of SampleSelectR from
[GitHub](https://github.com/rti-international/SampleSelectR) with either
the `pak` or `devtools` package:

``` r
# install.packages("pak")
pak::pak("RTIInternational/SampleSelectR")

# install.packages("devtools")
devtools::install_github("RTIInternational/SampleSelectR")
```

Then, you can load the package.

``` r
library(SampleSelectR)
```

## Included Data

SampleSelectR comes with three example datasets for illustration of
realistic sample frames including:

- `ipeds` which contains a list of postsecondary educational
  institutions in the United States from the Integrated Postsecondary
  Education Data System (IPEDS) with a select set of characteristics for
  operating institutions in Fall 2023[^1]
- `county_2023` which contains a list of the counties in the United
  States based on data from the 2019-2023 American Community Survey with
  a select set of characteristics[^2]
- `puma_2023` which contains a list of the Public Use Microdata Areas
  (PUMAs) in the United States based on data from the 2019-2023 American
  Community Survey with a select set of characteristics[^3]

``` r
summary(ipeds)
#>      UNITID          INSTNM             STABBR               FIPS      
#>  Min.   :100654   Length:5914        Length:5914        Min.   : 1.00  
#>  1st Qu.:169277   Class :character   Class :character   1st Qu.:13.00  
#>  Median :219493   Mode  :character   Mode  :character   Median :29.00  
#>  Mean   :284664                                         Mean   :29.19  
#>  3rd Qu.:446954                                         3rd Qu.:42.00  
#>  Max.   :499723                                         Max.   :78.00  
#>                                                                        
#>                                                         OBEREG    
#>  Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV):1467  
#>  Mid East (DE, DC, MD, NJ, NY, PA)                         : 971  
#>  Far West (AK, CA, HI, NV, OR, WA)                         : 870  
#>  Great Lakes (IL, IN, MI, OH, WI)                          : 851  
#>  Southwest (AZ, NM, OK, TX)                                : 633  
#>  Plains (IA, KS, MN, MO, NE, ND, SD)                       : 468  
#>  (Other)                                                   : 654  
#>                                 ICLEVEL    
#>  Four or more years                 :2747  
#>  At least 2 but less than 4 years   :1512  
#>  Less than 2 years (below associate):1655  
#>                                            
#>                                            
#>                                            
#>                                            
#>                                      SECTOR               LOCALE    
#>  Private not-for-profit, 4-year or above:1608   Suburb: Large:1442  
#>  Private for-profit, less-than 2-year   :1373   City: Large  :1392  
#>  Public, 2-year                         : 868   City: Small  : 778  
#>  Public, 4-year or above                : 820   City: Midsize: 713  
#>  Private for-profit, 2-year             : 517   Town: Distant: 396  
#>  Private for-profit, 4-year or above    : 319   Rural: Fringe: 298  
#>  (Other)                                : 409   (Other)      : 895  
#>                                         DEGGRANT   
#>  Degree-granting                            :3995  
#>  Nondegree-granting, primarily postsecondary:1919  
#>                                                    
#>                                                    
#>                                                    
#>                                                    
#>                                                    
#>                                      HLOFFER         ENRTOT      
#>  At least 1, but less than 2 academic yrs:1514   Min.   :     1  
#>  Doctor's degree                         :1232   1st Qu.:   127  
#>  Associate's degree                      :1022   Median :   588  
#>  Bachelor's degree                       : 677   Mean   :  3332  
#>  Master's degree                         : 672   3rd Qu.:  2849  
#>  At least 2, but less than 4 academic yrs: 490   Max.   :185015  
#>  (Other)                                 : 307                   
#>       EFUG              EFUG1ST          EFUGFT           EFGRAD       
#>  Min.   :     0.00   Min.   :    0   Min.   :     0   Min.   :    0.0  
#>  1st Qu.:    98.25   1st Qu.:   21   1st Qu.:    71   1st Qu.:    0.0  
#>  Median :   472.00   Median :  105   Median :   321   Median :    0.0  
#>  Mean   :  2777.88   Mean   :  508   Mean   :  1706   Mean   :  553.7  
#>  3rd Qu.:  2455.25   3rd Qu.:  478   3rd Qu.:  1426   3rd Qu.:  120.0  
#>  Max.   :159653.00   Max.   :15343   Max.   :135822   Max.   :50245.0  
#>                                                                        
#>     EFGRADFT       
#>  Min.   :    0.00  
#>  1st Qu.:    0.00  
#>  Median :    0.00  
#>  Mean   :  328.37  
#>  3rd Qu.:   56.75  
#>  Max.   :49193.00  
#> 

summary(county_2023)
#>     GEOID               Name              State                 Region    
#>  Length:3144        Length:3144        Length:3144        Northeast: 218  
#>  Class :character   Class :character   Class :character   Midwest  :1055  
#>  Mode  :character   Mode  :character   Mode  :character   South    :1422  
#>                                                           West     : 449  
#>                                                                           
#>                                                                           
#>                                                                           
#>                Division      Pop_Tot        Pop_Pct_White_NH  Pop_Pct_Black_NH 
#>  West North Central:618   Min.   :     43   Min.   :  2.153   Min.   : 0.0000  
#>  South Atlantic    :588   1st Qu.:  10794   1st Qu.: 62.608   1st Qu.: 0.6574  
#>  West South Central:470   Median :  25813   Median : 81.103   Median : 2.0825  
#>  East North Central:437   Mean   : 105721   Mean   : 74.298   Mean   : 8.6559  
#>  East South Central:364   3rd Qu.:  68379   3rd Qu.: 90.618   3rd Qu.: 9.5543  
#>  Mountain          :281   Max.   :9848406   Max.   :100.000   Max.   :86.0575  
#>  (Other)           :386                                                        
#>  Pop_Pct_AIAN_NH   Pop_Pct_Asian_NH  Pop_Pct_NHPI_NH     Pop_Pct_Other_NH
#>  Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.000000   Min.   : 0.000  
#>  1st Qu.: 0.0618   1st Qu.: 0.2780   1st Qu.: 0.000000   1st Qu.: 2.531  
#>  Median : 0.1546   Median : 0.6098   Median : 0.005365   Median : 3.478  
#>  Mean   : 1.6596   Mean   : 1.4257   Mean   : 0.096733   Mean   : 3.798  
#>  3rd Qu.: 0.4637   3rd Qu.: 1.3405   3rd Qu.: 0.059577   3rd Qu.: 4.495  
#>  Max.   :91.3520   Max.   :41.9978   Max.   :18.604651   Max.   :25.562  
#>                                                                          
#>  Pop_Pct_Hispanic     HU_Tot        HU_Pct_Occupied HU_Pct_Vacant   
#>  Min.   : 0.000   Min.   :     47   Min.   :22.68   Min.   : 1.707  
#>  1st Qu.: 2.533   1st Qu.:   5240   1st Qu.:78.61   1st Qu.: 9.393  
#>  Median : 4.881   Median :  12374   Median :85.28   Median :14.721  
#>  Mean   :10.066   Mean   :  45271   Mean   :83.10   Mean   :16.904  
#>  3rd Qu.:10.809   3rd Qu.:  31710   3rd Qu.:90.61   3rd Qu.:21.388  
#>  Max.   :97.306   Max.   :3624084   Max.   :98.29   Max.   :77.316  
#>                                                                     
#>   Pop_Pct_0004     Pop_Pct_0509     Pop_Pct_1014     Pop_Pct_2544  
#>  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
#>  1st Qu.: 4.900   1st Qu.: 5.200   1st Qu.: 5.700   1st Qu.:21.90  
#>  Median : 5.500   Median : 6.000   Median : 6.400   Median :23.60  
#>  Mean   : 5.529   Mean   : 5.997   Mean   : 6.455   Mean   :23.82  
#>  3rd Qu.: 6.200   3rd Qu.: 6.700   3rd Qu.: 7.200   3rd Qu.:25.60  
#>  Max.   :20.600   Max.   :21.000   Max.   :13.300   Max.   :58.20  
#>                                                                    
#>   Pop_Pct_4564    Pop_Pct_6574   Pop_Pct_75plus    Pop_Pct_1517   
#>  Min.   : 8.90   Min.   : 2.10   Min.   : 0.700   Min.   : 0.000  
#>  1st Qu.:24.40   1st Qu.:10.10   1st Qu.: 6.700   1st Qu.: 3.600  
#>  Median :26.00   Median :11.60   Median : 8.000   Median : 4.000  
#>  Mean   :25.75   Mean   :11.87   Mean   : 8.182   Mean   : 3.955  
#>  3rd Qu.:27.40   3rd Qu.:13.10   3rd Qu.: 9.400   3rd Qu.: 4.300  
#>  Max.   :46.90   Max.   :38.50   Max.   :46.300   Max.   :17.900  
#>                                                                   
#>   Pop_Pct_1824   
#>  Min.   : 0.000  
#>  1st Qu.: 6.900  
#>  Median : 7.800  
#>  Mean   : 8.453  
#>  3rd Qu.: 8.900  
#>  Max.   :49.100  
#> 

summary(puma_2023)
#>     GEOID               Name              State                 Region   
#>  Length:2462        Length:2462        Length:2462        Northeast:423  
#>  Class :character   Class :character   Class :character   Midwest  :506  
#>  Mode  :character   Mode  :character   Mode  :character   South    :952  
#>                                                           West     :581  
#>                                                                          
#>                                                                          
#>                                                                          
#>                Division      Pop_Tot       Pop_Pct_White_NH  Pop_Pct_Black_NH  
#>  South Atlantic    :496   Min.   : 94457   Min.   : 0.6772   Min.   : 0.06043  
#>  Pacific           :392   1st Qu.:114269   1st Qu.:41.1324   1st Qu.: 2.25999  
#>  East North Central:340   Median :128683   Median :62.3550   Median : 5.82201  
#>  Middle Atlantic   :313   Mean   :135007   Mean   :58.1157   Mean   :12.06644  
#>  West South Central:303   3rd Qu.:151797   3rd Qu.:78.8115   3rd Qu.:15.45296  
#>  Mountain          :189   Max.   :254693   Max.   :95.5992   Max.   :91.54119  
#>  (Other)           :429                                                        
#>  Pop_Pct_AIAN_NH    Pop_Pct_Asian_NH   Pop_Pct_NHPI_NH    Pop_Pct_Other_NH  
#>  Min.   : 0.00000   Min.   : 0.03401   Min.   : 0.00000   Min.   : 0.09704  
#>  1st Qu.: 0.06733   1st Qu.: 1.11991   1st Qu.: 0.00978   1st Qu.: 3.22126  
#>  Median : 0.12409   Median : 2.67222   Median : 0.03879   Median : 4.07446  
#>  Mean   : 0.54049   Mean   : 5.75511   Mean   : 0.16905   Mean   : 4.35801  
#>  3rd Qu.: 0.25500   3rd Qu.: 6.54233   3rd Qu.: 0.10762   3rd Qu.: 5.11640  
#>  Max.   :71.05525   Max.   :69.44064   Max.   :23.33594   Max.   :25.66375  
#>                                                                             
#>  Pop_Pct_Hispanic      HU_Tot       HU_Pct_Occupied HU_Pct_Vacant   
#>  Min.   : 0.7159   Min.   : 25782   Min.   :46.59   Min.   : 1.123  
#>  1st Qu.: 5.6869   1st Qu.: 47813   1st Qu.:87.58   1st Qu.: 4.995  
#>  Median :11.3890   Median : 55208   Median :92.36   Median : 7.638  
#>  Mean   :18.9952   Mean   : 57812   Mean   :90.36   Mean   : 9.639  
#>  3rd Qu.:24.5716   3rd Qu.: 65673   3rd Qu.:95.00   3rd Qu.:12.419  
#>  Max.   :96.0544   Max.   :140746   Max.   :98.88   Max.   :53.406  
#>                                                                     
#>   Pop_Pct_0004    Pop_Pct_0509     Pop_Pct_1014    Pop_Pct_2544  
#>  Min.   : 1.80   Min.   : 1.400   Min.   : 1.30   Min.   :11.70  
#>  1st Qu.: 5.00   1st Qu.: 5.400   1st Qu.: 5.80   1st Qu.:23.90  
#>  Median : 5.60   Median : 6.000   Median : 6.50   Median :26.10  
#>  Mean   : 5.69   Mean   : 6.035   Mean   : 6.47   Mean   :26.76  
#>  3rd Qu.: 6.30   3rd Qu.: 6.700   3rd Qu.: 7.20   3rd Qu.:28.70  
#>  Max.   :16.70   Max.   :14.400   Max.   :12.50   Max.   :55.40  
#>                                                                  
#>   Pop_Pct_4564    Pop_Pct_6574   Pop_Pct_75plus    Pop_Pct_1517 
#>  Min.   : 9.60   Min.   : 3.60   Min.   : 1.700   Min.   :0.60  
#>  1st Qu.:23.50   1st Qu.: 8.30   1st Qu.: 5.300   1st Qu.:3.50  
#>  Median :25.40   Median : 9.90   Median : 6.700   Median :3.90  
#>  Mean   :25.14   Mean   :10.03   Mean   : 6.837   Mean   :3.92  
#>  3rd Qu.:27.00   3rd Qu.:11.40   3rd Qu.: 8.100   3rd Qu.:4.30  
#>  Max.   :34.00   Max.   :30.80   Max.   :27.000   Max.   :6.70  
#>                                                                 
#>   Pop_Pct_1824   
#>  Min.   : 2.900  
#>  1st Qu.: 7.400  
#>  Median : 8.300  
#>  Mean   : 9.118  
#>  3rd Qu.: 9.700  
#>  Max.   :39.900  
#> 
```

## Example Workflow

The following are some examples of allocation and sampling on the IPEDS
data. The primary functions in workflows are `allocate()` to allocate
sample sizes though you can get a sample allocation in another manner,
if you prefer and `select_sample()` where you specify your frame, sample
sizes, sampling method, strata (if relevant), measure of size (if
relevant), and sort method (if relevant).

### Allocation

First, you must have a summary of your frame. In this case, we will be
specifying a sample size of 500 to be proportionally allocated across
region.

``` r
set.seed(8675309)

ipeds_summary <- ipeds |>
  tidytable::summarize(
    N = tidytable::n(),
    .by = "OBEREG"
  ) |>
  tidytable::ungroup()

ipeds_summary
#> # A tidytable: 10 × 2
#>    OBEREG                                                         N
#>    <fct>                                                      <int>
#>  1 U.S. Service schools                                           7
#>  2 New England (CT, ME, MA, NH, RI, VT)                         299
#>  3 Mid East (DE, DC, MD, NJ, NY, PA)                            971
#>  4 Great Lakes (IL, IN, MI, OH, WI)                             851
#>  5 Plains (IA, KS, MN, MO, NE, ND, SD)                          468
#>  6 Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)  1467
#>  7 Southwest (AZ, NM, OK, TX)                                   633
#>  8 Rocky Mountains (CO, ID, MT, UT, WY)                         216
#>  9 Far West (AK, CA, HI, NV, OR, WA)                            870
#> 10 Other U.S. jurisdictions (AS, FM, GU, MH, MP, PR, PW, VI)    132

ipeds_alloc <- ipeds_summary |>
  tidytable::mutate(
    sample_size = allocate("proportional", N.h = N, n.samp = 500)
  )
#> Sample allocation of 500 using proportional with the relevant inputs:
#>   N.h = 7, 299, 971, 851, 468, 1467, 633, 216, 870, 132
#> 
#> Output:
#> 2, 25, 82, 73, 39, 124, 53, 18, 73, 11

ipeds_alloc
#> # A tidytable: 10 × 3
#>    OBEREG                                                         N sample_size
#>    <fct>                                                      <int>       <int>
#>  1 U.S. Service schools                                           7           2
#>  2 New England (CT, ME, MA, NH, RI, VT)                         299          25
#>  3 Mid East (DE, DC, MD, NJ, NY, PA)                            971          82
#>  4 Great Lakes (IL, IN, MI, OH, WI)                             851          73
#>  5 Plains (IA, KS, MN, MO, NE, ND, SD)                          468          39
#>  6 Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)  1467         124
#>  7 Southwest (AZ, NM, OK, TX)                                   633          53
#>  8 Rocky Mountains (CO, ID, MT, UT, WY)                         216          18
#>  9 Far West (AK, CA, HI, NV, OR, WA)                            870          73
#> 10 Other U.S. jurisdictions (AS, FM, GU, MH, MP, PR, PW, VI)    132          11
```

### Sampling

Then, we are able to select a stratified sample and demonstrate this
using a SRS and systematic PPS. With the systematic and sequential
sampling methods, you can specify sort variables and the sort method.
Often, you want to be able to re-run sampling code and get the same
sample. To do this, we also set a seed.

``` r
ipeds_srs <- ipeds |>
  select_sample(
    method = "srs",
    n = tidytable::select(ipeds_alloc, OBEREG, sample_size),
    strata = "OBEREG"
  )
#> Stratum: OBEREG = Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV) 
#> --Frame size: 1467
#> --Sample size: 124
#> Stratum: OBEREG = Far West (AK, CA, HI, NV, OR, WA) 
#> --Frame size: 870
#> --Sample size: 73
#> Stratum: OBEREG = Southwest (AZ, NM, OK, TX) 
#> --Frame size: 633
#> --Sample size: 53
#> Stratum: OBEREG = U.S. Service schools 
#> --Frame size: 7
#> --Sample size: 2
#> Stratum: OBEREG = Plains (IA, KS, MN, MO, NE, ND, SD) 
#> --Frame size: 468
#> --Sample size: 39
#> Stratum: OBEREG = Rocky Mountains (CO, ID, MT, UT, WY) 
#> --Frame size: 216
#> --Sample size: 18
#> Stratum: OBEREG = New England (CT, ME, MA, NH, RI, VT) 
#> --Frame size: 299
#> --Sample size: 25
#> Stratum: OBEREG = Mid East (DE, DC, MD, NJ, NY, PA) 
#> --Frame size: 971
#> --Sample size: 82
#> Stratum: OBEREG = Great Lakes (IL, IN, MI, OH, WI) 
#> --Frame size: 851
#> --Sample size: 73
#> Stratum: OBEREG = Other U.S. jurisdictions (AS, FM, GU, MH, MP, PR, PW, VI) 
#> --Frame size: 132
#> --Sample size: 11

ipeds_srs
#> # A tidytable: 500 × 18
#>    OBEREG      UNITID INSTNM STABBR  FIPS ICLEVEL SECTOR LOCALE DEGGRANT HLOFFER
#>    <fct>        <dbl> <chr>  <chr>  <dbl> <fct>   <fct>  <fct>  <fct>    <fct>  
#>  1 Southeast … 101028 Chatt… AL         1 At lea… Publi… Subur… Degree-… At lea…
#>  2 Southeast … 101602 Lurle… AL         1 At lea… Publi… Rural… Degree-… Associ…
#>  3 Southeast … 101879 Unive… AL         1 Four o… Publi… City:… Degree-… Doctor…
#>  4 Southeast … 102049 Samfo… AL         1 Four o… Priva… Subur… Degree-… Doctor…
#>  5 Southeast … 102313 H Cou… AL         1 At lea… Publi… City:… Degree-… Associ…
#>  6 Southeast … 107318 Arkan… AR         5 At lea… Publi… Subur… Degree-… Associ…
#>  7 Southeast … 107460 North… AR         5 At lea… Publi… Town:… Degree-… At lea…
#>  8 Southeast … 132842 Albiz… FL        12 Four o… Priva… City:… Degree-… Doctor…
#>  9 Southeast … 133386 Dayto… FL        12 Four o… Publi… City:… Degree-… Bachel…
#> 10 Southeast … 133881 Flori… FL        12 Four o… Priva… City:… Degree-… Doctor…
#> # ℹ 490 more rows
#> # ℹ 8 more variables: ENRTOT <dbl>, EFUG <dbl>, EFUG1ST <dbl>, EFUGFT <dbl>,
#> #   EFGRAD <dbl>, EFGRADFT <dbl>, SelectionProbability <dbl>,
#> #   SamplingWeight <dbl>
```

``` r
ipeds_pps <- ipeds |>
  select_sample(
    method = "sys_pps",
    n = tidytable::select(ipeds_alloc, OBEREG, sample_size),
    strata = "OBEREG",
    mos = "ENRTOT",
    sort_vars = "SECTOR",
    sort_method = "serpentine"
  )
#> Stratum: OBEREG = Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV) 
#> --Frame size: 1467
#> --Sample size: 124
#> --Sampling interval (k): 37489.19
#> --Random start (r): 17174.08
#> Stratum: OBEREG = Far West (AK, CA, HI, NV, OR, WA) 
#> --Frame size: 870
#> --Sample size: 73
#> --Sampling interval (k): 46640.48
#> --Random start (r): 23535.96
#> Stratum: OBEREG = Southwest (AZ, NM, OK, TX) 
#> --Frame size: 633
#> --Sample size: 53
#> --Sampling interval (k): 49474.49
#> --Random start (r): 33072.34
#> Stratum: OBEREG = U.S. Service schools 
#> --Frame size: 7
#> --Sample size: 2
#> --Sampling interval (k): 9372
#> --Random start (r): 2483.683
#> Stratum: OBEREG = Plains (IA, KS, MN, MO, NE, ND, SD) 
#> --Frame size: 468
#> --Sample size: 39
#> --Sampling interval (k): 35210.1
#> --Random start (r): 301.2639
#> Stratum: OBEREG = Rocky Mountains (CO, ID, MT, UT, WY) 
#> --Frame size: 216
#> --Sample size: 18
#> --Sampling interval (k): 57314.5
#> --Random start (r): 10947.04
#> Stratum: OBEREG = New England (CT, ME, MA, NH, RI, VT) 
#> --Frame size: 299
#> --Sample size: 25
#> --Sampling interval (k): 44218.08
#> --Random start (r): 6258.501
#> Stratum: OBEREG = Mid East (DE, DC, MD, NJ, NY, PA) 
#> --Frame size: 971
#> --Sample size: 82
#> --Sampling interval (k): 33082.89
#> --Random start (r): 11354.24
#> Stratum: OBEREG = Great Lakes (IL, IN, MI, OH, WI) 
#> --Frame size: 851
#> --Sample size: 73
#> --Sampling interval (k): 35699.49
#> --Random start (r): 3529.973
#> Stratum: OBEREG = Other U.S. jurisdictions (AS, FM, GU, MH, MP, PR, PW, VI) 
#> --Frame size: 132
#> --Sample size: 11
#> --Sampling interval (k): 16320.64
#> --Random start (r): 11469.85

ipeds_pps
#> # A tidytable: 474 × 19
#>    OBEREG      UNITID INSTNM STABBR  FIPS ICLEVEL SECTOR LOCALE DEGGRANT HLOFFER
#>    <fct>        <dbl> <chr>  <chr>  <dbl> <fct>   <fct>  <fct>  <fct>    <fct>  
#>  1 Southeast … 100663 Unive… AL         1 Four o… Publi… City:… Degree-… Doctor…
#>  2 Southeast … 100751 The U… AL         1 Four o… Publi… City:… Degree-… Doctor…
#>  3 Southeast … 100858 Aubur… AL         1 Four o… Publi… City:… Degree-… Doctor…
#>  4 Southeast … 101480 Jacks… AL         1 Four o… Publi… Subur… Degree-… Doctor…
#>  5 Southeast … 102368 Troy … AL         1 Four o… Publi… Town:… Degree-… Doctor…
#>  6 Southeast … 106397 Unive… AR         5 Four o… Publi… City:… Degree-… Doctor…
#>  7 Southeast … 106467 Arkan… AR         5 Four o… Publi… Town:… Degree-… Doctor…
#>  8 Southeast … 132693 Easte… FL        12 Four o… Publi… City:… Degree-… Bachel…
#>  9 Southeast … 132851 Colle… FL        12 Four o… Publi… City:… Degree-… Bachel…
#> 10 Southeast … 132903 Unive… FL        12 Four o… Publi… Subur… Degree-… Doctor…
#> # ℹ 464 more rows
#> # ℹ 9 more variables: ENRTOT <dbl>, EFUG <dbl>, EFUG1ST <dbl>, EFUGFT <dbl>,
#> #   EFGRAD <dbl>, EFGRADFT <dbl>, SamplingWeight <dbl>, NumberHits <int>,
#> #   ExpectedHits <dbl>
sum(ipeds_pps$NumberHits)
#> [1] 500
```

## Contributing

We’d love your help to make SampleSelectR better! Here’s the workflow we
use:

- Start from the dev branch

- Always update your local copy first (git pull origin dev).

- Create a new branch for your work

- Name it clearly, e.g. fix-readme, add-tests, or feature-allocate.

- Make your changes

- Update code, documentation, or tests.

- If adding a function, include a short example and (if possible) a unit
  test.

- Push your branch to GitHub and open a PR into dev.

[^1]: National Center for Education Statistics, “Integrated
    Postsecondary Education Data System” 2023,
    <https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=2023&surveyNumber=-1&sid=4737d338-5121-4355-bb91-01ffa92243ef&rtid=7>,
    downloaded on May 2, 2025.

[^2]: U.S. Census Bureau, “American Community Survey 5-Year Estimates”
    2023, <https://api.census.gov/data/2023/acs/acs5>, accessed on May
    30, 2025. Tables B03002, B25002, and S0101. Note - the tidycensus
    package was used to download the data from the Census API. Region
    and division are coded based on
    <https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf>

[^3]: U.S. Census Bureau, “American Community Survey 5-Year Estimates”
    2023, <https://api.census.gov/data/2023/acs/acs5>, accessed on May
    30, 2025. Tables B03002, B25002, and S0101. Note - the tidycensus
    package was used to download the data from the Census API. Region
    and division are coded based on
    <https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf>
