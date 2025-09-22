PUMA and County Data, 2023
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tidycensus)
library(tidylog)
```

    ## 
    ## Attaching package: 'tidylog'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     add_count, add_tally, anti_join, count, distinct, distinct_all,
    ##     distinct_at, distinct_if, filter, filter_all, filter_at, filter_if,
    ##     full_join, group_by, group_by_all, group_by_at, group_by_if,
    ##     inner_join, left_join, mutate, mutate_all, mutate_at, mutate_if,
    ##     relocate, rename, rename_all, rename_at, rename_if, rename_with,
    ##     right_join, sample_frac, sample_n, select, select_all, select_at,
    ##     select_if, semi_join, slice, slice_head, slice_max, slice_min,
    ##     slice_sample, slice_tail, summarise, summarise_all, summarise_at,
    ##     summarise_if, summarize, summarize_all, summarize_at, summarize_if,
    ##     tally, top_frac, top_n, transmute, transmute_all, transmute_at,
    ##     transmute_if, ungroup
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     drop_na, fill, gather, pivot_longer, pivot_wider, replace_na,
    ##     separate_wider_delim, separate_wider_position,
    ##     separate_wider_regex, spread, uncount
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

# Race/ethnicity variables (B03002)

``` r
vars_in <- load_variables(year = 2023, dataset = "acs5")

vars_reth <-
  vars_in %>%
  filter(str_starts(name, "B03002_")) %>%
  mutate(label = str_remove_all(label, ":")) %>%
  separate_wider_delim(label, delim = "!!", names = c(NA, NA, "Hisp", "Race1", "Race2"), too_few = "align_start") %>%
  mutate(
    RaceEthShort = case_when(
      Hisp == "Hispanic or Latino" & !is.na(Race1) ~ NA,
      Hisp == "Hispanic or Latino" & is.na(Race1) ~ "Pop_Pct_Hispanic",
      is.na(Hisp) ~ NA,
      !is.na(Race2) ~ NA,
      is.na(Race1) ~ NA,
      Race1 == "White alone" ~ "Pop_Pct_White_NH",
      Race1 == "Black or African American alone" ~ "Pop_Pct_Black_NH",
      Race1 == "American Indian and Alaska Native alone" ~ "Pop_Pct_AIAN_NH",
      Race1 == "Asian alone" ~ "Pop_Pct_Asian_NH",
      Race1 == "Native Hawaiian and Other Pacific Islander alone" ~ "Pop_Pct_NHPI_NH",
      TRUE ~ "Pop_Pct_Other_NH",
    )
  ) %>%
  select(name, Hisp, Race1, RaceEthShort) %>%
  filter(!is.na(RaceEthShort))
```

    ## filter: removed 28,240 rows (>99%), 21 rows remaining
    ## mutate: changed 21 values (100%) of 'label' (0 new NAs)
    ## separate_wider_delim: reorganized (label) into (Hisp, Race1, Race2) [was 21x4, now 21x6]
    ## mutate: new variable 'RaceEthShort' (character) with 8 unique values and 62% NA
    ## select: dropped 3 variables (Race2, concept, geography)
    ## filter: removed 13 rows (62%), 8 rows remaining

``` r
vars_reth
```

    ## # A tibble: 8 × 4
    ##   name       Hisp                   Race1                           RaceEthShort
    ##   <chr>      <chr>                  <chr>                           <chr>       
    ## 1 B03002_003 Not Hispanic or Latino White alone                     Pop_Pct_Whi…
    ## 2 B03002_004 Not Hispanic or Latino Black or African American alone Pop_Pct_Bla…
    ## 3 B03002_005 Not Hispanic or Latino American Indian and Alaska Nat… Pop_Pct_AIA…
    ## 4 B03002_006 Not Hispanic or Latino Asian alone                     Pop_Pct_Asi…
    ## 5 B03002_007 Not Hispanic or Latino Native Hawaiian and Other Paci… Pop_Pct_NHP…
    ## 6 B03002_008 Not Hispanic or Latino Some other race alone           Pop_Pct_Oth…
    ## 7 B03002_009 Not Hispanic or Latino Two or more races               Pop_Pct_Oth…
    ## 8 B03002_012 Hispanic or Latino     <NA>                            Pop_Pct_His…

``` r
dat_reth_puma_in <- get_acs(
  geography = "puma",
  variables = pull(vars_reth, name),
  year = 2023,
  survey = "acs5",
  summary_var = "B03002_001"
) %>%
  mutate(Level = "PUMA")
```

    ## Getting data from the 2019-2023 5-year ACS
    ## mutate: new variable 'Level' (character) with one unique value and 0% NA

``` r
dat_reth_county_in <- get_acs(
  geography = "county",
  variables = pull(vars_reth, name),
  year = 2023,
  survey = "acs5",
  summary_var = "B03002_001"
) %>%
  mutate(Level = "County")
```

    ## Getting data from the 2019-2023 5-year ACS
    ## mutate: new variable 'Level' (character) with one unique value and 0% NA

``` r
dat_reth <- bind_rows(dat_reth_puma_in, dat_reth_county_in) %>%
  left_join(vars_reth, by = c("variable" = "name")) %>%
  summarize(
    Pop_Tot = summary_est[1],
    pct = sum(estimate) / Pop_Tot * 100,
    .by = c("GEOID", "NAME", "RaceEthShort", "Level")
  ) %>%
  pivot_wider(id_cols = c("GEOID", "NAME", "Pop_Tot", "Level"), names_from = RaceEthShort, values_from = pct)
```

    ## left_join: added 3 columns (Hisp, Race1, RaceEthShort)
    ##            > rows only in x               0
    ##            > rows only in vars_reth (     0)
    ##            > matched rows            45,664
    ##            >                        ========
    ##            > rows total              45,664
    ## summarize: now 39,956 rows and 6 columns, ungrouped
    ## pivot_wider: reorganized (RaceEthShort, pct) into (Pop_Pct_White_NH, Pop_Pct_Black_NH, Pop_Pct_AIAN_NH, Pop_Pct_Asian_NH, Pop_Pct_NHPI_NH, …) [was 39956x6, now 5708x11]

``` r
dat_reth
```

    ## # A tibble: 5,708 × 11
    ##    GEOID   NAME  Pop_Tot Level Pop_Pct_White_NH Pop_Pct_Black_NH Pop_Pct_AIAN_NH
    ##    <chr>   <chr>   <dbl> <chr>            <dbl>            <dbl>           <dbl>
    ##  1 0100100 Laud…  184663 PUMA              79.4            10.5            0.153
    ##  2 0100200 Lime…  107577 PUMA              73.5            12.6            0.316
    ##  3 0100300 Morg…  156924 PUMA              73.4            12.0            0.979
    ##  4 0100401 Madi…  115400 PUMA              71.6            17.3            0.572
    ##  5 0100402 Hunt…  161229 PUMA              51.1            34.3            0.386
    ##  6 0100403 Hunt…  107475 PUMA              66.4            18.3            0.246
    ##  7 0100501 Mars…  111743 PUMA              78.2             2.40           0.184
    ##  8 0100600 DeKa…  124785 PUMA              82.3             2.01           0.449
    ##  9 0100700 Etow…  128432 PUMA              79.3            12.8            0.164
    ## 10 0100800 Calh…  116141 PUMA              69.8            21.6            0.162
    ## # ℹ 5,698 more rows
    ## # ℹ 4 more variables: Pop_Pct_Asian_NH <dbl>, Pop_Pct_NHPI_NH <dbl>,
    ## #   Pop_Pct_Other_NH <dbl>, Pop_Pct_Hispanic <dbl>

# Occupancy variables (B25002)

``` r
vars_hu <-
  vars_in %>%
  filter(str_starts(name, "B25002_")) %>%
  mutate(label = str_remove_all(label, ":")) %>%
  separate_wider_delim(label, delim = "!!", names = c(NA, NA, "Occupancy"), too_few = "align_start") %>%
  mutate(
    OccShort = case_when(
      Occupancy == "Occupied" ~ "HU_Pct_Occupied",
      Occupancy == "Vacant" ~ "HU_Pct_Vacant",
    )
  ) %>%
  select(name, Occupancy, OccShort) %>%
  filter(!is.na(OccShort))
```

    ## filter: removed 28,258 rows (>99%), 3 rows remaining
    ## mutate: changed 3 values (100%) of 'label' (0 new NAs)
    ## separate_wider_delim: reorganized (label) into (Occupancy) [was 3x4, now 3x4]
    ## mutate: new variable 'OccShort' (character) with 3 unique values and 33% NA
    ## select: dropped 2 variables (concept, geography)
    ## filter: removed one row (33%), 2 rows remaining

``` r
vars_hu
```

    ## # A tibble: 2 × 3
    ##   name       Occupancy OccShort       
    ##   <chr>      <chr>     <chr>          
    ## 1 B25002_002 Occupied  HU_Pct_Occupied
    ## 2 B25002_003 Vacant    HU_Pct_Vacant

``` r
dat_hu_puma_in <- get_acs(
  geography = "puma",
  variables = pull(vars_hu, name),
  year = 2023,
  survey = "acs5",
  summary_var = "B25002_001"
) %>%
  mutate(Level = "PUMA")
```

    ## Getting data from the 2019-2023 5-year ACS
    ## mutate: new variable 'Level' (character) with one unique value and 0% NA

``` r
dat_hu_county_in <- get_acs(
  geography = "county",
  variables = pull(vars_hu, name),
  year = 2023,
  survey = "acs5",
  summary_var = "B25002_001"
) %>%
  mutate(Level = "County")
```

    ## Getting data from the 2019-2023 5-year ACS
    ## mutate: new variable 'Level' (character) with one unique value and 0% NA

``` r
dat_hu <- bind_rows(dat_hu_puma_in, dat_hu_county_in) %>%
  left_join(vars_hu, by = c("variable" = "name")) %>%
  rename(HU_Tot = summary_est) %>%
  mutate(
    pct = estimate / HU_Tot * 100
  ) %>%
  pivot_wider(id_cols = c("GEOID", "NAME", "HU_Tot", "Level"), names_from = OccShort, values_from = pct)
```

    ## left_join: added 2 columns (Occupancy, OccShort)
    ##            > rows only in x             0
    ##            > rows only in vars_hu (     0)
    ##            > matched rows          11,416
    ##            >                      ========
    ##            > rows total            11,416
    ## rename: renamed one variable (HU_Tot)
    ## mutate: new variable 'pct' (double) with 11,068 unique values and 0% NA
    ## pivot_wider: reorganized (variable, estimate, moe, summary_moe, Occupancy, …) into (HU_Pct_Occupied, HU_Pct_Vacant) [was 11416x11, now 5708x6]

``` r
dat_hu
```

    ## # A tibble: 5,708 × 6
    ##    GEOID   NAME                       HU_Tot Level HU_Pct_Occupied HU_Pct_Vacant
    ##    <chr>   <chr>                       <dbl> <chr>           <dbl>         <dbl>
    ##  1 0100100 Lauderdale, Colbert & Fra…  86653 PUMA             86.1         13.9 
    ##  2 0100200 Limestone County PUMA; Al…  43487 PUMA             92.0          7.97
    ##  3 0100300 Morgan & Lawrence Countie…  68886 PUMA             89.9         10.1 
    ##  4 0100401 Madison County (North & E…  47001 PUMA             94.5          5.45
    ##  5 0100402 Huntsville (North & Far W…  70915 PUMA             93.1          6.94
    ##  6 0100403 Huntsville City (Central …  51732 PUMA             91.4          8.64
    ##  7 0100501 Marshall & Madison (Far S…  46592 PUMA             87.4         12.6 
    ##  8 0100600 DeKalb & Jackson Counties…  55650 PUMA             84.7         15.3 
    ##  9 0100700 Etowah & Cherokee Countie…  62150 PUMA             79.9         20.1 
    ## 10 0100800 Calhoun County PUMA; Alab…  53271 PUMA             84.7         15.3 
    ## # ℹ 5,698 more rows

# Age variables (S0101)

``` r
vars_in <- load_variables(year = 2023, dataset = "acs5/subject")

vars_age <- vars_in %>%
  filter(
    str_starts(name, "S0101_"),
    str_detect(label, "Estimate!!Percent!!Total population"),
    str_detect(label, "!!AGE!!") | str_detect(label, "SELECTED AGE CATEGORIES")
  ) %>%
  separate_wider_delim(label, delim = "!!", names = c(NA, NA, NA, "Type", "Range")) %>%
  mutate(
    AgeShort = case_when(
      Range == "Under 5 years" ~ "Pop_Pct_0004",
      Range == "5 to 9 years" ~ "Pop_Pct_0509",
      Range == "10 to 14 years" ~ "Pop_Pct_1014",
      Range == "15 to 17 years" ~ "Pop_Pct_1517",
      Range == "18 to 24 years" ~ "Pop_Pct_1824",
      Range == "25 to 29 years" ~ "Pop_Pct_2544",
      Range == "30 to 34 years" ~ "Pop_Pct_2544",
      Range == "35 to 39 years" ~ "Pop_Pct_2544",
      Range == "40 to 44 years" ~ "Pop_Pct_2544",
      Range == "45 to 49 years" ~ "Pop_Pct_4564",
      Range == "50 to 54 years" ~ "Pop_Pct_4564",
      Range == "55 to 59 years" ~ "Pop_Pct_4564",
      Range == "60 to 64 years" ~ "Pop_Pct_4564",
      Range == "65 to 69 years" ~ "Pop_Pct_6574",
      Range == "70 to 74 years" ~ "Pop_Pct_6574",
      Range == "75 to 79 years" ~ "Pop_Pct_75plus",
      Range == "80 to 84 years" ~ "Pop_Pct_75plus",
      Range == "85 years and over" ~ "Pop_Pct_75plus",
      TRUE ~ NA
    )
  ) %>%
  filter(!is.na(AgeShort)) %>%
  select(-concept) %>%
  arrange(AgeShort)
```

    ## filter: removed 19,052 rows (>99%), 30 rows remaining
    ## separate_wider_delim: reorganized (label) into (Type, Range) [was 30x3, now 30x4]
    ## mutate: new variable 'AgeShort' (character) with 10 unique values and 40% NA
    ## filter: removed 12 rows (40%), 18 rows remaining
    ## select: dropped one variable (concept)

``` r
vars_age
```

    ## # A tibble: 18 × 4
    ##    name          Type                    Range             AgeShort      
    ##    <chr>         <chr>                   <chr>             <chr>         
    ##  1 S0101_C02_002 AGE                     Under 5 years     Pop_Pct_0004  
    ##  2 S0101_C02_003 AGE                     5 to 9 years      Pop_Pct_0509  
    ##  3 S0101_C02_004 AGE                     10 to 14 years    Pop_Pct_1014  
    ##  4 S0101_C02_021 SELECTED AGE CATEGORIES 15 to 17 years    Pop_Pct_1517  
    ##  5 S0101_C02_023 SELECTED AGE CATEGORIES 18 to 24 years    Pop_Pct_1824  
    ##  6 S0101_C02_007 AGE                     25 to 29 years    Pop_Pct_2544  
    ##  7 S0101_C02_008 AGE                     30 to 34 years    Pop_Pct_2544  
    ##  8 S0101_C02_009 AGE                     35 to 39 years    Pop_Pct_2544  
    ##  9 S0101_C02_010 AGE                     40 to 44 years    Pop_Pct_2544  
    ## 10 S0101_C02_011 AGE                     45 to 49 years    Pop_Pct_4564  
    ## 11 S0101_C02_012 AGE                     50 to 54 years    Pop_Pct_4564  
    ## 12 S0101_C02_013 AGE                     55 to 59 years    Pop_Pct_4564  
    ## 13 S0101_C02_014 AGE                     60 to 64 years    Pop_Pct_4564  
    ## 14 S0101_C02_015 AGE                     65 to 69 years    Pop_Pct_6574  
    ## 15 S0101_C02_016 AGE                     70 to 74 years    Pop_Pct_6574  
    ## 16 S0101_C02_017 AGE                     75 to 79 years    Pop_Pct_75plus
    ## 17 S0101_C02_018 AGE                     80 to 84 years    Pop_Pct_75plus
    ## 18 S0101_C02_019 AGE                     85 years and over Pop_Pct_75plus

``` r
dat_age_puma_in <- get_acs(
  geography = "puma",
  variables = pull(vars_age, name),
  year = 2023,
  survey = "acs5"
) %>%
  mutate(Level = "PUMA")
```

    ## Getting data from the 2019-2023 5-year ACS
    ## Using the ACS Subject Tables
    ## mutate: new variable 'Level' (character) with one unique value and 0% NA

``` r
dat_age_county_in <- get_acs(
  geography = "county",
  variables = pull(vars_age, name),
  year = 2023,
  survey = "acs5"
) %>%
  mutate(Level = "County")
```

    ## Getting data from the 2019-2023 5-year ACS
    ## Using the ACS Subject Tables
    ## mutate: new variable 'Level' (character) with one unique value and 0% NA

``` r
dat_age <- bind_rows(dat_age_puma_in, dat_age_county_in) %>%
  left_join(vars_age, by = c("variable" = "name")) %>%
  summarize(
    pct = sum(estimate),
    .by = c("GEOID", "NAME", "AgeShort", "Level")
  ) %>%
  pivot_wider(id_cols = c("GEOID", "NAME", "Level"), names_from = AgeShort, values_from = pct)
```

    ## left_join: added 3 columns (Type, Range, AgeShort)
    ##            > rows only in x               0
    ##            > rows only in vars_age (      0)
    ##            > matched rows           102,744
    ##            >                       =========
    ##            > rows total             102,744
    ## summarize: now 51,372 rows and 5 columns, ungrouped
    ## pivot_wider: reorganized (AgeShort, pct) into (Pop_Pct_0004, Pop_Pct_0509, Pop_Pct_1014, Pop_Pct_2544, Pop_Pct_4564, …) [was 51372x5, now 5708x12]

``` r
dat_age
```

    ## # A tibble: 5,708 × 12
    ##    GEOID   NAME        Level Pop_Pct_0004 Pop_Pct_0509 Pop_Pct_1014 Pop_Pct_2544
    ##    <chr>   <chr>       <chr>        <dbl>        <dbl>        <dbl>        <dbl>
    ##  1 0100100 Lauderdale… PUMA           5.5          5.6          6.3         23.6
    ##  2 0100200 Limestone … PUMA           5.5          6.3          6.4         27.4
    ##  3 0100300 Morgan & L… PUMA           6            6.2          6.7         24.4
    ##  4 0100401 Madison Co… PUMA           6.1          6.2          7.3         24.9
    ##  5 0100402 Huntsville… PUMA           5.4          5.9          6.3         28.7
    ##  6 0100403 Huntsville… PUMA           5.9          5.3          4.7         26.5
    ##  7 0100501 Marshall &… PUMA           6.9          6.6          8.1         24.4
    ##  8 0100600 DeKalb & J… PUMA           5.6          5.5          7.4         23.7
    ##  9 0100700 Etowah & C… PUMA           5.5          5.4          6.4         23.5
    ## 10 0100800 Calhoun Co… PUMA           5.6          5.5          6.4         24.6
    ## # ℹ 5,698 more rows
    ## # ℹ 5 more variables: Pop_Pct_4564 <dbl>, Pop_Pct_6574 <dbl>,
    ## #   Pop_Pct_75plus <dbl>, Pop_Pct_1517 <dbl>, Pop_Pct_1824 <dbl>

# Code region and division

Coding is based on:
<https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf>

``` r
state_reg_div <- fips_codes %>%
  as_tibble() %>%
  distinct(STATE = state, state_code) %>%
  filter(STATE %in% c(state.abb, "DC")) %>%
  mutate(
    DIVISION = case_match(
      as.numeric(state_code),
      c(9, 23, 25, 33, 44, 50) ~ "New England",
      c(34, 36, 42) ~ "Middle Atlantic",
      c(18, 17, 26, 39, 55) ~ "East North Central",
      c(19, 20, 27, 29, 31, 38, 46) ~ "West North Central",
      c(10, 11, 12, 13, 24, 37, 45, 51, 54) ~ "South Atlantic",
      c(1, 21, 28, 47) ~ "East South Central",
      c(5, 22, 40, 48) ~ "West South Central",
      c(4, 8, 16, 35, 30, 49, 32, 56) ~ "Mountain",
      c(2, 6, 15, 41, 53) ~ "Pacific"
    ) %>%
      factor(
        levels = c(
          "New England", "Middle Atlantic", "East North Central",
          "West North Central", "South Atlantic", "East South Central",
          "West South Central", "Mountain", "Pacific"
        )
      ),
    REGION = fct_collapse(
      DIVISION,
      Northeast = c("New England", "Middle Atlantic"),
      Midwest = c("East North Central", "West North Central"),
      South = c("South Atlantic", "East South Central", "West South Central"),
      West = c("Mountain", "Pacific")
    )
  ) %>%
  arrange(REGION, DIVISION)
```

    ## distinct: removed 3,199 rows (98%), 57 rows remaining
    ## filter: removed 6 rows (11%), 51 rows remaining
    ## mutate: new variable 'DIVISION' (factor) with 9 unique values and 0% NA
    ##         new variable 'REGION' (factor) with 4 unique values and 0% NA

``` r
state_reg_div %>% print(n = 51)
```

    ## # A tibble: 51 × 4
    ##    STATE state_code DIVISION           REGION   
    ##    <chr> <chr>      <fct>              <fct>    
    ##  1 CT    09         New England        Northeast
    ##  2 ME    23         New England        Northeast
    ##  3 MA    25         New England        Northeast
    ##  4 NH    33         New England        Northeast
    ##  5 RI    44         New England        Northeast
    ##  6 VT    50         New England        Northeast
    ##  7 NJ    34         Middle Atlantic    Northeast
    ##  8 NY    36         Middle Atlantic    Northeast
    ##  9 PA    42         Middle Atlantic    Northeast
    ## 10 IL    17         East North Central Midwest  
    ## 11 IN    18         East North Central Midwest  
    ## 12 MI    26         East North Central Midwest  
    ## 13 OH    39         East North Central Midwest  
    ## 14 WI    55         East North Central Midwest  
    ## 15 IA    19         West North Central Midwest  
    ## 16 KS    20         West North Central Midwest  
    ## 17 MN    27         West North Central Midwest  
    ## 18 MO    29         West North Central Midwest  
    ## 19 NE    31         West North Central Midwest  
    ## 20 ND    38         West North Central Midwest  
    ## 21 SD    46         West North Central Midwest  
    ## 22 DE    10         South Atlantic     South    
    ## 23 DC    11         South Atlantic     South    
    ## 24 FL    12         South Atlantic     South    
    ## 25 GA    13         South Atlantic     South    
    ## 26 MD    24         South Atlantic     South    
    ## 27 NC    37         South Atlantic     South    
    ## 28 SC    45         South Atlantic     South    
    ## 29 VA    51         South Atlantic     South    
    ## 30 WV    54         South Atlantic     South    
    ## 31 AL    01         East South Central South    
    ## 32 KY    21         East South Central South    
    ## 33 MS    28         East South Central South    
    ## 34 TN    47         East South Central South    
    ## 35 AR    05         West South Central South    
    ## 36 LA    22         West South Central South    
    ## 37 OK    40         West South Central South    
    ## 38 TX    48         West South Central South    
    ## 39 AZ    04         Mountain           West     
    ## 40 CO    08         Mountain           West     
    ## 41 ID    16         Mountain           West     
    ## 42 MT    30         Mountain           West     
    ## 43 NV    32         Mountain           West     
    ## 44 NM    35         Mountain           West     
    ## 45 UT    49         Mountain           West     
    ## 46 WY    56         Mountain           West     
    ## 47 AK    02         Pacific            West     
    ## 48 CA    06         Pacific            West     
    ## 49 HI    15         Pacific            West     
    ## 50 OR    41         Pacific            West     
    ## 51 WA    53         Pacific            West

# Bring data together

``` r
dat_tog <- dat_reth %>%
  full_join(dat_hu, by = c("GEOID", "NAME", "Level")) %>%
  full_join(dat_age, by = c("GEOID", "NAME", "Level")) %>%
  mutate(state_code = str_sub(GEOID, 1, 2)) %>%
  right_join(state_reg_div, by = "state_code")
```

    ## full_join: added 3 columns (HU_Tot, HU_Pct_Occupied, HU_Pct_Vacant)
    ##            > rows only in x           0
    ##            > rows only in dat_hu      0
    ##            > matched rows         5,708
    ##            >                     =======
    ##            > rows total           5,708
    ## full_join: added 9 columns (Pop_Pct_0004, Pop_Pct_0509, Pop_Pct_1014, Pop_Pct_2544, Pop_Pct_4564, …)
    ##            > rows only in x            0
    ##            > rows only in dat_age      0
    ##            > matched rows          5,708
    ##            >                      =======
    ##            > rows total            5,708
    ## mutate: new variable 'state_code' (character) with 52 unique values and 0% NA
    ## right_join: added 3 columns (STATE, DIVISION, REGION)
    ##             > rows only in x             (  102)
    ##             > rows only in state_reg_div      0
    ##             > matched rows                5,606    (includes duplicates)
    ##             >                            =======
    ##             > rows total                  5,606

``` r
puma_2023 <- dat_tog %>%
  filter(Level == "PUMA") %>%
  select(-state_code, -Level) %>%
  select(GEOID, Name = NAME, State = STATE, Region = REGION, Division = DIVISION, everything())
```

    ## filter: removed 3,144 rows (56%), 2,462 rows remaining
    ## select: dropped 2 variables (Level, state_code)
    ## select: renamed 4 variables (Name, State, Region, Division)

``` r
county_2023 <- dat_tog %>%
  filter(Level == "County") %>%
  select(-state_code, -Level) %>%
  select(GEOID, Name = NAME, State = STATE, Region = REGION, Division = DIVISION, everything())
```

    ## filter: removed 2,462 rows (44%), 3,144 rows remaining
    ## select: dropped 2 variables (Level, state_code)
    ## select: renamed 4 variables (Name, State, Region, Division)

# Save data

``` r
summary(puma_2023)
```

    ##     GEOID               Name              State                 Region   
    ##  Length:2462        Length:2462        Length:2462        Northeast:423  
    ##  Class :character   Class :character   Class :character   Midwest  :506  
    ##  Mode  :character   Mode  :character   Mode  :character   South    :952  
    ##                                                           West     :581  
    ##                                                                          
    ##                                                                          
    ##                                                                          
    ##                Division      Pop_Tot       Pop_Pct_White_NH  Pop_Pct_Black_NH  
    ##  South Atlantic    :496   Min.   : 94457   Min.   : 0.6772   Min.   : 0.06043  
    ##  Pacific           :392   1st Qu.:114269   1st Qu.:41.1324   1st Qu.: 2.25999  
    ##  East North Central:340   Median :128683   Median :62.3550   Median : 5.82201  
    ##  Middle Atlantic   :313   Mean   :135007   Mean   :58.1157   Mean   :12.06644  
    ##  West South Central:303   3rd Qu.:151797   3rd Qu.:78.8115   3rd Qu.:15.45296  
    ##  Mountain          :189   Max.   :254693   Max.   :95.5992   Max.   :91.54119  
    ##  (Other)           :429                                                        
    ##  Pop_Pct_AIAN_NH    Pop_Pct_Asian_NH   Pop_Pct_NHPI_NH    Pop_Pct_Other_NH  
    ##  Min.   : 0.00000   Min.   : 0.03401   Min.   : 0.00000   Min.   : 0.09704  
    ##  1st Qu.: 0.06733   1st Qu.: 1.11991   1st Qu.: 0.00978   1st Qu.: 3.22126  
    ##  Median : 0.12409   Median : 2.67222   Median : 0.03879   Median : 4.07446  
    ##  Mean   : 0.54049   Mean   : 5.75511   Mean   : 0.16905   Mean   : 4.35801  
    ##  3rd Qu.: 0.25500   3rd Qu.: 6.54233   3rd Qu.: 0.10762   3rd Qu.: 5.11640  
    ##  Max.   :71.05525   Max.   :69.44064   Max.   :23.33594   Max.   :25.66375  
    ##                                                                             
    ##  Pop_Pct_Hispanic      HU_Tot       HU_Pct_Occupied HU_Pct_Vacant   
    ##  Min.   : 0.7159   Min.   : 25782   Min.   :46.59   Min.   : 1.123  
    ##  1st Qu.: 5.6869   1st Qu.: 47813   1st Qu.:87.58   1st Qu.: 4.995  
    ##  Median :11.3890   Median : 55208   Median :92.36   Median : 7.638  
    ##  Mean   :18.9952   Mean   : 57812   Mean   :90.36   Mean   : 9.639  
    ##  3rd Qu.:24.5716   3rd Qu.: 65673   3rd Qu.:95.00   3rd Qu.:12.419  
    ##  Max.   :96.0544   Max.   :140746   Max.   :98.88   Max.   :53.406  
    ##                                                                     
    ##   Pop_Pct_0004    Pop_Pct_0509     Pop_Pct_1014    Pop_Pct_2544  
    ##  Min.   : 1.80   Min.   : 1.400   Min.   : 1.30   Min.   :11.70  
    ##  1st Qu.: 5.00   1st Qu.: 5.400   1st Qu.: 5.80   1st Qu.:23.90  
    ##  Median : 5.60   Median : 6.000   Median : 6.50   Median :26.10  
    ##  Mean   : 5.69   Mean   : 6.035   Mean   : 6.47   Mean   :26.76  
    ##  3rd Qu.: 6.30   3rd Qu.: 6.700   3rd Qu.: 7.20   3rd Qu.:28.70  
    ##  Max.   :16.70   Max.   :14.400   Max.   :12.50   Max.   :55.40  
    ##                                                                  
    ##   Pop_Pct_4564    Pop_Pct_6574   Pop_Pct_75plus    Pop_Pct_1517 
    ##  Min.   : 9.60   Min.   : 3.60   Min.   : 1.700   Min.   :0.60  
    ##  1st Qu.:23.50   1st Qu.: 8.30   1st Qu.: 5.300   1st Qu.:3.50  
    ##  Median :25.40   Median : 9.90   Median : 6.700   Median :3.90  
    ##  Mean   :25.14   Mean   :10.03   Mean   : 6.837   Mean   :3.92  
    ##  3rd Qu.:27.00   3rd Qu.:11.40   3rd Qu.: 8.100   3rd Qu.:4.30  
    ##  Max.   :34.00   Max.   :30.80   Max.   :27.000   Max.   :6.70  
    ##                                                                 
    ##   Pop_Pct_1824   
    ##  Min.   : 2.900  
    ##  1st Qu.: 7.400  
    ##  Median : 8.300  
    ##  Mean   : 9.118  
    ##  3rd Qu.: 9.700  
    ##  Max.   :39.900  
    ## 

``` r
glimpse(puma_2023)
```

    ## Rows: 2,462
    ## Columns: 25
    ## $ GEOID            <chr> "0100100", "0100200", "0100300", "0100401", "0100402"…
    ## $ Name             <chr> "Lauderdale, Colbert & Franklin Counties PUMA; Alabam…
    ## $ State            <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL",…
    ## $ Region           <fct> South, South, South, South, South, South, South, Sout…
    ## $ Division         <fct> East South Central, East South Central, East South Ce…
    ## $ Pop_Tot          <dbl> 184663, 107577, 156924, 115400, 161229, 107475, 11174…
    ## $ Pop_Pct_White_NH <dbl> 79.43660, 73.51664, 73.37947, 71.63432, 51.12046, 66.…
    ## $ Pop_Pct_Black_NH <dbl> 10.531617, 12.589122, 11.965028, 17.278163, 34.308964…
    ## $ Pop_Pct_AIAN_NH  <dbl> 0.15271061, 0.31605269, 0.97881777, 0.57192374, 0.386…
    ## $ Pop_Pct_Asian_NH <dbl> 0.4565073, 1.7838386, 0.5015167, 1.9367418, 3.6401640…
    ## $ Pop_Pct_NHPI_NH  <dbl> 0.021661080, 0.040900936, 0.013382274, 0.009532062, 0…
    ## $ Pop_Pct_Other_NH <dbl> 3.230750, 4.516765, 4.508552, 4.564991, 3.800185, 4.2…
    ## $ Pop_Pct_Hispanic <dbl> 6.170159, 7.236677, 8.653233, 4.004333, 6.696686, 9.1…
    ## $ HU_Tot           <dbl> 86653, 43487, 68886, 47001, 70915, 51732, 46592, 5565…
    ## $ HU_Pct_Occupied  <dbl> 86.07550, 92.02980, 89.85425, 94.54905, 93.05789, 91.…
    ## $ HU_Pct_Vacant    <dbl> 13.924503, 7.970198, 10.145748, 5.450948, 6.942114, 8…
    ## $ Pop_Pct_0004     <dbl> 5.5, 5.5, 6.0, 6.1, 5.4, 5.9, 6.9, 5.6, 5.5, 5.6, 5.6…
    ## $ Pop_Pct_0509     <dbl> 5.6, 6.3, 6.2, 6.2, 5.9, 5.3, 6.6, 5.5, 5.4, 5.5, 6.0…
    ## $ Pop_Pct_1014     <dbl> 6.3, 6.4, 6.7, 7.3, 6.3, 4.7, 8.1, 7.4, 6.4, 6.4, 6.9…
    ## $ Pop_Pct_2544     <dbl> 23.6, 27.4, 24.4, 24.9, 28.7, 26.5, 24.4, 23.7, 23.5,…
    ## $ Pop_Pct_4564     <dbl> 25.7, 27.6, 26.8, 30.0, 24.8, 24.1, 25.2, 26.6, 27.3,…
    ## $ Pop_Pct_6574     <dbl> 11.4, 9.4, 10.7, 10.1, 8.1, 9.8, 10.0, 11.4, 12.5, 11…
    ## $ Pop_Pct_75plus   <dbl> 8.3, 6.0, 7.4, 5.6, 4.9, 9.2, 6.6, 7.4, 7.9, 7.0, 7.1…
    ## $ Pop_Pct_1517     <dbl> 3.7, 4.1, 4.1, 3.5, 4.3, 3.4, 4.5, 4.3, 3.8, 3.9, 4.2…
    ## $ Pop_Pct_1824     <dbl> 10.1, 7.5, 7.9, 6.1, 11.6, 11.0, 7.8, 8.0, 7.7, 10.9,…

``` r
usethis::use_data(puma_2023, overwrite = TRUE)
```

``` r
summary(county_2023)
```

    ##     GEOID               Name              State                 Region    
    ##  Length:3144        Length:3144        Length:3144        Northeast: 218  
    ##  Class :character   Class :character   Class :character   Midwest  :1055  
    ##  Mode  :character   Mode  :character   Mode  :character   South    :1422  
    ##                                                           West     : 449  
    ##                                                                           
    ##                                                                           
    ##                                                                           
    ##                Division      Pop_Tot        Pop_Pct_White_NH  Pop_Pct_Black_NH 
    ##  West North Central:618   Min.   :     43   Min.   :  2.153   Min.   : 0.0000  
    ##  South Atlantic    :588   1st Qu.:  10794   1st Qu.: 62.608   1st Qu.: 0.6574  
    ##  West South Central:470   Median :  25813   Median : 81.103   Median : 2.0825  
    ##  East North Central:437   Mean   : 105721   Mean   : 74.298   Mean   : 8.6559  
    ##  East South Central:364   3rd Qu.:  68379   3rd Qu.: 90.618   3rd Qu.: 9.5543  
    ##  Mountain          :281   Max.   :9848406   Max.   :100.000   Max.   :86.0575  
    ##  (Other)           :386                                                        
    ##  Pop_Pct_AIAN_NH   Pop_Pct_Asian_NH  Pop_Pct_NHPI_NH     Pop_Pct_Other_NH
    ##  Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.000000   Min.   : 0.000  
    ##  1st Qu.: 0.0618   1st Qu.: 0.2780   1st Qu.: 0.000000   1st Qu.: 2.531  
    ##  Median : 0.1546   Median : 0.6098   Median : 0.005365   Median : 3.478  
    ##  Mean   : 1.6596   Mean   : 1.4257   Mean   : 0.096733   Mean   : 3.798  
    ##  3rd Qu.: 0.4637   3rd Qu.: 1.3405   3rd Qu.: 0.059577   3rd Qu.: 4.495  
    ##  Max.   :91.3520   Max.   :41.9978   Max.   :18.604651   Max.   :25.562  
    ##                                                                          
    ##  Pop_Pct_Hispanic     HU_Tot        HU_Pct_Occupied HU_Pct_Vacant   
    ##  Min.   : 0.000   Min.   :     47   Min.   :22.68   Min.   : 1.707  
    ##  1st Qu.: 2.533   1st Qu.:   5240   1st Qu.:78.61   1st Qu.: 9.393  
    ##  Median : 4.881   Median :  12374   Median :85.28   Median :14.721  
    ##  Mean   :10.066   Mean   :  45271   Mean   :83.10   Mean   :16.904  
    ##  3rd Qu.:10.809   3rd Qu.:  31710   3rd Qu.:90.61   3rd Qu.:21.388  
    ##  Max.   :97.306   Max.   :3624084   Max.   :98.29   Max.   :77.316  
    ##                                                                     
    ##   Pop_Pct_0004     Pop_Pct_0509     Pop_Pct_1014     Pop_Pct_2544  
    ##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
    ##  1st Qu.: 4.900   1st Qu.: 5.200   1st Qu.: 5.700   1st Qu.:21.90  
    ##  Median : 5.500   Median : 6.000   Median : 6.400   Median :23.60  
    ##  Mean   : 5.529   Mean   : 5.997   Mean   : 6.455   Mean   :23.82  
    ##  3rd Qu.: 6.200   3rd Qu.: 6.700   3rd Qu.: 7.200   3rd Qu.:25.60  
    ##  Max.   :20.600   Max.   :21.000   Max.   :13.300   Max.   :58.20  
    ##                                                                    
    ##   Pop_Pct_4564    Pop_Pct_6574   Pop_Pct_75plus    Pop_Pct_1517   
    ##  Min.   : 8.90   Min.   : 2.10   Min.   : 0.700   Min.   : 0.000  
    ##  1st Qu.:24.40   1st Qu.:10.10   1st Qu.: 6.700   1st Qu.: 3.600  
    ##  Median :26.00   Median :11.60   Median : 8.000   Median : 4.000  
    ##  Mean   :25.75   Mean   :11.87   Mean   : 8.182   Mean   : 3.955  
    ##  3rd Qu.:27.40   3rd Qu.:13.10   3rd Qu.: 9.400   3rd Qu.: 4.300  
    ##  Max.   :46.90   Max.   :38.50   Max.   :46.300   Max.   :17.900  
    ##                                                                   
    ##   Pop_Pct_1824   
    ##  Min.   : 0.000  
    ##  1st Qu.: 6.900  
    ##  Median : 7.800  
    ##  Mean   : 8.453  
    ##  3rd Qu.: 8.900  
    ##  Max.   :49.100  
    ## 

``` r
glimpse(county_2023)
```

    ## Rows: 3,144
    ## Columns: 25
    ## $ GEOID            <chr> "01001", "01003", "01005", "01007", "01009", "01011",…
    ## $ Name             <chr> "Autauga County, Alabama", "Baldwin County, Alabama",…
    ## $ State            <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL",…
    ## $ Region           <fct> South, South, South, South, South, South, South, Sout…
    ## $ Division         <fct> East South Central, East South Central, East South Ce…
    ## $ Pop_Tot          <dbl> 59285, 239945, 24757, 22152, 59292, 10157, 18807, 116…
    ## $ Pop_Pct_White_NH <dbl> 71.68255, 81.41324, 43.65230, 73.73149, 84.95919, 21.…
    ## $ Pop_Pct_Black_NH <dbl> 19.952771, 7.939736, 46.899867, 20.697905, 1.259866, …
    ## $ Pop_Pct_AIAN_NH  <dbl> 0.07084423, 0.25339140, 0.13733490, 0.10834236, 0.101…
    ## $ Pop_Pct_Asian_NH <dbl> 1.01037362, 0.94146575, 0.54933958, 0.23022752, 0.086…
    ## $ Pop_Pct_NHPI_NH  <dbl> 0.000000000, 0.032507450, 0.000000000, 0.000000000, 0…
    ## $ Pop_Pct_Other_NH <dbl> 3.592814, 3.837963, 2.742659, 1.873420, 3.390002, 1.5…
    ## $ Pop_Pct_Hispanic <dbl> 3.6906469, 5.5816958, 6.0184998, 3.3586132, 10.055319…
    ## $ HU_Tot           <dbl> 24731, 128518, 11702, 9090, 24793, 4554, 9841, 53271,…
    ## $ HU_Pct_Occupied  <dbl> 91.07193, 73.64105, 77.59357, 83.28933, 88.64196, 75.…
    ## $ HU_Pct_Vacant    <dbl> 8.928066, 26.358954, 22.406426, 16.710671, 11.358045,…
    ## $ Pop_Pct_0004     <dbl> 5.8, 5.2, 5.6, 4.8, 5.7, 5.4, 5.7, 5.6, 5.8, 4.7, 6.1…
    ## $ Pop_Pct_0509     <dbl> 5.8, 5.5, 6.8, 6.3, 6.1, 6.5, 5.5, 5.5, 5.3, 4.4, 7.5…
    ## $ Pop_Pct_1014     <dbl> 7.3, 6.7, 4.9, 5.5, 7.0, 6.5, 7.2, 6.4, 6.2, 6.3, 6.7…
    ## $ Pop_Pct_2544     <dbl> 26.3, 23.2, 26.2, 27.8, 23.9, 27.1, 23.1, 24.6, 23.8,…
    ## $ Pop_Pct_4564     <dbl> 26.7, 27.1, 25.2, 27.1, 26.7, 27.7, 25.4, 25.0, 27.3,…
    ## $ Pop_Pct_6574     <dbl> 9.2, 12.8, 11.4, 9.8, 11.0, 10.8, 12.5, 11.1, 11.7, 1…
    ## $ Pop_Pct_75plus   <dbl> 6.7, 8.5, 8.3, 7.2, 7.6, 7.1, 9.0, 7.0, 8.3, 9.4, 7.0…
    ## $ Pop_Pct_1517     <dbl> 4.6, 3.9, 3.7, 3.5, 4.4, 2.2, 4.0, 3.9, 3.4, 3.5, 3.9…
    ## $ Pop_Pct_1824     <dbl> 7.7, 7.0, 7.9, 8.0, 7.7, 6.6, 7.6, 10.9, 8.2, 6.7, 8.…

``` r
usethis::use_data(county_2023, overwrite = TRUE)
```
