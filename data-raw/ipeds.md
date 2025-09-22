IPEDS Fall 2023
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

# Read in data

Downloaded original files from [IPEDS Data
Center](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=2023&surveyNumber=-1&sid=4737d338-5121-4355-bb91-01ffa92243ef&rtid=7)

- 2023 - Institutional characteristics - Directory information - HD2023
- 2023 - Institutional characteristics -Response status for all survey
  components - FLAGS2023
- 2023 - Frequently used/Derived variables - Enrollments, retention
  rates and other selected enrollment indicators: Fall 2023 - DRVEF2023

``` r
inst_char_in <- read_csv(here::here("data-raw", "IPEDS_2023", "hd2023.csv"))
```

    ## Rows: 6163 Columns: 73
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (23): INSTNM, IALIAS, ADDR, CITY, STABBR, ZIP, CHFNM, CHFTITLE, EIN, UEI...
    ## dbl (50): UNITID, FIPS, OBEREG, GENTELE, OPEFLAG, SECTOR, ICLEVEL, CONTROL, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fall_enroll_in <- read_csv(here::here("data-raw", "IPEDS_2023", "drvef2023.csv"))
```

    ## Rows: 5914 Columns: 91
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (45): PCTFT1ST, PCUENRWH, PCUENRBK, PCUENRHS, PCUENRAP, PCUENRAS, PCUENR...
    ## dbl (46): UNITID, ENRTOT, FTE, ENRFT, ENRPT, PCTENRWH, PCTENRBK, PCTENRHS, P...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
flags_in <- read_csv(here::here("data-raw", "IPEDS_2023", "flags2023.csv"))
```

    ## Rows: 6163 Columns: 102
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (13): PCC_F, PCE12_F, PCSFA_F, PCGR_F, PCGR2_F, PCOM_F, PCADM_F, PCHR_F,...
    ## dbl (89): UNITID, STAT_IC, LOCK_IC, IMP_IC, STAT_C, LOCK_C, PRCH_C, IDX_C, I...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fall_enroll <- fall_enroll_in %>%
  select(UNITID, ENRTOT, EFUG, EFUG1ST, EFUGFT, EFGRAD, EFGRADFT)
```

    ## select: dropped 84 variables (FTE, ENRFT, ENRPT, PCTENRWH, PCTENRBK, …)

``` r
inst_char_slim <- inst_char_in %>%
  left_join(select(flags_in, UNITID, STAT_EF)) %>%
  filter(SECTOR != 0, CYACTIVE == 1, STAT_EF %in% c(1, 2, 4)) %>%
  select(UNITID, INSTNM, STABBR, FIPS, OBEREG, ICLEVEL, SECTOR, LOCALE, DEGGRANT, HLOFFER, STAT_EF)
```

    ## select: dropped 100 variables (STAT_IC, LOCK_IC, IMP_IC, STAT_C, LOCK_C, …)
    ## Joining with `by = join_by(UNITID)`
    ## left_join: added one column (STAT_EF)
    ##            > rows only in x                              0
    ##            > rows only in select(flags_in, UNITID.. (    0)
    ##            > matched rows                            6,163
    ##            >                                        =======
    ##            > rows total                              6,163
    ## filter: removed 249 rows (4%), 5,914 rows remaining
    ## select: dropped 63 variables (IALIAS, ADDR, CITY, ZIP, CHFNM, …)

``` r
inst_char_cb <- readxl::read_xlsx(here::here("data-raw", "IPEDS_2023", "hd2023.xlsx"), sheet = "Frequencies")

add_label <- function(var) {
  lu <- inst_char_cb %>%
    filter(varname == var)

  alphacode <- any(str_detect(str_to_upper(lu$codevalue) %>% str_flatten(), LETTERS))

  if (!alphacode) {
    lu <- lu %>%
      mutate(
        codevalue = as.numeric(codevalue),
        valuelabel = fct_reorder(valuelabel, codevalue)
      ) %>%
      select(codevalue, valuelabel)
  } else {
    lu <- lu %>%
      mutate(
        valuelabel = fct_reorder(valuelabel, codevalue)
      ) %>%
      select(codevalue, valuelabel)
  }


  jb <- "codevalue"
  names(jb) <- var

  outcol <- inst_char_slim %>%
    left_join(lu, by = jb) %>%
    mutate(valuelabel = fct_drop(valuelabel)) %>%
    select(valuelabel)

  names(outcol) <- var

  return(outcol)
}
options("tidylog.display" = list()) # turn off

var_desc <- inst_char_slim %>%
  select(OBEREG:HLOFFER) %>%
  names() %>%
  map(add_label) %>%
  list_cbind()
options("tidylog.display" = NULL) # turn on

inst_char_full <-
  inst_char_slim %>%
  select(-c(OBEREG:HLOFFER)) %>%
  cbind(var_desc) %>%
  as_tibble()
```

    ## select: dropped 6 variables (OBEREG, ICLEVEL, SECTOR, LOCALE, DEGGRANT, …)

``` r
ipeds_2023_init <- inst_char_full %>%
  left_join(fall_enroll, by = "UNITID") %>%
  mutate(
    EnrollmentDataAvailable = !(is.na(ENRTOT)),
    # manual fix of non ASCII character
    INSTNM = case_when(
      UNITID == 430935 ~ "Colegio de Cinematografia Artes y Television",
      UNITID == 449135 ~ "Dewey University-Juana Diaz",
      TRUE ~ INSTNM
    )
  )
```

    ## left_join: added 6 columns (ENRTOT, EFUG, EFUG1ST, EFUGFT, EFGRAD, …)
    ##            > rows only in x                0
    ##            > rows only in fall_enroll (    0)
    ##            > matched rows              5,914
    ##            >                          =======
    ##            > rows total                5,914
    ## mutate: changed 2 values (<1%) of 'INSTNM' (0 new NAs)
    ##         new variable 'EnrollmentDataAvailable' (logical) with one unique value and 0% NA

``` r
# Before and after rename
options("tidylog.display" = list()) # turn off
inst_char_full %>%
  filter(UNITID %in% c(430935, 449135)) %>%
  select(UNITID, INSTNM)
```

    ## # A tibble: 2 × 2
    ##   UNITID INSTNM                                           
    ##    <dbl> <chr>                                            
    ## 1 430935 "Colegio de Cinematograf\xeda Artes y Television"
    ## 2 449135 "Dewey University-Juana D\xedaz"

``` r
ipeds_2023_init %>%
  filter(UNITID %in% c(430935, 449135)) %>%
  select(UNITID, INSTNM)
```

    ## # A tibble: 2 × 2
    ##   UNITID INSTNM                                      
    ##    <dbl> <chr>                                       
    ## 1 430935 Colegio de Cinematografia Artes y Television
    ## 2 449135 Dewey University-Juana Diaz

``` r
options("tidylog.display" = NULL) # turn on

ipeds_2023_init %>% count(EnrollmentDataAvailable, STAT_EF)
```

    ## count: now 3 rows and 3 columns, ungrouped

    ## # A tibble: 3 × 3
    ##   EnrollmentDataAvailable STAT_EF     n
    ##   <lgl>                     <dbl> <int>
    ## 1 TRUE                          1  5906
    ## 2 TRUE                          2     1
    ## 3 TRUE                          4     7

``` r
ipeds <- ipeds_2023_init %>%
  select(-EnrollmentDataAvailable, -STAT_EF)
```

    ## select: dropped 2 variables (STAT_EF, EnrollmentDataAvailable)

# Save data

``` r
skimr::skim(ipeds)
```

|                                                  |       |
|:-------------------------------------------------|:------|
| Name                                             | ipeds |
| Number of rows                                   | 5914  |
| Number of columns                                | 16    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |       |
| Column type frequency:                           |       |
| character                                        | 2     |
| factor                                           | 6     |
| numeric                                          | 8     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |       |
| Group variables                                  | None  |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| INSTNM        |         0 |             1 |   3 |  91 |     0 |     5819 |          0 |
| STABBR        |         0 |             1 |   2 |   2 |     0 |       59 |          0 |

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts |
|:---|---:|---:|:---|---:|:---|
| OBEREG | 0 | 1 | FALSE | 10 | Sou: 1467, Mid: 971, Far: 870, Gre: 851 |
| ICLEVEL | 0 | 1 | FALSE | 3 | Fou: 2747, Les: 1655, At : 1512 |
| SECTOR | 0 | 1 | FALSE | 9 | Pri: 1608, Pri: 1373, Pub: 868, Pub: 820 |
| LOCALE | 0 | 1 | FALSE | 13 | Sub: 1442, Cit: 1392, Cit: 778, Cit: 713 |
| DEGGRANT | 0 | 1 | FALSE | 2 | Deg: 3995, Non: 1919 |
| HLOFFER | 0 | 1 | FALSE | 9 | At : 1514, Doc: 1232, Ass: 1022, Bac: 677 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| UNITID | 0 | 1 | 284663.97 | 139432.21 | 100654 | 169277.25 | 219492.5 | 446954.00 | 499723 | ▇▇▁▂▇ |
| FIPS | 0 | 1 | 29.19 | 16.78 | 1 | 13.00 | 29.0 | 42.00 | 78 | ▇▆▇▃▁ |
| ENRTOT | 0 | 1 | 3331.59 | 8201.02 | 1 | 127.00 | 588.0 | 2849.00 | 185015 | ▇▁▁▁▁ |
| EFUG | 0 | 1 | 2777.88 | 6672.24 | 0 | 98.25 | 472.0 | 2455.25 | 159653 | ▇▁▁▁▁ |
| EFUG1ST | 0 | 1 | 507.97 | 1113.65 | 0 | 21.00 | 105.0 | 478.00 | 15343 | ▇▁▁▁▁ |
| EFUGFT | 0 | 1 | 1706.23 | 4594.16 | 0 | 71.00 | 321.0 | 1425.50 | 135822 | ▇▁▁▁▁ |
| EFGRAD | 0 | 1 | 553.70 | 2203.12 | 0 | 0.00 | 0.0 | 120.00 | 50245 | ▇▁▁▁▁ |
| EFGRADFT | 0 | 1 | 328.37 | 1445.09 | 0 | 0.00 | 0.0 | 56.75 | 49193 | ▇▁▁▁▁ |

``` r
summary(ipeds)
```

    ##      UNITID          INSTNM             STABBR               FIPS      
    ##  Min.   :100654   Length:5914        Length:5914        Min.   : 1.00  
    ##  1st Qu.:169277   Class :character   Class :character   1st Qu.:13.00  
    ##  Median :219493   Mode  :character   Mode  :character   Median :29.00  
    ##  Mean   :284664                                         Mean   :29.19  
    ##  3rd Qu.:446954                                         3rd Qu.:42.00  
    ##  Max.   :499723                                         Max.   :78.00  
    ##                                                                        
    ##                                                         OBEREG    
    ##  Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV):1467  
    ##  Mid East (DE, DC, MD, NJ, NY, PA)                         : 971  
    ##  Far West (AK, CA, HI, NV, OR, WA)                         : 870  
    ##  Great Lakes (IL, IN, MI, OH, WI)                          : 851  
    ##  Southwest (AZ, NM, OK, TX)                                : 633  
    ##  Plains (IA, KS, MN, MO, NE, ND, SD)                       : 468  
    ##  (Other)                                                   : 654  
    ##                                 ICLEVEL    
    ##  Four or more years                 :2747  
    ##  At least 2 but less than 4 years   :1512  
    ##  Less than 2 years (below associate):1655  
    ##                                            
    ##                                            
    ##                                            
    ##                                            
    ##                                      SECTOR               LOCALE    
    ##  Private not-for-profit, 4-year or above:1608   Suburb: Large:1442  
    ##  Private for-profit, less-than 2-year   :1373   City: Large  :1392  
    ##  Public, 2-year                         : 868   City: Small  : 778  
    ##  Public, 4-year or above                : 820   City: Midsize: 713  
    ##  Private for-profit, 2-year             : 517   Town: Distant: 396  
    ##  Private for-profit, 4-year or above    : 319   Rural: Fringe: 298  
    ##  (Other)                                : 409   (Other)      : 895  
    ##                                         DEGGRANT   
    ##  Degree-granting                            :3995  
    ##  Nondegree-granting, primarily postsecondary:1919  
    ##                                                    
    ##                                                    
    ##                                                    
    ##                                                    
    ##                                                    
    ##                                      HLOFFER         ENRTOT      
    ##  At least 1, but less than 2 academic yrs:1514   Min.   :     1  
    ##  Doctor's degree                         :1232   1st Qu.:   127  
    ##  Associate's degree                      :1022   Median :   588  
    ##  Bachelor's degree                       : 677   Mean   :  3332  
    ##  Master's degree                         : 672   3rd Qu.:  2849  
    ##  At least 2, but less than 4 academic yrs: 490   Max.   :185015  
    ##  (Other)                                 : 307                   
    ##       EFUG              EFUG1ST          EFUGFT           EFGRAD       
    ##  Min.   :     0.00   Min.   :    0   Min.   :     0   Min.   :    0.0  
    ##  1st Qu.:    98.25   1st Qu.:   21   1st Qu.:    71   1st Qu.:    0.0  
    ##  Median :   472.00   Median :  105   Median :   321   Median :    0.0  
    ##  Mean   :  2777.88   Mean   :  508   Mean   :  1706   Mean   :  553.7  
    ##  3rd Qu.:  2455.25   3rd Qu.:  478   3rd Qu.:  1426   3rd Qu.:  120.0  
    ##  Max.   :159653.00   Max.   :15343   Max.   :135822   Max.   :50245.0  
    ##                                                                        
    ##     EFGRADFT       
    ##  Min.   :    0.00  
    ##  1st Qu.:    0.00  
    ##  Median :    0.00  
    ##  Mean   :  328.37  
    ##  3rd Qu.:   56.75  
    ##  Max.   :49193.00  
    ## 

``` r
glimpse(ipeds)
```

    ## Rows: 5,914
    ## Columns: 16
    ## $ UNITID   <dbl> 100654, 100663, 100690, 100706, 100724, 100751, 100760, 10081…
    ## $ INSTNM   <chr> "Alabama A & M University", "University of Alabama at Birming…
    ## $ STABBR   <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "…
    ## $ FIPS     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ OBEREG   <fct> "Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)",…
    ## $ ICLEVEL  <fct> Four or more years, Four or more years, Four or more years, F…
    ## $ SECTOR   <fct> "Public, 4-year or above", "Public, 4-year or above", "Privat…
    ## $ LOCALE   <fct> City: Midsize, City: Midsize, City: Midsize, City: Midsize, C…
    ## $ DEGGRANT <fct> "Degree-granting", "Degree-granting", "Degree-granting", "Deg…
    ## $ HLOFFER  <fct> "Doctor's degree", "Doctor's degree", "Doctor's degree", "Doc…
    ## $ ENRTOT   <dbl> 6614, 21160, 636, 8743, 3870, 39622, 1910, 2955, 5189, 33015,…
    ## $ EFUG     <dbl> 5845, 12382, 226, 6851, 3322, 33435, 1910, 2531, 3407, 26874,…
    ## $ EFUG1ST  <dbl> 1990, 2095, 0, 1176, 951, 8279, 381, 0, 413, 5935, 148, 272, …
    ## $ EFUGFT   <dbl> 5221, 9841, 129, 5644, 2959, 30003, 561, 1121, 2422, 24135, 7…
    ## $ EFGRAD   <dbl> 769, 8778, 410, 1892, 548, 6187, 0, 424, 1782, 6141, 18, 0, 1…
    ## $ EFGRADFT <dbl> 439, 4974, 132, 631, 378, 3660, 0, 63, 811, 3483, 18, 0, 11, …

``` r
usethis::use_data(ipeds, overwrite = TRUE, compress = "xz")
```

# Sketch out data for documentation

``` r
varlist_hd <- readxl::read_xlsx(here::here("data-raw", "IPEDS_2023", "hd2023.xlsx"), sheet = "varlist")
varlist_dr <- readxl::read_xlsx(here::here("data-raw", "IPEDS_2023", "drvef2023.xlsx"), sheet = "varlist")

varlist_all <- varlist_hd %>%
  bind_rows(varlist_dr) %>%
  distinct(varname, .keep_all = TRUE)
```

    ## distinct: removed one row (1%), 163 rows remaining

``` r
formatline <- glue::glue("#' @format A tibble with {nrow(ipeds)} rows and {ncol(ipeds)} columns:")

des <- lapply(ipeds, class) %>%
  as_tibble() %>%
  pivot_longer(everything(), names_to = "variable", values_to = "class") %>%
  left_join(varlist_all, by = c("variable" = "varname")) %>%
  mutate(
    Describe = str_c("#'   \\item{", variable, "}{", varTitle, " (", class, ")}")
  ) %>%
  pull(Describe)
```

    ## pivot_longer: reorganized (UNITID, INSTNM, STABBR, FIPS, OBEREG, …) into (variable, class) [was 1x16, now 16x2]
    ## left_join: added 6 columns (varnumber, DataType, Fieldwidth, format, imputationvar, …)
    ##            > rows only in x              0
    ##            > rows only in varlist_all (147)
    ##            > matched rows               16
    ##            >                          =====
    ##            > rows total                 16
    ## mutate: new variable 'Describe' (character) with 16 unique values and 0% NA

``` r
str_view(formatline)
```

    ## [1] │ #' @format A tibble with 5914 rows and 16 columns:

``` r
str_view(des)
```

    ##  [1] │ #'   \item{UNITID}{Unique identification number of the institution (numeric)}
    ##  [2] │ #'   \item{INSTNM}{Institution (entity) name (character)}
    ##  [3] │ #'   \item{STABBR}{State abbreviation (character)}
    ##  [4] │ #'   \item{FIPS}{FIPS state code (numeric)}
    ##  [5] │ #'   \item{OBEREG}{Bureau of Economic Analysis (BEA) regions (factor)}
    ##  [6] │ #'   \item{ICLEVEL}{Level of institution (factor)}
    ##  [7] │ #'   \item{SECTOR}{Sector of institution (factor)}
    ##  [8] │ #'   \item{LOCALE}{Degree of urbanization (Urban-centric locale) (factor)}
    ##  [9] │ #'   \item{DEGGRANT}{Degree-granting status (factor)}
    ## [10] │ #'   \item{HLOFFER}{Highest level of offering (factor)}
    ## [11] │ #'   \item{ENRTOT}{Total  enrollment (numeric)}
    ## [12] │ #'   \item{EFUG}{Undergraduate enrollment (numeric)}
    ## [13] │ #'   \item{EFUG1ST}{First-time degree/certificate-seeking undergraduate enrollment (numeric)}
    ## [14] │ #'   \item{EFUGFT}{Full-time undergraduate enrollment (numeric)}
    ## [15] │ #'   \item{EFGRAD}{Graduate enrollment (numeric)}
    ## [16] │ #'   \item{EFGRADFT}{Full-time graduate enrollment (numeric)}
