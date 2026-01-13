# IPEDS - Post secondary education institutions, Fall 2023

A select set of characteristics for postsecondary educational
institutions from the Integrated Postsecondary Education Data System
(IPEDS) for operational institutions reporting fall enrollment data for
2023.

## Usage

``` r
ipeds
```

## Format

A tibble with 5914 rows and 16 columns:

- UNITID:

  Unique identification number of the institution (numeric)

- INSTNM:

  Institution (entity) name (character)

- STABBR:

  State abbreviation (character)

- FIPS:

  FIPS state code (numeric)

- OBEREG:

  Bureau of Economic Analysis (BEA) regions (factor)

- ICLEVEL:

  Level of institution (factor)

- SECTOR:

  Sector of institution (factor)

- LOCALE:

  Degree of urbanization (Urban-centric locale) (factor)

- DEGGRANT:

  Degree-granting status (factor)

- HLOFFER:

  Highest level of offering (factor)

- ENRTOT:

  Total enrollment (numeric)

- EFUG:

  Undergraduate enrollment (numeric)

- EFUG1ST:

  First-time degree/certificate-seeking undergraduate enrollment
  (numeric)

- EFUGFT:

  Full-time undergraduate enrollment (numeric)

- EFGRAD:

  Graduate enrollment (numeric)

- EFGRADFT:

  Full-time graduate enrollment (numeric)

## Source

National Center for Education Statistics, "Integrated Postsecondary
Education Data System " 2023,
<https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=2023&surveyNumber=-1&sid=4737d338-5121-4355-bb91-01ffa92243ef&rtid=7>,
downloaded on May 2, 2025.
