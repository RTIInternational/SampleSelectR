#' @title IPEDS - Post secondary education institutions, Fall 2023
#' @description A select set of characteristics for postsecondary educational institutions from the Integrated Postsecondary Education Data System (IPEDS) for operational institutions reporting fall enrollment data for 2023.
#' @format A tibble with 5914 rows and 16 columns:
#' \describe{
#'   \item{UNITID}{Unique identification number of the institution (numeric)}
#'   \item{INSTNM}{Institution (entity) name (character)}
#'   \item{STABBR}{State abbreviation (character)}
#'   \item{FIPS}{FIPS state code (numeric)}
#'   \item{OBEREG}{Bureau of Economic Analysis (BEA) regions (factor)}
#'   \item{ICLEVEL}{Level of institution (factor)}
#'   \item{SECTOR}{Sector of institution (factor)}
#'   \item{LOCALE}{Degree of urbanization (Urban-centric locale) (factor)}
#'   \item{DEGGRANT}{Degree-granting status (factor)}
#'   \item{HLOFFER}{Highest level of offering (factor)}
#'   \item{ENRTOT}{Total  enrollment (numeric)}
#'   \item{EFUG}{Undergraduate enrollment (numeric)}
#'   \item{EFUG1ST}{First-time degree/certificate-seeking undergraduate enrollment (numeric)}
#'   \item{EFUGFT}{Full-time undergraduate enrollment (numeric)}
#'   \item{EFGRAD}{Graduate enrollment (numeric)}
#'   \item{EFGRADFT}{Full-time graduate enrollment (numeric)}
#' }
#' @source National Center for Education Statistics, "Integrated Postsecondary Education Data System " 2023, \url{https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=2023&surveyNumber=-1&sid=4737d338-5121-4355-bb91-01ffa92243ef&rtid=7}, downloaded on May 2, 2025.
"ipeds"
#' @title American Community Survey (ACS) 5-Year Estimates, 2023 for Public Use Microdata Areas (PUMAs)
#' @description A select set of estimates from the ACS for all PUMAs in the 50 states and District of Columbia.
#' @format A tibble/data.frame with 2,462 rows and 25 columns:
#' \describe{
#'   \item{GEOID}{Geographic Identifier (GEOID) of PUMA (character)}
#'   \item{Name}{Name of PUMA (character)}
#'   \item{State}{State (Abbreviation) (character)}
#'   \item{Region}{Census Region (factor)}
#'   \item{Division}{Census Division (factor)}
#'   \item{Pop_Tot}{Total population (B03002) (numeric)}
#'   \item{Pop_Pct_White_NH}{Percent of population who is White alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_Black_NH}{Percent of population who is Black or African American alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_AIAN_NH}{Percent of population who is American Indian and Alaska Native alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_Asian_NH}{Percent of population who is Asian alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_NHPI_NH}{Percent of population who is Native Hawaiian and Other Pacific Islander alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_Other_NH}{Percent of population who is another race (including 2 or more races), non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_Hispanic}{Percent of population who is Hispanic (B03002) (numeric)}
#'   \item{HU_Tot}{Total number of housing units (B25002) (numeric)}
#'   \item{HU_Pct_Occupied}{Percent of housing units that are occupied (B25002) (numeric)}
#'   \item{HU_Pct_Vacant}{Percent of housing units that are vacant (B25002) (numeric)}
#'   \item{Pop_Pct_0004}{Percent of population that are 0-4 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_0509}{Percent of population that are 5-9 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_1014}{Percent of population that are 10-14 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_2544}{Percent of population that are 15-17 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_4564}{Percent of population that are 18-24 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_6574}{Percent of population that are 25-44 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_75plus}{Percent of population that are 45-64 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_1517}{Percent of population that are 65-74 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_1824}{Percent of population that are 75 years of age or older (S0101) (numeric)}
#' }
#' @source U.S. Census Bureau, "American Community Survey 5-Year Estimates" 2023, \url{https://api.census.gov/data/2023/acs/acs5}, accessed on May 30, 2025. Tables B03002, B25002, and S0101. Note - the {tidycensus} package was used to download the data from the Census API.
#' Region and division are coded based on \url{https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf}
"puma_2023"

#' @title American Community Survey (ACS) 5-Year Estimates, 2023 for counties
#' @description A select set of estimates from the ACS for all counties in the 50 states and District of Columbia.
#' @format A tibble/data.frame with 3,144 rows and 25 columns:
#' \describe{
#'   \item{GEOID}{Geographic Identifier (GEOID) of county (character)}
#'   \item{Name}{Name of county (character)}
#'   \item{State}{State (Abbreviation) (character)}
#'   \item{Region}{Census Region (factor)}
#'   \item{Divison}{Census Division (factor)}
#'   \item{Pop_Tot}{Total population (B03002) (numeric)}
#'   \item{Pop_Pct_White_NH}{Percent of population who is White alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_Black_NH}{Percent of population who is Black or African American alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_AIAN_NH}{Percent of population who is American Indian and Alaska Native alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_Asian_NH}{Percent of population who is Asian alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_NHPI_NH}{Percent of population who is Native Hawaiian and Other Pacific Islander alone, non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_Other_NH}{Percent of population who is another race (including 2 or more races), non-Hispanic (B03002) (numeric)}
#'   \item{Pop_Pct_Hispanic}{Percent of population who is Hispanic (B03002) (numeric)}
#'   \item{HU_Tot}{Total number of housing units (B25002) (numeric)}
#'   \item{HU_Pct_Occupied}{Percent of housing units that are occupied (B25002) (numeric)}
#'   \item{HU_Pct_Vacant}{Percent of housing units that are vacant (B25002) (numeric)}
#'   \item{Pop_Pct_0004}{Percent of population that are 0-4 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_0509}{Percent of population that are 5-9 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_1014}{Percent of population that are 10-14 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_2544}{Percent of population that are 15-17 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_4564}{Percent of population that are 18-24 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_6574}{Percent of population that are 25-44 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_75plus}{Percent of population that are 45-64 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_1517}{Percent of population that are 65-74 years of age (S0101) (numeric)}
#'   \item{Pop_Pct_1824}{Percent of population that are 75 years of age or older (S0101) (numeric)}
#' }
#' @source U.S. Census Bureau, "American Community Survey 5-Year Estimates" 2023, \url{https://api.census.gov/data/2023/acs/acs5}, accessed on May 30, 2025. Tables B03002, B25002, and S0101. Note - the {tidycensus} package was used to download the data from the Census API.
#' Region and division are coded based on \url{https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf}
"county_2023"
