# Create grouping variable

Given a data frame and a vector of variables, creates a new variable
that assigns a unique ID to each group of rows that have the same values
for the given variables. The function will return the data frame with
the new id variable added.

## Usage

``` r
create_group_variable(.data, groupvars, groupid)
```

## Arguments

- .data:

  A data frame, tibble, or data.table. No default.

- groupvars:

  A vector of character variables which are the names of the columns to
  sort on. No default.

- groupid:

  ID grouping variable name. No default.

## Value

A copy of the input data with the addition of the grouping ID variable
