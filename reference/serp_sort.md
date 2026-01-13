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
