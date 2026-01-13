# Validate the measure of size (MOS) variable

Ensures that the MOS variable exists in the frame, is numeric, and
contains no missing or negative values.

## Usage

``` r
check_string_mos(mos, frame)
```

## Arguments

- mos:

  A string or symbol representing the column name of the measure of
  size.

- frame:

  The sampling frame as a data.frame or similar object.

## Value

Stops execution if validation fails. Invisibly returns TRUE.
