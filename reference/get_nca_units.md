# Get a table of units from a raw PKNCA object

Get a table of units from a raw PKNCA object

## Usage

``` r
get_nca_units(data, units_col = "PPSTRESU")
```

## Arguments

- data:

  PKNCA output object

- units_col:

  the name of the column holding the units, default is `PPSTRESU`

## Value

data.frame with `name` and `unit` columns
