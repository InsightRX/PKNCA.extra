# Check for each interval whether a concentration was measured at its end

Only non-missing, non-excluded concentrations are considered.

## Usage

``` r
is_sampled_at_end(output, conc_obj)
```

## Arguments

- output:

  data.frame with `end` and group columns

- conc_obj:

  `PKNCAconc` object

## Value

logical vector, one value per row in `output`
