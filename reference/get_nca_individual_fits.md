# Extract individual fit data from a PKNCA results object, and return as data.frame convenient for plotting.

Extract individual fit data from a PKNCA results object, and return as
data.frame convenient for plotting.

## Usage

``` r
get_nca_individual_fits(nca_obj = NULL, n_grid_points = 6, dictionary = NULL)
```

## Arguments

- nca_obj:

  object returned from PKNCA with NCA results.

- n_grid_points:

  number of grid points to simulate for the fitted line. Usually 40 is
  more than enough.

- dictionary:

  list object, mapping column names to expected variables. Any specified
  elements, if present in `nca_obj$data$conc$data`, will be exported in
  the output object.

## Value

A data.frame or tibble.
