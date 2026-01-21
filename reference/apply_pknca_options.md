# Read settings from a JSON file, and pass to PKNCA.options()

Read settings from a JSON file, and pass to PKNCA.options()

## Usage

``` r
apply_pknca_options(options, nca_parameters = NULL, verbose = TRUE)
```

## Arguments

- options:

  a list of PKNCA options to be set

- nca_parameters:

  list of what parameters to calculate, e.g.
  `list(auclast=TRUE, cmax=FALSE, ...)`

- verbose:

  show verbose output?

## Value

The current PKNCA options.
