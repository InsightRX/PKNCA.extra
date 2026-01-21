# NCA post-processing function to calculate accumulation rate

Will only be able to calculate accumulation ratio when data is multi-
interval.

## Usage

``` r
nca_post_accumulation(
  data,
  dictionary,
  options = list(parameters = c("auctau", "cmax")),
  groups = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  NCA output data

- dictionary:

  data dictionary

- options:

  options for this function:

  - `parameters`: vector of parameter names to calculate accumulation
    ratios for.

- groups:

  grouping variable

- verbose:

  verbosity

## Value

NCA output data with accumulation rate added.
