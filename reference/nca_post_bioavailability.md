# NCA post-processing function to calculate relative bioavailability

Will only be able to calculate relative bioavailability when data has a
grouper to distinguish the test and reference arms, e.g.

## Usage

``` r
nca_post_bioavailability(
  data,
  dictionary,
  options = list(parameters = c("auclast", "cmax"), arm = NULL, reference = NULL),
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

  - `parameters`: vector of parameter names to calculate ratio for.

  - `arm`: the column name used to indicate the varios arms of the
    study, e.g. fasting / fed, or iv / oral, etc. Can have multiple test
    arms, but only one `reference`.

  - `reference`: the label of the reference arm in `group` column, e.g.
    "fasted"

- groups:

  grouping variable

- verbose:

  verbosity
