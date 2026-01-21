# Create a table for NCA results

Confirms with layout of table commonly used in PK reports, i.e. where
stats are outputted in long format using a single column.

## Usage

``` r
create_nca_table(
  nca_data,
  parameters = NULL,
  groups = NULL,
  path = NULL,
  format = c("wide", "long"),
  style = c("simple", "pretty"),
  specification = "default",
  description = TRUE,
  prefix = NULL,
  verbose = TRUE
)
```

## Arguments

- nca_data:

  output data from NCA, generated using
  [`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)

- parameters:

  vector of parameters to output, e.g. `c("AUCALL", "CMAX", "TMAX")`. If
  `NULL`, defaults to output all available parameters.

- groups:

  name of variable in dataset to group statistics by, e.g. `"ACTARM"`

- path:

  optional, path to filename to save output table to.

- format:

  output as table in wide or long format.

- style:

  style of output table. Default is `simple`, in which the output is
  essentially a regular data.frame that can easily be used for further
  parsing. The `pretty` style is intended for printing or inclusion in
  reports: when multiple rows are in the table for a single parameter or
  interval, it will leave blanks on the subsequent rows for parameter
  and description.

- specification:

  specification file with settings for NCA. Needs to match the
  `specification` that was used in
  [`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
  to properly map column names.

- description:

  include description of parameter names into table?

- prefix:

  prefix all column names with this, e.g. `superpos_`.

- verbose:

  Verbose?

## Value

A data.frame or tibble.
