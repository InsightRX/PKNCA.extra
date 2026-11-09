# Get the PK parameters requested in a PKNCA interval specification

The PKNCA option `single.dose.aucs` (see
[`PKNCA::PKNCA.options()`](https://humanpred.github.io/pknca/reference/PKNCA.options.html))
is a data.frame with one row per NCA interval (`start`, `end`) and one
logical column per PK parameter. Wherever a parameter is set to `TRUE`,
PKNCA will compute it for that interval. This function returns the names
of all parameters requested for at least one interval, so that they can
be made available in the output from
[`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md).

## Usage

``` r
get_pk_parameters_from_pknca_options(options = NULL, no_dots = FALSE)
```

## Arguments

- options:

  data.frame with interval specification for NCA, in the format of
  `PKNCA::PKNCA.options()$single.dose.aucs`. If `NULL` (default), the
  currently active PKNCA option `single.dose.aucs` is used.

- no_dots:

  if `TRUE`, will replace any dots in parameter names (e.g.
  `aucinf.obs`) with underscores (`aucinf_obs`). Default is `FALSE`,
  i.e. PKNCA parameter names are returned unchanged.

## Value

character vector of PK parameter names (possibly of length 0).

## Details

Only logical columns are considered parameter columns. Other columns
(e.g. `start`, `end`, `impute`) are ignored, regardless of their values.
`NA` values are treated as not requested.

Parameters are returned in order of first appearance, scanning intervals
(rows) in order and, within each interval, parameter columns from left
to right. Each parameter is returned only once.

The result depends only on `options`. When `options` is `NULL`, the
*currently active* PKNCA options are used, i.e.
`PKNCA::PKNCA.options()$single.dose.aucs`, so calling the function with
`NULL` or with that value explicitly gives identical results. Note that
[`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
restores the PKNCA options on exit, so calling
[`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
does not change the result of a later call with `options = NULL`.
