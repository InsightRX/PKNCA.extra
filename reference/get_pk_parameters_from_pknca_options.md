# When PKNCA::PKNCA.options() is invoked without arguments, it will return a list of its configuration. Among the options is the data.frame `single.dose.aucs` which will list the exported PK parameters for both the AUC0-24 and AUCinf computations. Wherever the parameters are set to TRUE PKNCA will compute those and include in output. So in that case we can use those parameters as well and should make them available in the output from `run_nca()`.

When PKNCA::PKNCA.options() is invoked without arguments, it will return
a list of its configuration. Among the options is the data.frame
`single.dose.aucs` which will list the exported PK parameters for both
the AUC0-24 and AUCinf computations. Wherever the parameters are set to
TRUE PKNCA will compute those and include in output. So in that case we
can use those parameters as well and should make them available in the
output from
[`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md).

## Usage

``` r
get_pk_parameters_from_pknca_options(options = NULL, no_dots = FALSE)
```

## Arguments

- options:

  parameters for interval specification for NCA

- no_dots:

  if `TRUE` (default) will replace any dots in parameter names (e.g.
  `aucinf.obs`) with underscores (`aucinf_obs`).
