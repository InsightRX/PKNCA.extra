# Creates a data.frame specifying start and end time, and stores it as option `single.dose.aucs` in PKNCA.options()

Note: this modifies the global PKNCA options for the current session.
Save the options beforehand with
[`PKNCA::PKNCA.options()`](https://humanpred.github.io/pknca/reference/PKNCA.options.html)
and restore them with
[`restore_pknca_options()`](https://insightrx.github.io/PKNCA.extra/reference/restore_pknca_options.md)
if this side effect is not desired.

## Usage

``` r
set_pknca_interval_parameters(time, parameters)
```

## Arguments

- time:

  numeric vector of length 2, with the start and end time of the NCA
  interval.

- parameters:

  list of parameters to calculate, indicated by TRUE and FALSE, e.g.
  `list(auc.inf.pred=TRUE, cmax.pred=FALSE, ...)`

## Value

The updated PKNCA options (invisibly), as returned by
[`PKNCA::PKNCA.options()`](https://humanpred.github.io/pknca/reference/PKNCA.options.html).
