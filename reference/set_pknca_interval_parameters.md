# Creates a data.frame specifying start and end time, and stores it as option `single.dose.auc` in PKNCA.options()

Creates a data.frame specifying start and end time, and stores it as
option `single.dose.auc` in PKNCA.options()

## Usage

``` r
set_pknca_interval_parameters(time, parameters)
```

## Arguments

- time:

  description...

- parameters:

  list of parameters to calculate, indicated by TRUE and FALSE, e.g.
  `list(auc.inf.pred=TRUE, cmax.pred=FALSE, ...)`
