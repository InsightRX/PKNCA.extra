# Imputation methods for the concentration at the start of an NCA interval

Custom PKNCA imputation methods used by
[`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md).
Unlike PKNCA's built-in `start_conc0`, these methods never overwrite a
concentration that was observed at the start of the interval.

## Usage

``` r
PKNCA_impute_method_start_obs_predose_conc0(
  conc,
  time,
  start = 0,
  end = Inf,
  conc.group = NULL,
  time.group = NULL,
  ...,
  options = list()
)

PKNCA_impute_method_start_obs_c0_logslope(
  conc,
  time,
  start = 0,
  end = Inf,
  ...,
  options = list()
)
```

## Arguments

- conc, time:

  concentration and time in the interval

- start, end:

  start and end time of the interval

- conc.group, time.group:

  all concentrations and times for the group

- ...:

  ignored

- options:

  PKNCA options

## Value

data.frame with columns `conc` and `time`

## Details

- `start_obs_predose_conc0`: keep the observed concentration at the
  interval start if present. Otherwise use the pre-dose concentration
  (PKNCA's `start_predose` logic, i.e. the last sample before the
  interval start within 5% of the interval length). If there is no such
  sample, impute a concentration of 0. Used for extravascular doses and
  infusions.

- `start_obs_c0_logslope`: for IV bolus doses. Keep the observed
  (non-zero) concentration at the interval start if present. Otherwise
  back-extrapolate C0 using a log-linear regression of the first two
  concentrations after the dose
  ([`PKNCA::pk.calc.c0()`](https://humanpred.github.io/pknca/reference/pk.calc.c0.html),
  methods `"c0"`, `"logslope"`, `"c1"`).

The function names follow the PKNCA convention
(`PKNCA_impute_method_<name>`) so they can be referred to as `<name>` in
the `impute` argument of
[`PKNCA::PKNCAdata()`](https://humanpred.github.io/pknca/reference/PKNCAdata.html).
