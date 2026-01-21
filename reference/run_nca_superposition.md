# Run an NCA on superpositioned data, either just extrapolated to steady state for the current dose levels, or using linear extrapolation to a different dose and interval.

Run an NCA on superpositioned data, either just extrapolated to steady
state for the current dose levels, or using linear extrapolation to a
different dose and interval.

## Usage

``` r
run_nca_superposition(
  data,
  dose_data = NULL,
  regimen,
  dictionary,
  parameters = c("auclast", "cmax"),
  groups = c(),
  verbose = TRUE
)
```

## Arguments

- data:

  dataset in pre-parse format, preferrably created using
  `create_nca_data()`. Can also be a NONMEM-style dataset with EVID=0\|1
  column indicating PK an dosing data.

- dose_data:

  optional, a data.frame with all dosing info. Useful for multi-dose
  NCAs. Alternative to specifying a NONMEM-style datasett to `data`.

- regimen:

  a list that specifies at least the `interval`, and potentially `dose`.
  E.g. `list(interval = 8)` to calculate steady state concentration data
  using superposition. If a `dose` is also specified, it will scale the
  superposition data linearly compared to the original input `dose`,
  e.g. `list(dose = 500, interval = 12)`.

- dictionary:

  abridged data dictionary for the input dataset, specified as list
  object. Requires at least entries for `subject_id`, `time`, `conc`,
  `dose`. Optional entries are: `visit`. If required entries are
  specified only partially, then default values (i.e. CDISC)
  nomenclature will be imputed for missing identifiers.

- parameters:

  description...

- groups:

  optional grouping variable, e.g. `ACTARM`.

- verbose:

  verbose output?

## Value

a PKNCA output object based on superposition data
