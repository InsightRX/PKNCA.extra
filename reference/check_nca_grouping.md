# Check that `grouping` specified to PKNCA is likely to be correct If the grouping results in most groups having too few datapoints to perform NCA and calculate half-life, then the grouping is likely wrong.

Check that `grouping` specified to PKNCA is likely to be correct If the
grouping results in most groups having too few datapoints to perform NCA
and calculate half-life, then the grouping is likely wrong.

## Usage

``` r
check_nca_grouping(
  data,
  groups,
  dictionary,
  settings = list(),
  threshold = 0.7,
  verbose = TRUE
)
```

## Arguments

- data:

  dataset in pre-parse format, preferrably created using
  `create_nca_data()`. Can also be a NONMEM-style dataset with EVID=0\|1
  column indicating PK an dosing data.

- groups:

  optional grouping variable, e.g. `ACTARM`.

- dictionary:

  abridged data dictionary for the input dataset, specified as list
  object. Requires at least entries for `subject_id`, `time`, `conc`,
  `dose`. Optional entries are: `visit`. If required entries are
  specified only partially, then default values (i.e. CDISC)
  nomenclature will be imputed for missing identifiers.

- settings:

  list of settings for NCA. Provided setting names can either be
  settings recognized directly by `pknca`, or settings that are included
  in the map provided by `nca_settings_map()` (which are then mapped to
  `pknca` setting names).

- threshold:

  the fraction of data to be allowed to have too few data points for
  NCA.

- verbose:

  verbose output?

## Value

`TRUE` or `FALSE`
