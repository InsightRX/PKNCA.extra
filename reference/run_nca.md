# Run a non-compartmental analysis

This function runs an NCA on a pre-parsed dataset. The NCA will be
performed using the PKNCA package. The NCA output object will be saved
to an rds file if `path` is specified.

## Usage

``` r
run_nca(
  data,
  dose_data = NULL,
  specification = "default",
  dictionary = NULL,
  settings = NULL,
  blq = "<",
  groups = NULL,
  include_cols = NULL,
  intervals = data.frame(start = 0, end = Inf),
  partial_auc = NULL,
  exclude_points = NULL,
  exclude_subjects = NULL,
  sequence_from = NULL,
  add_auctau = TRUE,
  post = list(accumulation = list(parameters = c("auctau", "cmax"))),
  no_dots = TRUE,
  path = NULL,
  check_grouping = FALSE,
  verbose = FALSE,
  ...
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

- specification:

  use specific options for the NCA and for mapping of input / output
  data.

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

- blq:

  optional, string to match in concentration data, e.g. "\<" or "BLQ\>.
  If `conc` column in the data is `character` class, then if `blq` is
  not NULL, it will detect any values that match this string and set
  those values to zero. If NULL, or `conc` data is numeric, the data
  will be used as-is.

- groups:

  optional grouping variable, e.g. `ACTARM`.

- include_cols:

  Optional. Character vector of column names in `data` that should be
  merged in with NCA results (including only a single value for each
  row). Merge will happen by subject ID, any supplied groupings, and
  `nca_start` time.

- intervals:

  specify time intervals for NCA calculations. The passed argument
  should be a data.frame, with at least the `start` and `end` time for
  each row that specifies the interval. `Inf` is allowed to specify the
  end of the interval. Example:

      intervals = bind_rows(
         c(start = 0, end = 24),
         c(start = 120, end = Inf)
      )

  The `run_nca()` function will compute all parameters that are
  specified in `settings()` or in the default settings.

- partial_auc:

  a data.frame containing at least the `start` and `end` time for each
  row that specifies the start- and endtime for the partial AUC
  calculation. At least the AUC_0-t (`auclast`) will be calculated for
  each row. Any other parameters specified in the data.frame as column
  will also be calculated, e.g. `cmax=TRUE`. Currently, only a single
  row can be supplied as argument.

- exclude_points:

  list of points to exclude, based on name(s) in the list that should
  match column name in the input dataset. E.g.
  `list("sample_id" = c("12345", "23456"))`. This could similarly be
  used to exclude entire subjects, e.g.
  `list("subject_id = c("103, "105")`

- exclude_subjects:

  description...

- sequence_from:

  optional. Argument should specify the column name that indicates the
  treatment e.g. for food-effect cross-over trials. It will then add
  sequence group and sequence description columns to the output data.

- add_auctau:

  compute AUCtau whenever relevant? TRUE (default) / FALSE. For
  multi-dose PK data, this will add the parameter `auc_tau` to the NCA
  output data as the AUC for each interval. In essence, it will just
  make a copy of the AUClast for the interval. See exact logic in
  function help page.

- post:

  named list of lists, indicating which hard-coded post-processing steps
  to include, and options for each step. Possible post-processing steps
  include:

  - `accumulation`: if NCA calculated for multiple non-overlapping
    intervals, will add accumulation ratios for the parameters listed in
    the `parameters` argument to the NCA output table for each interval.

  Other options may be added in future. Default is:
  `list("accumulation" = list("parameters" = c("auclast", "cmax")))`.

- no_dots:

  if `TRUE` (default) will replace any dots in parameter names (e.g.
  `aucinf.obs`) with underscores (`aucinf_obs`).

- path:

  path to file to store output object from PKNCA as RDS (optional)

- check_grouping:

  check whether the grouping specified in `groups` is likely to be
  correct for NCA, and is not creating groups that are too small for NCA
  to be able to calculate half-life and other parameters. See
  [`check_nca_grouping()`](https://insightrx.github.io/PKNCA.extra/reference/check_nca_grouping.md)
  for details.

- verbose:

  verbose output?

- ...:

  parameters passed to \`PKNCA::pk.nca()“ function.

## Details

There are four possible types of input datasets that can be used to run
the NCA:

1.  Using a separate data.frame for PK data (`data`) and dose data
    (`dose_data`), with each PK or dosing event a row in the respective
    datasedt. Can be used for both single and multi-dose data.

2.  Using a single data.frame for PK and dose data (`data`), with event
    type separated using an evid column (specified in `dictionary`). PK
    data rows should be set to `evid=0`, and doses to `evid=1`. This is
    essentially a NONMEM-style dataset. Rows with other `evid` values
    will be ignored. Can be used for both single and multi-dose data,
    the dosing occasions will be determined from the dosing events. If
    an `evid` column is present in `data`, it will not attempt to use
    approach 3.

3.  Using a single data.frame (`data`) with PK data as rows, and a
    column for dose (with colum name specified in `dictionary`). A
    `dose_data` data.frame will be constructed from the dose column. Can
    only be used for single-dose data, unless `groups` are used to
    separate the dosing occasions.

4.  Using a single data.frame (`data`) with PK data as rows, but no
    dosing info in the data. NCA can still compute AUC and Cmax etc, but
    parameters that require the dose such as CL, V, etc will be returned
    as NA. Can only be used for single dose, unless `groups` are used to
    separate the dosing occasions.

## Examples

``` r
if (FALSE) { # \dontrun{
run_nca(
  data = data,
  path = "./outputs/nca.rds"
)
} # }
```
