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
  exclude_lambda_z = NULL,
  include_lambda_z = NULL,
  units = NULL,
  conversions = NULL,
  sequence_from = NULL,
  add_auctau = TRUE,
  post = list(accumulation = list(parameters = c("auctau", "cmax"))),
  format = c("wide", "long"),
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
  `pknca` setting names). In addition to standard PKNCA settings, the
  following settings are handled internally and not passed to PKNCA:

  - `min.hl.time` (or `minHalfLifeTime`): minimum time required for a
    data point to be eligible for the lambda-z (terminal slope)
    calculation. Any data points with time \< this value are excluded
    from the lambda-z fit (but remain in the analysis for other
    parameters such as Cmax and AUClast). This is implemented by setting
    `exclude_lambda_z` flags internally.

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

  list of points to exclude for entire NCA analysis, based on name(s) in
  the list that should match column name in the input dataset. E.g.
  `list("sample_id" = c("12345", "23456"))`. This could similarly be
  used to exclude entire subjects, e.g.
  `list("subject_id = c("103, "105")`

- exclude_subjects:

  optional, vector of subjects to exclude from NCA

- exclude_lambda_z:

  list of points to exclude but exclusions are only for the calculation
  of lambda-z and downstream parameters (halflife, CL, V, AUCinf etc)
  but not summarization parameters like Cmax, Tmax, etc. List with
  similar structure as `exclude_points`.

- include_lambda_z:

  list of points to include for the calculation of lambda-z and
  downstream parameters (halflife, CL, V, AUCinf etc) but not
  summarization parameters like Cmax, Tmax, etc. List with similar
  structure as `exclude_points`. Please note that for any subject and
  interval where `include_lambda_z` are specified, PKNCA will not run
  the automated curve-stripping procedure and the `exclude_lambda_z`
  argument (if specified) will be ignored for that subject and AUC
  interval.

- units:

  optional units specification, passed to
  [`PKNCA::PKNCAdata()`](http://humanpred.github.io/pknca/reference/PKNCAdata.md).
  Either a named list with elements `concu`, `timeu`, `doseu`, and/or
  `amountu`, which are forwarded to
  [`PKNCA::pknca_units_table()`](http://humanpred.github.io/pknca/reference/pknca_units_table.md)
  internally; or a pre-built data frame from
  [`PKNCA::pknca_units_table()`](http://humanpred.github.io/pknca/reference/pknca_units_table.md).
  When provided, a `units` attribute containing a per-parameter units
  table is attached to the returned data frame.

  To automatically simplify derived units (e.g. V in `L`, CL in `L/h`),
  include `concu_pref` and/or `doseu_pref` in the list — these are
  forwarded to
  [`PKNCA::pknca_units_table()`](http://humanpred.github.io/pknca/reference/pknca_units_table.md)
  and use the `units` R package to compute conversion factors
  automatically. For example:

      units = list(
        concu = "ng/mL", timeu = "h", doseu = "mg",
        concu_pref = "mg/L"   # makes V = mg/(mg/L) = L, CL = L/h
      )

- conversions:

  optional manual unit conversion table — a data.frame with columns
  `PPORRESU` (original unit), `PPSTRESU` (preferred unit), and
  `conversion_factor` (multiply PPORRESU value to obtain PPSTRESU
  value), as accepted by `PKNCA::pknca_units_table(conversions = ...)`.
  This is an alternative to `concu_pref`/`doseu_pref` that does not
  require the `units` R package. Only used when `units` is a named list
  (not a pre-built data.frame). See the *Unit strings and conversions*
  section for format details.

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

- format:

  output as table in wide or long format.

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

## Unit strings and conversions

**Unit string format**

The `concu`, `timeu`, `doseu`, and `amountu` fields in the `units` list
are free-form label strings. PKNCA passes them through as-is to build
composite unit labels for each NCA parameter (e.g. AUC = `timeu*concu`,
V = `doseu/(concu)`, CL = `doseu/(timeu*concu)`). There is no built-in
validation or normalisation, so:

- **Use consistent, conventional abbreviations** throughout: `"mg"` not
  `"MG"` or `"milligram"`, `"h"` not `"hr"` or `"hours"`, `"ng/mL"` not
  `"ng*mL-1"`.

- **Case is significant**: `"mg"` and `"MG"` produce different PPORRESU
  strings, which matter when matching entries in `conversions`.

- **Concentration units** should be written as `"<mass>/<volume>"`, e.g.
  `"ng/mL"`, `"ug/mL"`, `"mg/L"`, `"nmol/L"`.

- **Dose units** may include a body-weight denominator, e.g. `"mg/kg"`.

**Using `conversions` (no `units` package required)**

The `conversions` data.frame maps a raw PPORRESU string to a preferred
display unit with a numeric conversion factor:

|                     |                                                             |
|---------------------|-------------------------------------------------------------|
| Column              | Description                                                 |
| `PPORRESU`          | Exact PPORRESU string as produced by PKNCA (case-sensitive) |
| `PPSTRESU`          | Preferred output unit label                                 |
| `conversion_factor` | Multiply the raw value by this to get the preferred value   |

PPORRESU strings for common NCA parameters follow these patterns (where
the unit variables are exactly the strings you passed in `units`):

|                               |                       |                  |
|-------------------------------|-----------------------|------------------|
| Parameter type                | PPORRESU pattern      | Example          |
| Concentration (Cmax, Ctrough) | `concu`               | `"ng/mL"`        |
| Time (t½, Tmax)               | `timeu`               | `"h"`            |
| AUC                           | `timeu*concu`         | `"h*ng/mL"`      |
| AUMC                          | `timeu^2*concu`       | `"h^2*ng/mL"`    |
| Volume (Vz, Vss)              | `doseu/(concu)`       | `"mg/(ng/mL)"`   |
| Clearance (CL)                | `doseu/(timeu*concu)` | `"mg/(h*ng/mL)"` |

Example — converting V and CL to L / L/h for `concu = "ng/mL"`,
`doseu = "mg"`, `timeu = "h"` (conversion factor = 1000 because 1
mg/(ng/mL) = 1000 L):

    run_nca(data,
      units = list(concu = "ng/mL", timeu = "h", doseu = "mg"),
      conversions = data.frame(
        PPORRESU          = c("mg/(ng/mL)",   "mg/(h*ng/mL)"),
        PPSTRESU          = c("L",            "L/h"),
        conversion_factor = c(1000,           1000)
      )
    )

**Using `concu_pref` / `doseu_pref` (requires the `units` R package)**

As an alternative, PKNCA can derive conversion factors automatically via
the `units` package. Pass `concu_pref` (and/or `doseu_pref`,
`timeu_pref`, `amountu_pref`) inside the `units` list. Unit strings for
`_pref` fields must be recognised by the `units` package (standard SI
abbreviations).

The preferred concentration unit controls the derived units. To obtain V
in L and CL in L/h, choose a `concu_pref` whose mass unit matches
`doseu`:

    # concu = "ng/mL", doseu = "mg" -> set concu_pref = "mg/L"
    # V = mg / (mg/L) = L, CL = mg / (h * mg/L) = L/h
    run_nca(data,
      units = list(concu = "ng/mL", timeu = "h", doseu = "mg",
                   concu_pref = "mg/L")
    )

## Examples

``` r
if (FALSE) { # \dontrun{
run_nca(
  data = data,
  path = "./outputs/nca.rds"
)
} # }
```
