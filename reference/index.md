# Package index

## All functions

- [`apply_pknca_options()`](https://insightrx.github.io/PKNCA.extra/reference/apply_pknca_options.md)
  : Read settings from a JSON file, and pass to PKNCA.options()

- [`check_nca_grouping()`](https://insightrx.github.io/PKNCA.extra/reference/check_nca_grouping.md)
  :

  Check that `grouping` specified to PKNCA is likely to be correct If
  the grouping results in most groups having too few datapoints to
  perform NCA and calculate half-life, then the grouping is likely
  wrong.

- [`create_nca_table()`](https://insightrx.github.io/PKNCA.extra/reference/create_nca_table.md)
  : Create a table for NCA results

- [`get_nca_individual_fits()`](https://insightrx.github.io/PKNCA.extra/reference/get_nca_individual_fits.md)
  : Extract individual fit data from a PKNCA results object, and return
  as data.frame convenient for plotting.

- [`get_parameter_names_from_nca_data()`](https://insightrx.github.io/PKNCA.extra/reference/get_parameter_names_from_nca_data.md)
  : Get parameter names from a run_nca() output object

- [`get_pk_parameters_from_pknca_options()`](https://insightrx.github.io/PKNCA.extra/reference/get_pk_parameters_from_pknca_options.md)
  :

  When PKNCA::PKNCA.options() is invoked without arguments, it will
  return a list of its configuration. Among the options is the
  data.frame `single.dose.aucs` which will list the exported PK
  parameters for both the AUC0-24 and AUCinf computations. Wherever the
  parameters are set to TRUE PKNCA will compute those and include in
  output. So in that case we can use those parameters as well and should
  make them available in the output from
  [`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md).

- [`get_treatment_sequence()`](https://insightrx.github.io/PKNCA.extra/reference/get_treatment_sequence.md)
  : Function to generate a sequence variable for e.g. a crossover trial
  design

- [`is_set_to_inf()`](https://insightrx.github.io/PKNCA.extra/reference/is_set_to_inf.md)
  :

  Is value meant to be `Inf`?

- [`is_set_to_missing()`](https://insightrx.github.io/PKNCA.extra/reference/is_set_to_missing.md)
  :

  Is value meant to be `NA`?

- [`map_nca_settings()`](https://insightrx.github.io/PKNCA.extra/reference/map_nca_settings.md)
  : Map user-provided NCA settings to NCA engine (e.g. PKNCA)

- [`nca_add_auctau()`](https://insightrx.github.io/PKNCA.extra/reference/nca_add_auctau.md)
  : Add AUCtau to the NCA output, if multi-dose PK data

- [`nca_admiral`](https://insightrx.github.io/PKNCA.extra/reference/nca_admiral.md)
  : Example data for NCA

- [`nca_post_accumulation()`](https://insightrx.github.io/PKNCA.extra/reference/nca_post_accumulation.md)
  : NCA post-processing function to calculate accumulation rate

- [`nca_post_bioavailability()`](https://insightrx.github.io/PKNCA.extra/reference/nca_post_bioavailability.md)
  : NCA post-processing function to calculate relative bioavailability

- [`parse_data_for_nca()`](https://insightrx.github.io/PKNCA.extra/reference/parse_data_for_nca.md)
  : Parse data into dataset for NCA

- [`parse_exclusions()`](https://insightrx.github.io/PKNCA.extra/reference/parse_exclusions.md)
  :

  Helper function to get a mask based on a list of vectors with
  identifiers. This function is used to gather exclusions in the
  [`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
  function.

- [`parse_pknca_opts_from_spec()`](https://insightrx.github.io/PKNCA.extra/reference/parse_pknca_opts_from_spec.md)
  :

  Get the PKNCA options from a spec file, and parse into expected format
  for
  [`PKNCA::PKNCA.options()`](http://humanpred.github.io/pknca/reference/PKNCA.options.md)

- [`read_spec_from_file()`](https://insightrx.github.io/PKNCA.extra/reference/read_spec_from_file.md)
  : Read specification from metadata file

- [`remap_core()`](https://insightrx.github.io/PKNCA.extra/reference/remap_core.md)
  : Core remapping function

- [`remap_nca_output()`](https://insightrx.github.io/PKNCA.extra/reference/remap_nca_output.md)
  : Function to export output from PKNCA to CDISC- or other mappings

- [`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
  : Run a non-compartmental analysis

- [`run_nca_superposition()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca_superposition.md)
  : Run an NCA on superpositioned data, either just extrapolated to
  steady state for the current dose levels, or using linear
  extrapolation to a different dose and interval.

- [`set_pknca_interval_parameters()`](https://insightrx.github.io/PKNCA.extra/reference/set_pknca_interval_parameters.md)
  :

  Creates a data.frame specifying start and end time, and stores it as
  option `single.dose.auc` in PKNCA.options()
