# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Common Commands

``` r
devtools::load_all('.')   # Load package locally (always do this before testing manually)
devtools::test()          # Run full test suite
devtools::check()         # Full R CMD check
devtools::document()      # Regenerate roxygen2 docs
```

Run a single test file:

``` r
devtools::load_all('.'); testthat::test_file("tests/testthat/test_run_nca.R")
```

Run a specific test by name:

``` r
devtools::load_all('.'); testthat::test_file("tests/testthat/test_run_nca.R", filter = "units")
```

**Important:** Always use `devtools::load_all('.')` before running
Rscript tests — the installed package may be out of date.

## Development Rules

- **Version bump:** Update the package version in `DESCRIPTION` for
  every code change.
- **Documentation:** After changing any function documentation (roxygen2
  comments), run `devtools::document()` to regenerate the docs.

## Architecture Overview

PKNCA.extra wraps [PKNCA](https://github.com/billdenney/pknca) to
simplify non-compartmental analysis (NCA) workflows. The main data flow
is:

1.  **Input parsing** —
    [`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
    accepts multiple data formats: separate PK/dose data frames,
    NONMEM-style EVID column, dose column on PK rows, or PK-only. It
    normalizes these into
    [`PKNCA::PKNCAconc`](http://humanpred.github.io/pknca/reference/PKNCAconc.md)
    /
    [`PKNCA::PKNCAdata`](http://humanpred.github.io/pknca/reference/PKNCAdata.md)
    objects.
2.  **Exclusion handling** —
    [`parse_exclusions()`](https://insightrx.github.io/PKNCA.extra/reference/parse_exclusions.md)
    turns named lists of sample/subject IDs into boolean mask columns.
    `exclude_lambda_z` / `include_lambda_z` map to `exclude_half.life` /
    `include_half.life` columns passed to `PKNCAconc()`.
3.  **NCA execution** —
    [`PKNCA::pk.nca()`](http://humanpred.github.io/pknca/reference/pk.nca.md)
    runs the analysis. Settings are loaded from JSON spec files in
    `inst/md/` (default: `default.json`) and mapped to PKNCA parameter
    names by
    [`map_nca_settings()`](https://insightrx.github.io/PKNCA.extra/reference/map_nca_settings.md).
4.  **Post-processing** —
    [`nca_post_accumulation()`](https://insightrx.github.io/PKNCA.extra/reference/nca_post_accumulation.md),
    [`nca_post_bioavailability()`](https://insightrx.github.io/PKNCA.extra/reference/nca_post_bioavailability.md),
    [`nca_add_auctau()`](https://insightrx.github.io/PKNCA.extra/reference/nca_add_auctau.md)
    add derived parameters.
5.  **Output formatting** —
    [`create_nca_table()`](https://insightrx.github.io/PKNCA.extra/reference/create_nca_table.md)
    produces wide/long tables with optional pretty-printing;
    [`remap_nca_output()`](https://insightrx.github.io/PKNCA.extra/reference/remap_nca_output.md)
    translates PKNCA names to CDISC or custom names.

### Key files

| File                          | Role                                                                        |
|-------------------------------|-----------------------------------------------------------------------------|
| `R/run_nca.R`                 | Main entry point (~713 lines); all NCA orchestration                        |
| `R/get_nca_individual_fits.R` | Extracts per-point data with `used_in_fit` flag for terminal-phase plotting |
| `R/create_nca_table.R`        | Wide/long output table generation                                           |
| `R/parse_exclusions.R`        | Converts exclusion lists to boolean mask columns                            |
| `R/nca_post.R`                | Accumulation & bioavailability post-processing                              |
| `inst/md/default.json`        | Default NCA settings & parameter list                                       |
| `inst/md/cdisc.json`          | CDISC parameter name mappings                                               |

### Lambda-z point selection

[`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
accepts `include_lambda_z` / `exclude_lambda_z` (named lists of sample
IDs). These are stored as `include_hl` / `exclude_hl` columns on the PK
data and passed to `PKNCAconc(include_half.life=, exclude_half.life=)`.
PKNCA records the column names in
`nca_obj$data$conc$columns$include_half.life` / `$exclude_half.life`.

[`get_nca_individual_fits()`](https://insightrx.github.io/PKNCA.extra/reference/get_nca_individual_fits.md)
reads those column names back to reconstruct `used_in_fit` per point: -
Points in `exclude_hl` are excluded from candidates entirely. - Groups
with any `include_hl = TRUE` use `include_hl` directly as
`used_in_fit`. - Otherwise the last `n_points` candidates from the tail
of the time series are used.

**Known limitation:** If `include_lambda_z` is specified for any
subject, PKNCA disables automatic curve-stripping for subjects with
all-FALSE `include_hl`, so those subjects lose lambda-z.

### Test conventions

- Primary test dataset: `nca_admiral` (168 subjects, CDISC ADMIRAL
  format, 14 timepoints each).
- Each `test_that` block should define
  `dat <- nca_admiral; ids <- unique(dat$USUBJID)` locally.
- `tests/testthat/helper.R` provides `local_set_global_plot_theme()` and
  `local_create_sqlite_db()`.
