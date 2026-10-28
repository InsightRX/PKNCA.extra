# PKNCA.extra 0.0.0.9022

## Bug fixes

* `run_nca()` no longer modifies the global `PKNCA::PKNCA.options()`. The
  options in effect before the call are restored on exit (also on error).
  Previously, the NCA specification and settings applied by `run_nca()` leaked
  into the session, so that later code reading `PKNCA.options()` (including
  `get_pk_parameters_from_pknca_options()` with `options = NULL` or with
  `PKNCA.options()$single.dose.aucs` passed explicitly) depended on which
  analyses had been run before. This made results order-dependent, e.g. a test
  of `get_pk_parameters_from_pknca_options()` could return
  `c("auclast", "aucinf_obs")` instead of the PKNCA default
  `c("auclast", "cmax", "tmax", "half_life", "aucinf_obs")` when run after
  another test that called `run_nca()`.
* `get_pk_parameters_from_pknca_options()`:
  * only considers logical columns as parameter columns. Previously an interval
    with `start = 1` or `end = 1` would return `"start"`/`"end"` as parameter
    names.
  * returns each parameter only once, in order of first appearance.
  * handles specifications with zero intervals (returns `character(0)`).
  * validates its `options` and `no_dots` arguments.
  * documentation now correctly states that `no_dots` defaults to `FALSE`.
* `run_nca()` called `get_treatment_sequence()` using partially matched
  argument names (`dat`, `group`); now uses the full names (`data`, `groups`).

* `parse_data_for_nca()` is now reproducible. The simulated actual time since
  last dose (`ATSLD`) was drawn with `rnorm()` from the session's random number
  stream, so output differed between calls. It is now generated using a fixed
  seed (new argument `seed`, default `12345`) with R's default random number
  generators, and the session's random number state is left untouched. Use
  `seed = NULL` for the previous behavior.

## Other changes

* New internal helper `restore_pknca_options()`. It restores the options
  atomically (reset to defaults, then a single validated set of all changed
  options), restores options that are `NULL`, and does not replay PKNCA
  validation warnings for saved values (e.g. `min.span.ratio < 2`), also when
  `options(warn = 2)` is set.
* Minimum R version is now 3.6.0, required for reproducible seeding in
  `parse_data_for_nca()` (`set.seed(sample.kind = )`).
* Fixed R CMD check NOTEs for undefined global variables in
  `check_nca_grouping()`, `create_nca_table()`, `get_nca_individual_fits()`
  and `run_nca()` (no change in behavior).
* `CLAUDE.md` and `.claude/` are excluded from the package build.
* Added tests for `get_pk_parameters_from_pknca_options()`,
  `set_pknca_interval_parameters()`, `restore_pknca_options()` and
  `parse_data_for_nca()`. Tests that modify PKNCA options now restore them.
* Tests no longer depend on execution order: shared fixtures and helper
  functions moved from top-level code in test files to `helper-*.R` files, and
  test data is created inside each test. Verified with shuffled test order,
  random file order, and each test file run in isolation.

# PKNCA.extra 0.0.0.9021

* `run_nca(partial_auc = ...)` now reports PKNCA's `aucint.last.dose`
  (`aucint.last` without dose data) instead of `auclast`, interpolating to the
  end of the interval. AUCtau uses `aucint.last.dose` for intervals without a
  concentration measured exactly at the end of the interval, and `auclast`
  otherwise. Previously both were biased low when the sample at the end of
  the interval was drawn late. `nca_add_auctau()` gains an `auc_col`
  argument (#19).
* `run_nca()` gives a clear error when PKNCA cannot select any interval from
  the dosing schedule, and warns when `partial_auc` is not computed for
  multi-dose data (#19).
* Initial CRAN submission.
