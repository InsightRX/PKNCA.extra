# Changelog

## PKNCA.extra (development version)

- `run_nca(partial_auc = ...)` now reports PKNCA’s `aucint.last.dose`
  (`aucint.last` without dose data) instead of `auclast`, interpolating
  to the end of the interval. AUCtau uses `aucint.last.dose` for
  intervals without a concentration measured exactly at the end of the
  interval, and `auclast` otherwise. Previously both were biased low
  when the sample at the end of the interval was drawn late.
  [`nca_add_auctau()`](https://insightrx.github.io/PKNCA.extra/reference/nca_add_auctau.md)
  gains an `auc_col` argument (#19).
- [`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
  gives a clear error when PKNCA cannot select any interval from the
  dosing schedule, and warns when `partial_auc` is not computed for
  multi-dose data (#19).
- Initial CRAN submission.
