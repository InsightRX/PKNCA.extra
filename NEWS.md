# PKNCA.extra (development version)

* `run_nca(partial_auc = ...)` and AUCtau (`nca_add_auctau()`) now use PKNCA's
  `aucint.last.dose` (or `aucint.last` without dose data) instead of `auclast`,
  interpolating to the end of the interval. Previously these were biased low
  when the sample at the end of the interval was drawn late (#19).
* Initial CRAN submission.
