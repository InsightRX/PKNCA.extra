# PKNCA.extra (development version)

* `run_nca(partial_auc = ...)` now reports PKNCA's `aucint.last.dose`
  (`aucint.last` without dose data) instead of `auclast`, interpolating to the
  end of the interval. AUCtau uses `aucint.last.dose` for intervals without a
  concentration measured exactly at the end of the interval, and `auclast`
  otherwise. Previously both were biased low when the sample at the end of
  the interval was drawn late. `nca_add_auctau()` gains an `auc_col`
  argument (#19).
* Initial CRAN submission.
