# PKNCA.extra (development version)

* `run_nca(partial_auc = ...)` and AUCtau (`nca_add_auctau()`) now use PKNCA's
  `aucint.last.dose` (or `aucint.last` without dose data) instead of `auclast`,
  interpolating to the end of the interval. Previously these were biased low
  when the sample at the end of the interval was drawn late (#19).
* For multi-dose data, AUCtau at an interval ending at the next dose uses
  `aucint.inf.obs.dose` (lambda-z extrapolation to the dose, not using samples
  after it), falling back to `aucint.last.dose`. Dosing intervals that PKNCA
  skips because no sample was taken exactly at a dose time are now added, so
  AUCtau is reported for them (#19). AUCtau is computed in a separate PKNCA
  pass, so it does not add parameters to the main NCA output.
* Initial CRAN submission.
