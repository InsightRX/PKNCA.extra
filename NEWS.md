# PKNCA.extra (development version)

* `run_nca(partial_auc = ...)` and AUCtau (`nca_add_auctau()`) now use PKNCA's
  `aucint.last.dose` (or `aucint.last` without dose data) instead of `auclast`,
  interpolating to the end of the interval. Previously these were biased low
  when the sample at the end of the interval was drawn late (#19).
* For multi-dose data, AUCtau at an interval ending at the next dose without a
  sample at that time uses `aucint.inf.obs.dose` (lambda-z extrapolation to the
  dose, not using samples after it). If lambda-z cannot be estimated, AUCtau is
  `NA` with a warning. Dosing intervals that PKNCA
  skips because no sample was taken exactly at a dose time are now added, so
  AUCtau is reported for them (#19). AUCtau is computed in a separate PKNCA
  pass, so it does not add parameters to the main NCA output.
* Partial AUC and AUCtau now also extrapolate (`aucint.inf.obs(.dose)`) when
  the end of the interval is after the last measurable sample, instead of
  stopping at the last sample. Excluded samples are not used as boundary
  samples (#19).
* Partial AUC is now computed in a separate PKNCA pass as well, so it is also
  available for multi-dose data, where PKNCA selects intervals from the dosing
  schedule (#19). Only the AUC is reported; other columns in `partial_auc`
  (NCA parameters, `impute`) are ignored with a warning.
* Initial CRAN submission.
