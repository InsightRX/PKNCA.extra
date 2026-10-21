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
* `nca_add_auctau()` no longer falls back to `auclast` when no interval AUC is
  available, `auctau` is `NA` (with a warning) instead. The `auctau` column is
  added whenever there is an interval with a finite end, also when all values
  are `NA`. The final dosing interval is added also when sampling stopped
  before its end, with AUCtau extrapolated using lambda-z (#19).
* Partial AUC and AUCtau now also extrapolate (`aucint.inf.obs(.dose)`) when
  the end of the interval is after the last measurable sample, instead of
  stopping at the last sample. Excluded samples are not used as boundary
  samples (#19).
* Partial AUC is now computed in a separate PKNCA pass as well, so it is also
  available for multi-dose data, where PKNCA selects intervals from the dosing
  schedule (#19). Only the AUC is reported; other columns in `partial_auc`
  (NCA parameters, `impute`) are ignored with a warning.
* Partial AUC and AUCtau for intervals starting at a dose without a sample at
  that time use the start-of-interval imputation (e.g. C0 back-extrapolation
  for an IV bolus) for the part up to the last measurable sample, since
  PKNCA's `aucint.*` parameters ignore imputation (#19).
* Partial AUC spanning one or more doses is computed per dosing interval part
  and summed, so the concentration just before a dose and the
  start-of-interval imputation after it are handled as for AUCtau (#19).
* For a repeated IV bolus with a (pre-dose) sample at the time of the dose,
  AUCtau and partial AUC back-extrapolate C0 (new imputation method
  `start_c0_logslope`) instead of using the trough as the start of the next
  dosing interval (#19).
* For multi-dose data, `run_nca()` selects the dosing intervals itself (as
  PKNCA does, plus the dosing intervals PKNCA skips), so AUCtau is also
  available when no subject has samples exactly at the dose times (#19).
* Initial CRAN submission.
