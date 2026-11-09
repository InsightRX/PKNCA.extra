## PKNCA default `single.dose.aucs`, independent of any options set in this
## session. `PKNCA.options(default = TRUE)` resets the global options, so
## restore them afterwards.
default_aucs <- function() {
  old <- PKNCA::PKNCA.options()
  on.exit(restore_pknca_options(old))
  PKNCA::PKNCA.options(default = TRUE)$single.dose.aucs
}
