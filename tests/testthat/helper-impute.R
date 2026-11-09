## One-compartment oral model, q12h x 2
pk_oral <- function(t, D = 100, ka = 1, CL = 5, V = 20) {
  ke <- CL / V
  ifelse(t < 0, 0, D * ka / (V * (ka - ke)) * (exp(-ke * t) - exp(-ka * t)))
}
## One-compartment IV bolus model
pk_iv <- function(t, D = 100, CL = 5, V = 20) {
  D / V * exp(-CL / V * t)
}
dict_evid <- list(subject_id = "ID", time = "TIME", conc = "DV", dose = "AMT", evid = "EVID")

make_multidose_data <- function() {
  tt <- c(0, 0.5, 1, 2, 4, 6, 8, 10, 12, 12.5, 13, 14, 16, 18, 20, 22, 24)
  d <- rbind(
    data.frame(ID = 1, TIME = c(0, 12), DV = 0, EVID = 1, AMT = 100),
    data.frame(ID = 1, TIME = tt, DV = pk_oral(tt) + pk_oral(tt - 12), EVID = 0, AMT = 0)
  )
  d[order(d$TIME, -d$EVID), ]
}

pknca_auclast <- function(conc_data, start, end, impute = NA_character_) {
  x <- PKNCA::PKNCAdata(
    PKNCA::PKNCAconc(conc_data, DV ~ TIME | ID),
    intervals = data.frame(start = start, end = end, auclast = TRUE),
    impute = impute
  )
  res <- as.data.frame(PKNCA::pk.nca(x))
  res$PPORRES[res$PPTESTCD == "auclast"]
}
