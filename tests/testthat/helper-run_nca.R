make_late_boundary_data <- function(dose_times = 0) {
  subj <- data.frame(ID = 1:5, CL = c(4, 5, 6, 4.5, 5.5), V = c(18, 20, 22, 19, 21))
  late <- c(0.25, 0.40, 0.15, 0.55, 0.30)
  nom  <- c(0, 0.5, 1, 2, 4, 6, 8, 10, 12, 16, 24)
  conc <- do.call(rbind, lapply(1:5, function(i) {
    t <- nom
    t[nom == 12] <- 12 + late[i]
    k <- subj$CL[i] / subj$V[i]
    dv <- sapply(t, function(tt) sum(100 / subj$V[i] * exp(-k * (tt - dose_times[dose_times <= tt]))))
    data.frame(ID = i, TIME = t, DV = dv, AMT = 0, EVID = 0)
  }))
  dose <- expand.grid(ID = 1:5, TIME = dose_times)
  dose <- data.frame(ID = dose$ID, TIME = dose$TIME, DV = 0, AMT = 100, EVID = 1)
  d <- rbind(dose, conc)
  d <- d[order(d$ID, d$TIME, -d$EVID), ]
  list(data = d, subj = subj)
}
dict_late_boundary <- list(subject_id = "ID", time = "TIME", conc = "DV", dose = "AMT", evid = "EVID")
