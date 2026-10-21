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

test_that("observed pre-dose trough is not overwritten in multi-dose intervals", {
  d <- make_multidose_data()
  res <- run_nca(d, dictionary = dict_evid, add_auctau = TRUE)
  expected <- pknca_auclast(d[d$EVID == 0, ], start = 12, end = 24)
  expect_equal(res$auclast[res$nca_start == 12], expected)
  expect_equal(res$auctau[res$nca_start == 12], expected)
  ## the old behavior (start_conc0) replaced the trough with 0
  biased <- pknca_auclast(d[d$EVID == 0, ], start = 12, end = 24, impute = "start_conc0")
  expect_true(res$auclast[res$nca_start == 12] > biased)
})

test_that("concentration of 0 is imputed at start when no sample is available", {
  tt <- c(0.5, 1, 2, 4, 6, 8, 12)
  d <- rbind(
    data.frame(ID = 1, TIME = 0, DV = 0, EVID = 1, AMT = 100),
    data.frame(ID = 1, TIME = tt, DV = pk_oral(tt), EVID = 0, AMT = 0)
  )
  res <- run_nca(d, dictionary = dict_evid, intervals = data.frame(start = 0, end = 12))
  expected <- pknca_auclast(d[d$EVID == 0, ], start = 0, end = 12, impute = "start_conc0")
  expect_equal(res$auclast, expected)
})

test_that("distant earlier sample is not used as pre-dose concentration", {
  d <- make_multidose_data()
  d <- d[!(d$EVID == 0 & d$TIME %in% c(10, 12)), ]
  res <- run_nca(d, dictionary = dict_evid, add_auctau = TRUE)
  ## no sample within 0.6 h before t=12, so conc=0 is imputed at start
  conc_data <- d[d$EVID == 0 & d$TIME > 12, ]
  expected <- pknca_auclast(conc_data, start = 12, end = 24, impute = "start_conc0")
  expect_equal(res$auclast[res$nca_start == 12], expected)
})

test_that("observed C0 for IV bolus is not overwritten", {
  tt <- c(0, 0.5, 1, 2, 4, 6, 8, 12)
  d <- rbind(
    data.frame(ID = 1, TIME = 0, DV = 0, EVID = 1, AMT = 100),
    data.frame(ID = 1, TIME = tt, DV = pk_iv(tt), EVID = 0, AMT = 0)
  )
  d <- d[order(d$TIME, -d$EVID), ]
  res <- run_nca(d, dictionary = dict_evid, intervals = data.frame(start = 0, end = 12))
  expected <- pknca_auclast(d[d$EVID == 0, ], start = 0, end = 12)
  expect_equal(res$auclast, expected)
})

test_that("C0 is back-extrapolated for IV bolus doses when route is specified", {
  tt <- c(0.5, 1, 2, 4, 6, 8, 12)
  d <- rbind(
    data.frame(ID = 1:2, TIME = 0, DV = 0, EVID = 1, AMT = 100, ROUTE = c("IV", "ORAL")),
    data.frame(
      ID = rep(1:2, each = length(tt)),
      TIME = c(tt, tt),
      DV = c(pk_iv(tt), pk_oral(tt)),
      EVID = 0, AMT = 0,
      ROUTE = rep(c("IV", "ORAL"), each = length(tt))
    )
  )
  d <- d[order(d$ID, d$TIME, -d$EVID), ]
  res <- run_nca(
    d,
    dictionary = c(dict_evid, route = "ROUTE"),
    intervals = data.frame(start = 0, end = 12)
  )
  ## log-linear back-extrapolation is exact for mono-exponential decline
  expect_equal(res$auclast[res$ID == 1], integrate(pk_iv, 0, 12)$value, tolerance = 1e-4)
  ## oral subject still gets conc=0 at start
  expected_oral <- pknca_auclast(d[d$EVID == 0 & d$ID == 2, ], start = 0, end = 12, impute = "start_conc0")
  expect_equal(res$auclast[res$ID == 2], expected_oral)

  ## without route, IV bolus is treated as extravascular (C0 = 0)
  res_noroute <- run_nca(d, dictionary = dict_evid, intervals = data.frame(start = 0, end = 12))
  expect_true(res_noroute$auclast[res_noroute$ID == 1] < res$auclast[res$ID == 1])
})

test_that("IV infusion (non-zero duration) is not back-extrapolated", {
  tt <- c(0.5, 1, 2, 4, 6, 8, 12)
  d <- rbind(
    data.frame(ID = 1, TIME = 0, DV = 0, EVID = 1, AMT = 100, ROUTE = "IV", DUR = 1),
    data.frame(ID = 1, TIME = tt, DV = pk_iv(tt), EVID = 0, AMT = 0, ROUTE = "IV", DUR = 1)
  )
  res <- run_nca(
    d,
    dictionary = c(dict_evid, route = "ROUTE", duration = "DUR"),
    intervals = data.frame(start = 0, end = 12)
  )
  expected <- pknca_auclast(d[d$EVID == 0, ], start = 0, end = 12, impute = "start_conc0")
  expect_equal(res$auclast, expected)
})

test_that("user-specified impute column in intervals is respected", {
  d <- make_multidose_data()
  res <- run_nca(
    d,
    dictionary = dict_evid,
    intervals = data.frame(start = 0, end = Inf, impute = "start_conc0")
  )
  expected <- pknca_auclast(d[d$EVID == 0, ], start = 12, end = 24, impute = "start_conc0")
  expect_equal(res$auclast[res$nca_start == 12], expected)
})

test_that("start_obs_predose_conc0 imputation method", {
  ## observed concentration at start is kept
  out <- PKNCA_impute_method_start_obs_predose_conc0(
    conc = c(2, 5, 3), time = c(12, 13, 14), start = 12, end = 24,
    conc.group = c(0, 5, 3, 2, 5, 3), time.group = c(0, 1, 2, 12, 13, 14)
  )
  expect_equal(out$conc, c(2, 5, 3))
  expect_equal(out$time, c(12, 13, 14))

  ## pre-dose sample (within 5% of interval) is shifted to start
  out <- PKNCA_impute_method_start_obs_predose_conc0(
    conc = c(5, 3), time = c(13, 14), start = 12, end = 24,
    conc.group = c(0, 5, 3, 2, 5, 3), time.group = c(0, 1, 2, 11.9, 13, 14)
  )
  expect_equal(out$conc, c(2, 5, 3))
  expect_equal(out$time, c(12, 13, 14))

  ## pre-dose sample outside the 5% window (0.6 h for 12-24 h) is not used,
  ## also when the interval does not start at time 0
  out <- PKNCA_impute_method_start_obs_predose_conc0(
    conc = c(5, 3), time = c(13, 14), start = 12, end = 24,
    conc.group = c(0, 5, 3, 2, 5, 3), time.group = c(0, 1, 2, 8, 13, 14)
  )
  expect_equal(out$conc, c(0, 5, 3))
  expect_equal(out$time, c(12, 13, 14))

  ## window for open-ended interval is based on the last sample in the interval
  out <- PKNCA_impute_method_start_obs_predose_conc0(
    conc = c(5, 3), time = c(13, 32), start = 12, end = Inf,
    conc.group = c(0, 5, 2, 5, 3), time.group = c(0, 1, 11, 13, 32)
  )
  expect_equal(out$conc, c(2, 5, 3))
  out <- PKNCA_impute_method_start_obs_predose_conc0(
    conc = c(5, 3), time = c(13, 32), start = 12, end = Inf,
    conc.group = c(0, 5, 2, 5, 3), time.group = c(0, 1, 10.9, 13, 32)
  )
  expect_equal(out$conc, c(0, 5, 3))

  ## no sample at or just before start: impute 0
  out <- PKNCA_impute_method_start_obs_predose_conc0(
    conc = c(5, 3), time = c(1, 2), start = 0, end = 24,
    conc.group = c(5, 3), time.group = c(1, 2)
  )
  expect_equal(out$conc, c(0, 5, 3))
  expect_equal(out$time, c(0, 1, 2))

  ## missing concentration at start is treated as not observed
  out <- PKNCA_impute_method_start_obs_predose_conc0(
    conc = c(NA, 5, 3), time = c(0, 1, 2), start = 0, end = 24,
    conc.group = c(NA, 5, 3), time.group = c(0, 1, 2)
  )
  expect_equal(out$conc, c(0, 5, 3))
})

test_that("start_obs_c0_logslope imputation method", {
  ## observed C0 is kept
  out <- PKNCA_impute_method_start_obs_c0_logslope(conc = c(10, 5, 2.5), time = c(0, 1, 2), start = 0)
  expect_equal(out$conc, c(10, 5, 2.5))

  ## back-extrapolated from first two points
  out <- PKNCA_impute_method_start_obs_c0_logslope(conc = c(5, 2.5), time = c(1, 2), start = 0)
  expect_equal(out$conc, c(10, 5, 2.5))
  expect_equal(out$time, c(0, 1, 2))

  ## zero at start is replaced by back-extrapolated C0
  out <- PKNCA_impute_method_start_obs_c0_logslope(conc = c(0, 5, 2.5), time = c(0, 1, 2), start = 0)
  expect_equal(out$conc, c(10, 5, 2.5))
})
