test_that("create_nca_table gives expected result", {
  dat <- data.frame(
    USUBJID = c("A", "B", "C", "D", "E", "F"),
    ACTARM = c(
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X Low Dose",
      "Drug X Low Dose"
    ),
    nca_start = 0,
    nca_end = 24,
    nca_interval = rep("0 - 24", 6),
    CMAX = c(4.3, 4.9, 6.8, 2.2, 4.2, 3.9),
    TMAX = c(7.8, 1.5, 3.8, 1.4, 4, 4),
    AUCALL = c(75.8, 41.4, 106.9, 26.6, 47.9, 38.6),
    LAMZHL = c(12.4, 8.3, 12.2, 8, 26.1, 21.1),
    AUCLST = c(75.1, 41.5, 109.3, 27.7, 47.3, 42.4),
    CLST = c(0.4, 0.1, 0.6, 0.1, 0.2, 0.1),
    LAMZ = c(0.1, 0.1, 0.1, 0.1, 0, 0),
    LAMZNPT = c(5.2, 8.6, 6.7, 9.1, 3.1, 3.1)
  )
  attr(dat, "PKNCA_object") <- list(result = list(
    PPTESTCD = c("CMAX", "TMAX", "AUCALL", "LAMZHL", "AUCLST", "CLST", "LAMZ", "LAMZNPT"))
  )
  # attr(dat, "parameters") <- c("CMAX", "TMAX", "AUCALL", "LAMZHL", "AUCLST", "CLST", "LAMZ", "LAMZNPT")
  res <- create_nca_table(
    dat, 
    specification = "cdisc",
    format = "long"
  )
  expect_true(is.data.frame(res))
  expect_named(res, c("Parameter", "Description", "Interval", "Statistic", "value"))
  expect_equal(nrow(res), 80)
  expect_true(all(c("AUC (extrapolated to zero from last obs)", "AUC to last obs") %in% res$Description))
  expect_equal(
    as.numeric(res[res$Parameter == "AUCALL" & res$Statistic == "arithm_mean", ]$value),
    56.2
  )

  res2 <- create_nca_table(
    dat,
    parameters = c("CMAX", "TMAX", "AUCALL", "LAMZHL", "AUCLST", "CLST", "LAMZ", "LAMZNPT"),
    specification = "cdisc",
    format = "wide"
  )
  expect_named(
    res2,
    c("Parameter", "Description", "Interval", "geom_mean", "geom_cv_pct", 
      "arithm_mean", "arithm_sd", "arithm_cv_pct", "median", "pct_5", 
      "pct_95", "min", "max")
  )
  expect_equal(
    head(data.frame(res2), 1),
    structure(list(Parameter = "AUCALL", Description = "AUC (extrapolated to zero from last obs)", 
                   Interval = "0 - 24", geom_mean = 50.46, geom_cv_pct = 47.26, 
                   arithm_mean = 56.2, arithm_sd = 29.76, arithm_cv_pct = 52.96, 
                   median = 44.65, pct_5 = c(`5%` = 29.6), pct_95 = c(`95%` = 99.13), 
                   min = 26.6, max = 106.9), row.names = 1L, class = "data.frame")
  )
})
  
test_that("Specification file is respected", {
  dat <- data.frame(
    subject_id = c("A", "B", "C", "D", "E", "F"),
    ACTARM = c(
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X Low Dose",
      "Drug X Low Dose"
    ),
    nca_start = 0,
    nca_end = Inf,
    nca_interval = rep("0 - Inf", 6),
    cmax = c(4.3, 4.9, 6.8, 2.2, 4.2, 3.9),
    tmax = c(7.8, 1.5, 3.8, 1.4, 4, 4),
    aucall = c(75.8, 41.4, 106.9, 26.6, 47.9, 38.6),
    lamba.z = c(12.4, 8.3, 12.2, 8, 26.1, 21.1),
    auclast = c(75.1, 41.5, 109.3, 27.7, 47.3, 42.4),
    lambda.z.n.points = c(5.2, 8.6, 6.7, 9.1, 3.1, 3.1)
  )
  res <- create_nca_table(
    dat,
    parameters = c("cmax", "tmax", "auclast"),  
    format = "long",
    description = TRUE
  )
  expect_named(res, c("Parameter", "Description", "Interval", "Statistic", "value"))
  expect_equal(data.frame(res),
               structure(
                 list(
                   Parameter = c(
                     "auclast",
                     "auclast",
                     "auclast",
                     "auclast",
                     "auclast",
                     "auclast",
                     "auclast",
                     "auclast",
                     "auclast",
                     "auclast",
                     "cmax",
                     "cmax",
                     "cmax",
                     "cmax",
                     "cmax",
                     "cmax",
                     "cmax",
                     "cmax",
                     "cmax",
                     "cmax",
                     "tmax",
                     "tmax",
                     "tmax",
                     "tmax",
                     "tmax",
                     "tmax",
                     "tmax",
                     "tmax",
                     "tmax",
                     "tmax"
                   ),
                   Description = c(
                     "AUC to last obs",
                     "AUC to last obs",
                     "AUC to last obs",
                     "AUC to last obs",
                     "AUC to last obs",
                     "AUC to last obs",
                     "AUC to last obs",
                     "AUC to last obs",
                     "AUC to last obs",
                     "AUC to last obs",
                     "Maximum conc",
                     "Maximum conc",
                     "Maximum conc",
                     "Maximum conc",
                     "Maximum conc",
                     "Maximum conc",
                     "Maximum conc",
                     "Maximum conc",
                     "Maximum conc",
                     "Maximum conc",
                     "Time of maximum conc",
                     "Time of maximum conc",
                     "Time of maximum conc",
                     "Time of maximum conc",
                     "Time of maximum conc",
                     "Time of maximum conc",
                     "Time of maximum conc",
                     "Time of maximum conc",
                     "Time of maximum conc",
                     "Time of maximum conc"
                   ),
                   Interval = c(
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf",
                     "0 - Inf"
                   ),
                   Statistic = c(
                     "geom_mean",
                     "geom_cv_pct",
                     "arithm_mean",
                     "arithm_sd",
                     "arithm_cv_pct",
                     "median",
                     "pct_5",
                     "pct_95",
                     "min",
                     "max",
                     "geom_mean",
                     "geom_cv_pct",
                     "arithm_mean",
                     "arithm_sd",
                     "arithm_cv_pct",
                     "median",
                     "pct_5",
                     "pct_95",
                     "min",
                     "max",
                     "geom_mean",
                     "geom_cv_pct",
                     "arithm_mean",
                     "arithm_sd",
                     "arithm_cv_pct",
                     "median",
                     "pct_5",
                     "pct_95",
                     "min",
                     "max"
                   ),
                   value = c(
                     51.62,
                     46.62,
                     57.22,
                     29.9,
                     52.25,
                     44.85,
                     `5%` = 31.15,
                     `95%` = 100.75,
                     27.7,
                     109.3,
                     4.16,
                     42.14,
                     4.38,
                     1.49,
                     34.07,
                     4.25,
                     `5%` = 2.62,
                     `95%` = 6.32,
                     2.2,
                     6.8,
                     3.16,
                     56.89,
                     3.75,
                     2.33,
                     62.1,
                     3.9,
                     `5%` = 1.42,
                     `95%` = 6.85,
                     1.4,
                     7.8
                   )
                 ),
                 class = "data.frame",
                 row.names = c(NA, -30L)
               )
  )
})

test_that("Description is excluded if not requested", {
  dat <- data.frame(
    USUBJID = c("A", "B", "C", "D", "E", "F"),
    ACTARM = c(
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X Low Dose",
      "Drug X Low Dose"
    ),
    nca_start = 0,
    nca_end = 24,
    nca_interval = rep("0 - 24", 6),
    CMAX = c(4.3, 4.9, 6.8, 2.2, 4.2, 3.9),
    TMAX = c(7.8, 1.5, 3.8, 1.4, 4, 4),
    AUCALL = c(75.8, 41.4, 106.9, 26.6, 47.9, 38.6),
    LAMZHL = c(12.4, 8.3, 12.2, 8, 26.1, 21.1),
    AUCLST = c(75.1, 41.5, 109.3, 27.7, 47.3, 42.4),
    CLST = c(0.4, 0.1, 0.6, 0.1, 0.2, 0.1),
    LAMZ = c(0.1, 0.1, 0.1, 0.1, 0, 0),
    LAMZNPT = c(5.2, 8.6, 6.7, 9.1, 3.1, 3.1)
  )
  res <- create_nca_table(
    dat, 
    parameters = c("CMAX", "TMAX", "AUCALL", "LAMZHL", "AUCLST", "CLST", "LAMZ", "LAMZNPT"),
    specification = "cdisc",
    description = FALSE,
    format = "long"
  )
  expect_named(res, c("Parameter", "Interval", "Statistic", "value"))
  res_wide <- create_nca_table(
    dat, 
    parameters = c("CMAX", "TMAX", "AUCALL", "LAMZHL", "AUCLST", "CLST", "LAMZ", "LAMZNPT"),
    specification = "cdisc",
    description = FALSE,
    format = "wide"
  )
  expect_named(res_wide, c("Parameter", "Interval", "geom_mean", "geom_cv_pct", "arithm_mean", 
                           "arithm_sd", "arithm_cv_pct", "median", "pct_5", "pct_95", "min", 
                           "max"))
})

test_that("Grouping works", {
  dat <- data.frame(
    USUBJID = c("A", "B", "C", "D", "E", "F"),
    ACTARM = c(
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X Low Dose",
      "Drug X Low Dose"
    ),
    nca_start = 0,
    nca_end = 24,
    nca_interval = rep("0 - 24", 6),
    CMAX = c(4.3, 4.9, 6.8, 2.2, 4.2, 3.9),
    TMAX = c(7.8, 1.5, 3.8, 1.4, 4, 4),
    AUCALL = c(75.8, 41.4, 106.9, 26.6, 47.9, 38.6),
    LAMZHL = c(12.4, 8.3, 12.2, 8, 26.1, 21.1),
    AUCLST = c(75.1, 41.5, 109.3, 27.7, 47.3, 42.4),
    CLST = c(0.4, 0.1, 0.6, 0.1, 0.2, 0.1),
    LAMZ = c(0.1, 0.1, 0.1, 0.1, 0, 0),
    LAMZNPT = c(5.2, 8.6, 6.7, 9.1, 3.1, 3.1)
  )
  res <- create_nca_table(
    dat, 
    parameters = c("CMAX", "TMAX", "AUCALL", "LAMZHL", "AUCLST", "CLST", "LAMZ", "LAMZNPT"),
    description = FALSE, 
    group = "ACTARM",
    format = "long"
  )
  expect_named(
    res,
    c("Parameter", "Interval", "Statistic", "Drug X High Dose", "Drug X Low Dose")
  )
  expect_equal(
    as.numeric(res[res$Parameter == "AUCALL" & res$Statistic == "arithm_mean", "Drug X High Dose", drop = TRUE]),
    91.35
  )
  expect_equal(
    as.numeric(res[res$Parameter == "AUCALL" & res$Statistic == "arithm_mean", "Drug X Low Dose", drop = TRUE]),
    38.62
  )
})

test_that("csv export works", {
  tmpfile <- withr::local_file(file.path(tempdir(), "nca_table.csv"))
  dat <- data.frame(
    USUBJID = c("A", "B", "C", "D", "E", "F"),
    ACTARM = c(
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X High Dose",
      "Drug X Low Dose",
      "Drug X Low Dose",
      "Drug X Low Dose"
    ),
    nca_start = 0,
    nca_end = 24,
    nca_interval = rep("0 - 24", 6),
    CMAX = c(4.3, 4.9, 6.8, 2.2, 4.2, 3.9),
    TMAX = c(7.8, 1.5, 3.8, 1.4, 4, 4),
    AUCMETHD = c(
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log",
      "linear log"
    ),
    AUCALL = c(75.8, 41.4, 106.9, 26.6, 47.9, 38.6),
    LAMZHL = c(12.4, 8.3, 12.2, 8, 26.1, 21.1),
    AUCLST = c(75.1, 41.5, 109.3, 27.7, 47.3, 42.4),
    CLST = c(0.4, 0.1, 0.6, 0.1, 0.2, 0.1),
    LAMZ = c(0.1, 0.1, 0.1, 0.1, 0, 0),
    LAMZNPT = c(5.2, 8.6, 6.7, 9.1, 3.1, 3.1)
  )
  tbl <- create_nca_table(
    dat,
    parameters = c("CMAX", "TMAX", "AUCALL", "LAMZHL", "AUCLST", "CLST", "LAMZ", "LAMZNPT"),
    path = tmpfile,
    format = "long"
  )
  res <- read.csv(tmpfile)
  expect_equal(nrow(res), 80)
  expect_named(res, c("Parameter", "Description", "Interval", "Statistic", "value"))
})

