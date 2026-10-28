test_that("parse_data_for_nca merges SDTM tables into NCA dataset", {
  res <- parse_data_for_nca(make_sdtm())
  expect_s3_class(res, "data.frame")
  ## Placebo arm is removed
  expect_equal(sort(unique(res$USUBJID)), c("STUDY-001", "STUDY-002"))
  expect_equal(nrow(res), 6)
  expect_true(all(c(
    "ANALYTE", "MATRIX", "PROFILE", "GROUP", "GROUPN", "ATIME", "NTIME",
    "ACONC", "CONCUNIT", "LLOQ", "DOSE", "DOSEUNIT", "ADM", "ATSLD", "SAMPLEID"
  ) %in% names(res)))
  expect_equal(res$ANALYTE, rep("DRUGX", 6))
  expect_equal(res$NTIME, res$PCTPTNUM)
  expect_equal(res$ACONC, res$PCSTRESC)
  expect_equal(res$DOSE[res$USUBJID == "STUDY-002"], rep(20, 3))
  expect_equal(unique(res$ADM), "EXTRAVASCULAR")
  expect_equal(unique(res$ADUR), 0)
  expect_equal(res$SAMPLEID[1], "STUDY001-20240101T0000")
})

test_that("parse_data_for_nca sets IV administration", {
  res <- parse_data_for_nca(make_sdtm(route = "INTRAVENOUS"))
  expect_equal(unique(res$ADM), "INTRAVENOUS")
  expect_true(all(is.na(res$ADUR)))
})

test_that("parse_data_for_nca output is reproducible", {
  sdtm <- make_sdtm()
  res1 <- parse_data_for_nca(sdtm)
  res2 <- parse_data_for_nca(sdtm)
  expect_identical(res1, res2)
  ## ATSLD is simulated around nominal time, so 0 at time 0
  expect_equal(res1$ATSLD[res1$PCTPTNUM == 0], rep(0, 2))
  expect_false(identical(res1$ATSLD, res1$PCTPTNUM))
  ## Fixed reference values, guards against changes in the simulation
  expect_equal(res1$ATSLD, c(0, 1.05, 3.97, 0, 1.04, 3.52))
  ## Different seed gives different ATSLD
  res3 <- parse_data_for_nca(sdtm, seed = 1)
  expect_false(identical(res1$ATSLD, res3$ATSLD))
})

test_that("parse_data_for_nca does not change the global random number stream", {
  set.seed(1)
  expected <- stats::runif(1)
  set.seed(1)
  parse_data_for_nca(make_sdtm())
  expect_identical(stats::runif(1), expected)
})

test_that("parse_data_for_nca with seed = NULL uses session random number state", {
  sdtm <- make_sdtm()
  set.seed(10)
  res1 <- parse_data_for_nca(sdtm, seed = NULL)
  set.seed(10)
  res2 <- parse_data_for_nca(sdtm, seed = NULL)
  expect_identical(res1, res2)
})

test_that("parse_data_for_nca validates seed", {
  expect_error(parse_data_for_nca(make_sdtm(), seed = "a"), "seed")
  expect_error(parse_data_for_nca(make_sdtm(), seed = c(1, 2)), "seed")
  expect_error(parse_data_for_nca(make_sdtm(), seed = NA), "seed")
})

test_that("parse_data_for_nca does not change the random number generator kind", {
  old_kind <- RNGkind()
  withr::defer(do.call(RNGkind, as.list(old_kind)))
  RNGkind("L'Ecuyer-CMRG")
  res <- parse_data_for_nca(make_sdtm())
  expect_equal(RNGkind()[1], "L'Ecuyer-CMRG")
  ## result does not depend on the session's generator kind
  expect_equal(res$ATSLD, c(0, 1.05, 3.97, 0, 1.04, 3.52))
})
