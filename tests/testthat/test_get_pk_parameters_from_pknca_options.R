test_that("NULL options and explicit PKNCA single.dose.aucs options give identical results", {
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  PKNCA::PKNCA.options(default = TRUE)
  for(no_dots in c(TRUE, FALSE)) {
    expect_identical(
      get_pk_parameters_from_pknca_options(NULL, no_dots = no_dots),
      get_pk_parameters_from_pknca_options(
        PKNCA::PKNCA.options()$single.dose.aucs,
        no_dots = no_dots
      )
    )
  }
})

test_that("explicit PKNCA single.dose.aucs options with no_dots TRUE gives underscored names", {
  res <- get_pk_parameters_from_pknca_options(default_aucs(), no_dots = TRUE)
  expect_equal(res, c("auclast", "cmax", "tmax", "half_life", "aucinf_obs"))
})

test_that("no_dots defaults to FALSE and keeps PKNCA parameter names", {
  res <- get_pk_parameters_from_pknca_options(default_aucs())
  expect_equal(res, c("auclast", "cmax", "tmax", "half.life", "aucinf.obs"))
})

test_that("result is not affected by a preceding run_nca() call", {
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  PKNCA::PKNCA.options(default = TRUE)
  before_null <- get_pk_parameters_from_pknca_options(NULL, no_dots = TRUE)

  dat <- nca_admiral
  ids <- unique(dat$USUBJID)
  dat <- dat[dat$USUBJID %in% ids[1:3], ]
  suppressMessages(run_nca(dat, verbose = FALSE))

  expect_identical(
    get_pk_parameters_from_pknca_options(NULL, no_dots = TRUE),
    before_null
  )
  expect_identical(
    get_pk_parameters_from_pknca_options(
      PKNCA::PKNCA.options()$single.dose.aucs,
      no_dots = TRUE
    ),
    before_null
  )
})

test_that("only logical columns are treated as parameters", {
  ## start/end equal to 1 must not be picked up as parameters
  opts <- data.frame(
    start = 1, end = 1, impute = "start_conc0",
    auclast = TRUE, cmax = FALSE
  )
  expect_equal(get_pk_parameters_from_pknca_options(opts), "auclast")
})

test_that("parameters requested in multiple intervals are returned once, in order of first appearance", {
  opts <- data.frame(
    start = c(0, 0), end = c(24, Inf),
    cmax = c(FALSE, TRUE), auclast = c(TRUE, TRUE), aucinf.obs = c(FALSE, TRUE)
  )
  expect_equal(
    get_pk_parameters_from_pknca_options(opts, no_dots = TRUE),
    c("auclast", "cmax", "aucinf_obs")
  )
})

test_that("NA values are treated as not requested", {
  opts <- data.frame(start = 0, end = Inf, auclast = TRUE, cmax = NA)
  expect_equal(get_pk_parameters_from_pknca_options(opts), "auclast")
})

test_that("no requested parameters or no intervals gives empty character vector", {
  opts <- data.frame(start = 0, end = Inf, auclast = FALSE)
  expect_identical(get_pk_parameters_from_pknca_options(opts), character(0))
  expect_identical(get_pk_parameters_from_pknca_options(opts[0, ]), character(0))
})

test_that("invalid arguments throw errors", {
  expect_error(get_pk_parameters_from_pknca_options(list(auclast = TRUE)), "data.frame")
  expect_error(get_pk_parameters_from_pknca_options(default_aucs(), no_dots = NA), "no_dots")
  expect_error(get_pk_parameters_from_pknca_options(default_aucs(), no_dots = "yes"), "no_dots")
})
