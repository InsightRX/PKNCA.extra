test_that("set_pknca_interval_parameters sets single.dose.aucs option", {
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  set_pknca_interval_parameters(
    time = c(0, 24),
    parameters = list(auclast = TRUE, cmax = TRUE, half.life = FALSE)
  )
  aucs <- PKNCA::PKNCA.options()$single.dose.aucs
  expect_equal(nrow(aucs), 1)
  expect_equal(aucs$start, 0)
  expect_equal(aucs$end, 24)
  expect_equal(
    get_pk_parameters_from_pknca_options(NULL),
    c("auclast", "cmax")
  )
})

test_that("set_pknca_interval_parameters requires start and end time", {
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  expect_error(
    set_pknca_interval_parameters(time = 24, parameters = list(auclast = TRUE)),
    "length 2"
  )
  expect_identical(PKNCA::PKNCA.options(), old)
})
