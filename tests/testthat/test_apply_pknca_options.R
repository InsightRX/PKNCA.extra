test_that("apply_pknca_options is able to apply options correctly from options object", {
  md <- jsonlite::read_json( system.file(package = "PKNCA.extra", file.path("nca", paste0("pknca", ".json"))) )
  options <- md$defaults
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  PKNCA::PKNCA.options(default = TRUE) # make sure to reset to defaults, and check that default are applied
  tmp1 <- PKNCA::PKNCA.options()
  expect_equal(tmp1$conc.blq, list(first = "keep", middle = "drop", last = "keep"))
  expect_message(apply_pknca_options(options, verbose = T))
  tmp2 <- PKNCA::PKNCA.options()
  expect_equal(tmp2$conc.blq, list(first = "keep", middle = "keep", last = "keep"))
})

test_that("restore_pknca_options restores all previously saved options", {
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  PKNCA::PKNCA.options(
    single.dose.aucs = data.frame(start = 0, end = 24, auclast = TRUE),
    min.hl.points = 4
  )
  expect_false(identical(PKNCA::PKNCA.options(), old))
  restore_pknca_options(old)
  expect_identical(PKNCA::PKNCA.options(), old)
})

test_that("run_nca() does not change the global PKNCA options", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  suppressMessages(
    run_nca(
      dat[dat$USUBJID %in% ids[1:3], ],
      settings = list(min.hl.points = 4),
      verbose = FALSE
    )
  )
  expect_identical(PKNCA::PKNCA.options(), old)
})
