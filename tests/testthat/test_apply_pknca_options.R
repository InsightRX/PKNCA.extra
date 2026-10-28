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

test_that("restore_pknca_options does not replay validation warnings", {
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  ## valid but discouraged value, PKNCA warns when it is set
  expect_warning(PKNCA::PKNCA.options(min.span.ratio = 1), "min.span.ratio")
  saved <- PKNCA::PKNCA.options()
  PKNCA::PKNCA.options(default = TRUE)
  expect_no_warning(restore_pknca_options(saved))
  expect_identical(PKNCA::PKNCA.options(), saved)
})

test_that("restore_pknca_options restores all options when warnings are errors", {
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  suppressWarnings(PKNCA::PKNCA.options(min.span.ratio = 1, min.hl.points = 4))
  saved <- PKNCA::PKNCA.options()
  PKNCA::PKNCA.options(default = TRUE)
  withr::with_options(list(warn = 2), restore_pknca_options(saved))
  expect_identical(PKNCA::PKNCA.options(), saved)
})

test_that("restore_pknca_options restores options that are NULL by default", {
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  PKNCA::PKNCA.options(default = TRUE)
  saved <- PKNCA::PKNCA.options()
  expect_null(saved$keep_interval_cols)
  PKNCA::PKNCA.options(keep_interval_cols = "x")
  restore_pknca_options(saved)
  expect_identical(PKNCA::PKNCA.options(), saved)
})

test_that("run_nca() does not emit warnings when restoring discouraged options", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)
  old <- PKNCA::PKNCA.options()
  withr::defer(restore_pknca_options(old))
  suppressWarnings(PKNCA::PKNCA.options(min.span.ratio = 1))
  saved <- PKNCA::PKNCA.options()
  ## capture warnings raised after the analysis, while restoring options
  restore_warnings <- character(0)
  withCallingHandlers(
    suppressMessages(run_nca(dat[dat$USUBJID %in% ids[1:3], ], verbose = FALSE)),
    warning = function(w) {
      if (grepl("min.span.ratio", conditionMessage(w))) {
        restore_warnings <<- c(restore_warnings, conditionMessage(w))
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_length(restore_warnings, 0)
  expect_identical(PKNCA::PKNCA.options(), saved)
})
