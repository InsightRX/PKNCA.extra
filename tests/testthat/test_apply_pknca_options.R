test_that("apply_pknca_options is able to apply options correctly from options object", {
  md <- jsonlite::read_json( system.file(package = "PKNCA.extra", file.path("nca", paste0("pknca", ".json"))) )
  options <- md$defaults
  PKNCA::PKNCA.options(default = TRUE) # make sure to reset to defaults, and check that default are applied
  tmp1 <- PKNCA::PKNCA.options()
  expect_equal(tmp1$conc.blq, list(first = "keep", middle = "drop", last = "keep"))
  expect_message(apply_pknca_options(options, verbose = T))
  tmp2 <- PKNCA::PKNCA.options()
  expect_equal(tmp2$conc.blq, list(first = "keep", middle = "keep", last = "keep"))
})
