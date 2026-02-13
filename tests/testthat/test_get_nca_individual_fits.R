test_that("wrong object throws error", {
  expect_error(get_nca_individual_fits(list(foo = "bar")))
})

test_that("calculations are correct, for NCA with 2 groupings (subject, treatment)", {
  res <- readRDS(system.file("testdata/pknca/nca_testresult.rds", package = "PKNCA.extra"))
  fit <- get_nca_individual_fits(res, dictionary = list(foo = "SDEID", bar = "SITEID"))
  expect_equal(names(fit), c("conc", "time", "treatment_group", "subject_id", "TREATXT", "SDEID", "SITEID", "n_points", "r_squared", "adj_r_squared", "blq", "used_in_fit", "excluded", "exclude_reason", "prediction"))
  expect_equal(nrow(fit), 3240)
  expect_equal(fit$conc[1:5], c(0.626, 27.7, 43.9, 48.8, 38.7))
  expect_equal(fit$prediction[1:5], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
  expect_equal(round(tail(fit$prediction, 5), 3), c(1.837, 1.685, 1.546, NA, 1.418))
  expect_equal(fit$SITEID[1:5], rep(1234, 5))
  
  ## check correct number of points used in fit marked in data
  expect_equal(
    fit |> 
      dplyr::filter(subject_id == 12341001, is.na(prediction), TREATXT == "D 2mg + g") |>
      dplyr::pull(used_in_fit) |>
    sum(),
    3
  )
})
  