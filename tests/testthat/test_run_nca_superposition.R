test_that("run_nca_superposition works with basic superposition", {
  # Create test data with concentration and dose information
  test_data <- data.frame(
    subject_id = rep(c("S1", "S2"), each = 6),
    time = rep(c(0, 0, 1, 2, 3, 4), 2),
    conc = c(0, 0, 10, 8, 6, 5, 0, 0, 12, 9, 7, 6),
    dose_level = rep(100, 12),
    amount = rep(c(100, 0, 0, 0, 0, 0), 2),
    evid = rep(c(1, 0, 0, 0, 0, 0), 2)  # 1 for dose, 0 for concentration
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time",
    conc = "conc",
    dose = "amount",
    dose_level = "dose_level"
  )
  
  regimen <- list(interval = 12)
  
  result <- run_nca_superposition(
    data = test_data,
    regimen = regimen,
    dictionary = dictionary,
    parameters = c("auclast", "cmax"),
    verbose = FALSE
  )
  
  expect_true(inherits(result, "data.frame"))
  expect_true(all(c("auclast", "cmax") %in% names(result)))
  expect_equal(round(result$aucall, 2), c(47.27, 59.82))
})

test_that("run_nca_superposition works with different dose levels", {
  # Create test data with multiple dose levels
  test_data <- data.frame(
    subject_id = rep(c("S1", "S2"), each = 6),
    time = rep(c(0, 0, 1, 2, 3, 4), 2),
    conc = c(0, 0, 10, 8, 6, 5, 0, 0, 12, 9, 7, 6),
    dose_level = rep(100, 12),
    amount = rep(c(100, 0, 0, 0, 0, 0), 2),
    evid = rep(c(1, 0, 0, 0, 0, 0), 2)  # 1 for dose, 0 for concentration
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time",
    conc = "conc",
    dose = "amount",
    dose_level = "dose_level"
  )
  
  regimen <- list(interval = 4)
  
  result <- run_nca_superposition(
    data = test_data,
    regimen = regimen,
    dictionary = dictionary,
    parameters = c("auclast", "cmax"),
    verbose = FALSE
  )
  
  expect_true(inherits(result, "data.frame"))
  expect_true(all(c("auclast", "cmax") %in% names(result)))
  expect_equal(round(result$aucall, 2), c(43.6, 55.09))
})

test_that("run_nca_superposition works with custom dose in regimen", {
  # Create test data
  test_data <- data.frame(
    subject_id = rep(c("S1", "S2"), each = 6),
    time = rep(c(0, 0, 1, 2, 3, 4), 2),
    conc = c(0, 0, 10, 8, 6, 5, 0, 0, 12, 9, 7, 6),
    dose_level = rep(100, 12),
    amount = rep(c(100, 0, 0, 0, 0, 0), 2),
    evid = rep(c(1, 0, 0, 0, 0, 0), 2)  # 1 for dose, 0 for concentration
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time",
    conc = "conc",
    dose = "amount",
    dose_level = "dose_level"
  )
  
  # Specify a different dose in the regimen
  regimen <- list(interval = 12, dose = 200)
  
  result <- run_nca_superposition(
    data = test_data,
    regimen = regimen,
    dictionary = dictionary,
    parameters = c("auclast", "cmax"),
    verbose = FALSE
  )
  
  expect_true(inherits(result, "data.frame"))
  expect_true(all(c("auclast", "cmax") %in% names(result)))
  expect_equal(round(result$aucall, 2), c(94.55, 119.65))
  
})

test_that("run_nca_superposition works with groups", {
  # Create test data with groups
  test_data <- data.frame(
    subject_id = rep(c("S1", "S2"), each = 6),
    time = rep(c(0, 0, 1, 2, 3, 4), 2),
    conc = c(0, 0, 10, 8, 6, 5, 0, 0, 12, 9, 7, 6),
    dose_level = rep(100, 12),
    amount = rep(c(100, 0, 0, 0, 0, 0), 2),
    evid = rep(c(1, 0, 0, 0, 0, 0), 2),  # 1 for dose, 0 for concentration
    group = rep(c("A", "B"), each = 6)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time",
    conc = "conc",
    dose = "amount",
    dose_level = "dose_level"
  )
  
  regimen <- list(interval = 4)
  
  result <- run_nca_superposition(
    data = test_data,
    regimen = regimen,
    dictionary = dictionary,
    parameters = c("auclast", "cmax"),
    groups = "group",
    verbose = FALSE
  )
  
  expect_true(inherits(result, "data.frame"))
  expect_true(all(c("auclast", "cmax") %in% names(result)))
  expect_equal(round(result$aucall, 2), c(43.6, 55.09))
  expect_equal(result$group, c("A", "B"))
})

test_that("run_nca_superposition handles missing dose_level gracefully", {
  # Create test data without dose_level
  test_data <- data.frame(
    subject_id = rep(c("S1", "S2"), each = 4),
    time = rep(c(0, 1, 2, 3), 2),
    conc = c(0, 10, 8, 6, 0, 12, 9, 7),
    evid = rep(c(1, 0, 0, 0), 2)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time",
    conc = "conc"
  )
  
  regimen <- list(interval = 4)
  
  # Expect an error when dose_level is missing
  expect_error(
    run_nca_superposition(
      data = test_data,
      regimen = regimen,
      dictionary = dictionary,
      parameters = c("auclast", "cmax"),
      verbose = FALSE
    ),
    "A dictionary entry `dose_level` and corresponding column in `data` is required"
  )
})

test_that("run_nca_superposition handles missing EVID column gracefully", {
  # Create test data without EVID column
  test_data <- data.frame(
    subject_id = rep(c("S1", "S2"), each = 4),
    time = rep(c(0, 1, 2, 3), 2),
    conc = c(0, 10, 8, 6, 0, 12, 9, 7),
    dose_level = rep(100, 8)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time",
    conc = "conc",
    dose_level = "dose_level"
  )
  
  regimen <- list(interval = 4)
  
  # Expect an error when EVID column is missing
  expect_error(
    run_nca_superposition(
      data = test_data,
      regimen = regimen,
      dictionary = dictionary,
      parameters = c("auclast", "cmax"),
      verbose = FALSE
    ),
    "Data requires an EVID column"
  )
}) 
