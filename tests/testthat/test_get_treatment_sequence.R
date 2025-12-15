test_that("get_treatment_sequence works with basic crossover design", {
  # Create test data
  test_data <- data.frame(
    subject_id = rep(c("S1", "S2", "S3"), each = 2),
    time = rep(c(0, 24), 3),
    treatment = c("A", "B", "B", "A", "A", "B")
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time"
  )
  
  result <- get_treatment_sequence(test_data, dictionary)
  
  # Check structure
  expect_true(all(c("subject_id", "sequence", "treatment_sequence") %in% names(result)))
  expect_equal(nrow(result), 3)  # One row per subject
  
  # Check sequence assignments
  expect_equal(
    result$treatment_sequence[result$subject_id == "S1"],
    "A -> B"
  )
  expect_equal(
    result$treatment_sequence[result$subject_id == "S2"],
    "B -> A"
  )
  expect_equal(
    result$treatment_sequence[result$subject_id == "S3"],
    "A -> B"
  )
  
  # Check that identical sequences get the same sequence number
  expect_equal(
    result$sequence[result$subject_id == "S1"],
    result$sequence[result$subject_id == "S3"]
  )
})

test_that("get_treatment_sequence works with groups parameter", {
  # Create test data with groups
  test_data <- data.frame(
    subject_id = rep(c("S1", "S2", "S3"), each = 2),
    time = rep(c(0, 24), 3),
    treatment = c("A", "B", "B", "A", "A", "B"),
    group = rep(c("G1", "G2", "G1"), each = 2)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time"
  )
  
  result <- get_treatment_sequence(test_data, dictionary, groups = "group")
  
  # Check that groups are preserved
  expect_true(all(c("subject_id", "sequence", "treatment_sequence") %in% names(result)))
  expect_equal(nrow(result), 3)
})

test_that("get_treatment_sequence handles empty data", {
  # Create empty test data
  test_data <- data.frame(
    subject_id = character(),
    time = numeric(),
    treatment = character()
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time"
  )
  
  result <- get_treatment_sequence(test_data, dictionary)
  
  # Check that result is empty but has correct structure
  expect_equal(nrow(result), 0)
  expect_true(all(c("subject_id", "sequence", "treatment_sequence") %in% names(result)))
})

test_that("get_treatment_sequence works with custom variable name", {
  # Create test data with custom variable name
  test_data <- data.frame(
    subject_id = rep(c("S1", "S2"), each = 2),
    time = rep(c(0, 24), 2),
    drug = c("X", "Y", "Y", "X")
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    time = "time"
  )
  
  result <- get_treatment_sequence(test_data, dictionary, variable = "drug")
  
  # Check that custom variable is used
  expect_equal(
    result$treatment_sequence[result$subject_id == "S1"],
    "X -> Y"
  )
  expect_equal(
    result$treatment_sequence[result$subject_id == "S2"],
    "Y -> X"
  )
}) 

