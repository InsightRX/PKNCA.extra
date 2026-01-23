test_that("check_nca_grouping returns TRUE when groups have sufficient data", {
  # Create test data with sufficient points per group
  data <- data.frame(
    subject_id = rep(1:5, each = 10),
    dose = rep(c(100, 200), each = 25),
    conc = c(rnorm(50, 10, 2)),
    time = rep(0:9, 5)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    conc = "conc"
  )
  
  result <- check_nca_grouping(
    data = data,
    groups = "dose",
    dictionary = dictionary,
    verbose = FALSE
  )
  
  expect_true(result)
})

test_that("check_nca_grouping returns FALSE when many groups have insufficient data", {
  # Create test data where most groups have < 3 points
  data <- data.frame(
    subject_id = rep(1:20, each = 2),
    dose = rep(1:10, each = 4),
    conc = rnorm(40, 10, 2),
    time = rep(c(0, 1), 20)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    conc = "conc"
  )
  
  result <- check_nca_grouping(
    data = data,
    groups = "dose",
    dictionary = dictionary,
    threshold = 0.7,
    verbose = FALSE
  )
  
  expect_false(result)
})

test_that("check_nca_grouping respects custom threshold", {
  # Create data where 50% of groups have insufficient data
  data <- data.frame(
    subject_id = rep(1:10, each = 3),
    dose = rep(1:10, each = 3),
    conc = rnorm(30, 10, 2),
    time = rep(0:2, 10)
  )
  
  # Half have 3 points, half have 2
  data <- data[-c(1, 4, 7, 10, 13), ]
  
  dictionary <- list(
    subject_id = "subject_id",
    conc = "conc"
  )
  
  # Should return FALSE with threshold 0.3 (30%)
  result1 <- check_nca_grouping(
    data = data,
    groups = "dose",
    dictionary = dictionary,
    threshold = 0.3,
    verbose = FALSE
  )
  expect_false(result1)
  
  # Should return TRUE with threshold 0.7 (70%)
  result2 <- check_nca_grouping(
    data = data,
    groups = "dose",
    dictionary = dictionary,
    threshold = 0.7,
    verbose = FALSE
  )
  expect_true(result2)
})

test_that("check_nca_grouping respects custom minPointsRegression setting", {
  data <- data.frame(
    subject_id = rep(1:5, each = 4),
    dose = rep(1:2, each = 10),
    conc = rnorm(20, 10, 2),
    time = rep(0:3, 5)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    conc = "conc"
  )
  
  # With minPointsRegression = 3, should pass
  result1 <- check_nca_grouping(
    data = data,
    groups = "dose",
    dictionary = dictionary,
    settings = list(minPointsRegression = 3),
    verbose = FALSE
  )
  expect_true(result1)
  
  # With minPointsRegression = 5, should fail
  result2 <- check_nca_grouping(
    data = data,
    groups = "dose",
    dictionary = dictionary,
    settings = list(minPointsRegression = 5),
    verbose = FALSE
  )
  expect_false(result2)
})

test_that("check_nca_grouping filters out NA and zero concentrations", {
  data <- data.frame(
    subject_id = rep(1:5, each = 6),
    dose = rep(1, 30),
    conc = c(10, 8, NA, 6, 0, 4, 
             12, NA, 9, 7, 0, 5,
             11, 9, 8, NA, 6, 0,
             13, 10, 9, 0, 7, NA,
             14, 11, 0, 9, NA, 6),
    time = rep(0:5, 5)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    conc = rlang::sym("conc")
  )
  
  # After filtering NA and zeros, each subject should have 3 points
  result <- check_nca_grouping(
    data = data,
    groups = "dose",
    dictionary = dictionary,
    settings = list(minPointsRegression = 3),
    verbose = FALSE
  )
  
  expect_true(result)
})

test_that("check_nca_grouping errors when dictionary lacks subject_id", {
  data <- data.frame(
    id = 1:10,
    conc = rnorm(10)
  )
  
  dictionary <- list(conc = "conc")
  
  expect_error(
    check_nca_grouping(
      data = data,
      groups = "id",
      dictionary = dictionary,
      verbose = FALSE
    ),
    "Need a `dictionary` with `subject_id` entry"
  )
})

test_that("check_nca_grouping works with multiple grouping variables", {
  data <- data.frame(
    subject_id = rep(1:10, each = 5),
    dose = rep(c(100, 200), each = 25),
    formulation = rep(c("A", "B"), 25),
    conc = rnorm(50, 10, 2),
    time = rep(0:4, 10)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    conc = "conc"
  )
  
  result <- check_nca_grouping(
    data = data,
    groups = c("dose", "formulation"),
    dictionary = dictionary,
    verbose = FALSE
  )
  
  expect_true(result)
})

test_that("check_nca_grouping works when conc is NULL in dictionary", {
  data <- data.frame(
    subject_id = rep(1:5, each = 4),
    dose = rep(1:2, each = 10),
    time = rep(0:3, 5)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    conc = NULL
  )
  
  # Should not filter by concentration
  result <- check_nca_grouping(
    data = data,
    groups = "dose",
    dictionary = dictionary,
    verbose = FALSE
  )
  
  expect_true(result)
})

test_that("check_nca_grouping produces warning message when verbose = TRUE", {
  data <- data.frame(
    subject_id = rep(1:10, each = 2),
    dose = rep(1:10, each = 2),
    conc = rnorm(20, 10, 2),
    time = rep(c(0, 1), 10)
  )
  
  dictionary <- list(
    subject_id = "subject_id",
    conc = rlang::sym("conc")
  )
  
  expect_warning(
    check_nca_grouping(
      data = data,
      groups = "dose",
      dictionary = dictionary,
      verbose = TRUE
    ),
    "Many groups.*have too few data points"
  )
})