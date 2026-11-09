test_that("read_spec_from_file reads local file if it exists", {
  test_file <- local_test_spec_file()
  result <- read_spec_from_file(test_file)
  expect_equal(result, test_spec_content)
})

test_that("read_spec_from_file works for JSON included with package", {
  result <- read_spec_from_file("default")
  expect_equal(names(result$nca), c("pknca.parameters", "pknca.parameter_mapping"))
})

test_that("read_spec_from_file throws an error if file does not exist", {
  expect_error(read_spec_from_file("nonexistent_spec"), "Specification 'nonexistent_spec' not found.")
})

test_that("read_spec_from_file returns correct content", {
  test_file <- local_test_spec_file()
  result <- read_spec_from_file(test_file)
  expect_equal(result$name, "test")
  expect_equal(result$value, 123)
})

