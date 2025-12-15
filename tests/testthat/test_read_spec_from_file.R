# Mock data for testing
test_spec_content <- list(name = "test", value = 123)
test_spec_json <- jsonlite::toJSON(test_spec_content, auto_unbox = TRUE)

# Create a temporary directory for test files
temp_dir <- tempdir()
test_file <- file.path(temp_dir, "test_spec.json")
write(test_spec_json, test_file)

# Write mock package spec file
pkg_spec_file <- file.path(temp_dir, "md", "pkg_spec.json")
dir.create(dirname(pkg_spec_file), recursive = TRUE)
write(test_spec_json, pkg_spec_file)

# Clean up mock files
on.exit({
  unlink(test_file)
  unlink(pkg_spec_file, recursive = TRUE)
})

test_that("read_spec_from_file reads local file if it exists", {
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
  result <- read_spec_from_file(test_file)
  expect_equal(result$name, "test")
  expect_equal(result$value, 123)
})

