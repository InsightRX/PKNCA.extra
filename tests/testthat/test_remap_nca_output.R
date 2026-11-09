test_that("remap_nca_output works with valid inputs", {
  data <- make_remap_data()
  mapping <- make_remap_mapping()
  result <- remap_nca_output(data, mapping, exclude_unmapped = TRUE)
  expect_true("new_param1" %in% colnames(result))
  expect_true("new_param2" %in% colnames(result))
  expect_false("another_column" %in% colnames(result))
})

test_that("remap_nca_output works with exclude_unmapped = FALSE", {
  data <- make_remap_data()
  mapping <- make_remap_mapping()
  result <- remap_nca_output(data, mapping, exclude_unmapped = FALSE)
  expect_true("new_param1" %in% colnames(result))
  expect_true("new_param2" %in% colnames(result))
  expect_true("another_column" %in% colnames(result))
})

test_that("remap_nca_output keeps specified columns", {
  data <- make_remap_data()
  mapping <- make_remap_mapping()
  result <- remap_nca_output(data, mapping, exclude_unmapped = TRUE, keep = "another_column")
  expect_true("new_param1" %in% colnames(result))
  expect_true("new_param2" %in% colnames(result))
  expect_true("another_column" %in% colnames(result))
})

test_that("remap_nca_output stops when mapping is not a list", {
  data <- make_remap_data()
  expect_error(
    remap_nca_output(data, mapping = "not_a_list"), 
    "Provided NCA parameter mapping not in expected format."
  )
})

test_that("remap_nca_output stops when data is not a data.frame", {
  mapping <- make_remap_mapping()
  expect_error(
    remap_nca_output(mapping = mapping, data = "not_a_dataframe"), 
    "Please provide a data.frame or tibble as `data` argument."
  )
})

