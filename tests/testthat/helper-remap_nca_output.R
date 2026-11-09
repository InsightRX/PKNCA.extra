## Sample data for testing remap_nca_output()
make_remap_data <- function() {
  data <- data.frame(
    old_param1 = 1:5,
    old_param2 = 6:10,
    another_column = 11:15
  )
  attr(data, "parameters") <- c("old_param1", "old_param2")
  data
}

make_remap_mapping <- function() {
  list(
    new_param1 = "old_param1",
    new_param2 = "old_param2"
  )
}
