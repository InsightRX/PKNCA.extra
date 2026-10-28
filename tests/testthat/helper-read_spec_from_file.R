## Mock specification content for testing read_spec_from_file()
test_spec_content <- list(name = "test", value = 123)

## Write the mock specification to a temporary JSON file, which is removed
## when the calling test finishes
local_test_spec_file <- function(env = parent.frame()) {
  test_file <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  write(jsonlite::toJSON(test_spec_content, auto_unbox = TRUE), test_file)
  test_file
}
