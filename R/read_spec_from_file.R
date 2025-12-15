#' Read specification from metadata file
#' 
#' @param spec_file JSON file with specifications for NCA. See example files
#' for more info.
read_spec_from_file <- function(spec_file) {
  if(is.null(spec_file)) {
    warning("NCA specification file cannot be NULL, setting to `default`")
    spec_file <- "default"
  }
  if(!file.exists(spec_file)) {
    pkg_spec_file <- system.file(package = "PKNCA.extra", file.path("md", paste0(spec_file, ".json")))
    if(!file.exists(pkg_spec_file)) {
      stop("Specification '", spec_file, "' not found.")
    }
    spec_file <- pkg_spec_file
  }
  jsonlite::read_json(spec_file)
}