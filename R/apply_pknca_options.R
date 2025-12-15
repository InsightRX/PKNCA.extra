#' Read settings from a JSON file, and pass to PKNCA.options()
#' 
#' @param options a list of PKNCA options to be set
#' @param nca_parameters list of what parameters to calculate, e.g. 
#' `list(auclast=TRUE, cmax=FALSE, ...)`
#' @param verbose show verbose output?
#' 
apply_pknca_options <- function(
  options, 
  nca_parameters = NULL, 
  verbose = TRUE
) {
  arg_list <- parse_pknca_opts_from_spec(options)
  if(length(arg_list) > 0) {
    if(verbose) message("Applying custom settings to PKNCA:\n  ", paste0(names(arg_list), collapse = "\n  "))
    do.call(PKNCA::PKNCA.options, args = arg_list)
  } else {
    if(verbose) message("No PKNCA options found in specification, using defaults from package.")
  }
  if(!is.null(nca_parameters)) {
    if(is.null(nca_parameters$start)) nca_parameters$start <- 0
    if(is.null(nca_parameters$end)) nca_parameters$end <- Inf
    PKNCA::PKNCA.options(single.dose.aucs = as.data.frame(nca_parameters))
  }
  return(PKNCA::PKNCA.options())
}

#' Get the PKNCA options from a spec file, and parse into 
#' expected format for `PKNCA::PKNCA.options()`
#' 
#' @param opts list of options for PKNCA
parse_pknca_opts_from_spec <- function(opts) {
  arg_list <- list()
  for(key in names(opts)) {
    if (is_set_to_missing(opts[[key]])) { ## JSON has no support for NA, so we're using "<NA>" to represent NA
      arg_list[[key]] <- NA
    } else if (is_set_to_inf(opts[[key]])) { ## JSON has no support for Inf, so we're using "<Inf>" to represent Inf
      arg_list[[key]] <- Inf
    } else {
      arg_list[[key]] <- opts[[key]]
    }
  }
  arg_list
}

#' Is value meant to be `NA`?
#' 
#' @param x value 
is_set_to_missing <- function(x) {
  inherits(x, "character") && x == "<NA>"
}

#' Is value meant to be `Inf`?
#' 
#' @param x value 
is_set_to_inf <- function(x) {
  inherits(x, "character") && x == "<Inf>"
}
