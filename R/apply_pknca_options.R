#' Read settings from a JSON file, and pass to PKNCA.options()
#' 
#' @param options a list of PKNCA options to be set
#' @param nca_parameters list of what parameters to calculate, e.g. 
#' `list(auclast=TRUE, cmax=FALSE, ...)`
#' @param verbose show verbose output?
#' 
#' @returns The current PKNCA options.
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

#' Restore a previously saved set of PKNCA options
#'
#' `PKNCA::PKNCA.options()` is global, session-wide state. Functions that
#' change it temporarily (such as `run_nca()`) should save the options on entry
#' with `PKNCA::PKNCA.options()` and call this function on exit, so that the
#' options seen by any later code are not affected by the analysis.
#'
#' The options are first reset to the PKNCA defaults, after which all options
#' that differ from the defaults are set in a single call to
#' `PKNCA::PKNCA.options()`. PKNCA validates all values before storing any of
#' them, so the restore is atomic: either all options are restored, or (if any
#' value is rejected) the options are left at the PKNCA defaults. Options
#' whose saved value is `NULL` are restored through the reset to defaults,
#' since `PKNCA::PKNCA.options()` cannot set an option to `NULL`.
#'
#' PKNCA re-validates the saved values when they are set. Any warnings from
#' that validation (e.g. `min.span.ratio` below 2) were already shown when the
#' options were originally set, and are therefore suppressed. A warning is
#' given if the restored options are not identical to the saved options.
#'
#' @param options list of PKNCA options, as returned by
#' `PKNCA::PKNCA.options()`.
#'
#' @returns The restored PKNCA options (invisibly).
restore_pknca_options <- function(options) {
  defaults <- PKNCA::PKNCA.options(default = TRUE)
  changed <- names(options)[
    !vapply(
      names(options),
      function(key) identical(options[[key]], defaults[[key]]),
      logical(1)
    )
  ]
  changed <- changed[!vapply(options[changed], is.null, logical(1))]
  if(length(changed) > 0) {
    suppressWarnings(
      do.call(PKNCA::PKNCA.options, options[changed])
    )
  }
  restored <- PKNCA::PKNCA.options()
  if(!identical(restored, options)) {
    cli::cli_warn("PKNCA options could not be fully restored.")
  }
  invisible(restored)
}

#' Get the PKNCA options from a spec file, and parse into 
#' expected format for `PKNCA::PKNCA.options()`
#' 
#' @param opts list of options for PKNCA
#' 
#' @returns
#' A formatted list of arguments to be passed to `PKNCA::PKNCA.options()`.
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
