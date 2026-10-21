#' Add AUCtau to the NCA output, if multi-dose PK data
#' 
#' Will copy `auclast` (or the column specified in `auc_col`) to `auctau` in
#' the NCA output, for those intervals where the end of the interval is <Inf.
#' `run_nca()` uses `aucint.last.dose` instead of `auclast` for intervals
#' without a concentration measured exactly at the end of the interval.
#' 
#' It will first check if the NCA was indeed multi-dose,
#' as this is not relevant variable for single-dose data.
#'
#' @param output PKNCA output object
#' @param dictionary data dictionary
#' @param verbose verbose output?
#' @param auc_col name of the column with the AUC to use for `auctau`.
#' 
#' @returns 
#' NCA output data with `auctau` added.
#' 
#' @export
nca_add_auctau <- function(
  output,
  dictionary,
  verbose = FALSE,
  auc_col = "auclast"
) {
  
  if(auc_col %in% names(output)) {
    ## Check that there are intervals where we need to add it.
    ## We don't want to add auctau for single dose data
    n_rows_to_update <- output |>
      dplyr::filter(!is.na(.data[[auc_col]]) & .data$nca_end < Inf) |>
      nrow()
    if(n_rows_to_update == 0) {
      if(verbose)
        cli::cli_alert_info("Not adding `auctau`, no multi-dose intervals detected in NCA output")
    } else{
      if(verbose)
        cli::cli_alert_info("Adding auctau for relevant subjects / intervals")
      output <- output |>
        dplyr::mutate(
          auctau = dplyr::if_else(.data$nca_end < Inf, .data[[auc_col]], NA)
        )
    }
  } else {
    cli::cli_alert_warning(paste0("Column `", auc_col, "` not found in NCA data, not adding `auctau` variable."))
  }

  output
  
}

#' Check for each interval whether a concentration was measured at its end
#'
#' Only non-missing, non-excluded concentrations are considered.
#'
#' @param output data.frame with `end` and group columns
#' @param conc_obj `PKNCAconc` object
#'
#' @returns logical vector, one value per row in `output`
#'
#' @keywords internal
is_sampled_at_end <- function(output, conc_obj) {
  dat <- as.data.frame(conc_obj$data)
  usable <- !is.na(dat[[conc_obj$columns$concentration]])
  exclude_col <- conc_obj$columns$exclude
  if(!is.null(exclude_col) && exclude_col %in% names(dat)) {
    usable <- usable & (is.na(dat[[exclude_col]]) | dat[[exclude_col]] %in% "")
  }
  dat <- dat[usable, , drop = FALSE]
  group_vars <- intersect(conc_obj$columns$groups$group_vars, names(output))
  data_keys <- do.call(paste, c(unname(dat[c(group_vars, conc_obj$columns$time)]), sep = "\r"))
  output_keys <- do.call(paste, c(unname(output[c(group_vars, "end")]), sep = "\r"))
  output_keys %in% data_keys
}
