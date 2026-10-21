#' Add AUCtau to the NCA output, if multi-dose PK data
#' 
#' Will copy the interval AUC to `auctau` in the NCA output, for
#' those intervals where the end of the interval is <Inf.
#' 
#' The interval AUC is taken from the first available column of
#' `aucint.last.dose`, `aucint.last` (or their `no_dots` variants). These
#' interpolate/extrapolate the concentration to the end of the interval, so
#' AUCtau is not biased low when the sample at the end of the dosing interval
#' was drawn late. The dose-aware variant does not use samples after the next
#' dose for interpolation. If none of these columns is available, `auclast` is
#' used instead (with a warning), which only integrates up to the last sample
#' at or before the end of the interval.
#' 
#' It will first check if the NCA was indeed multi-dose,
#' as this is not relevant variable for single-dose data.
#'
#' @param output PKNCA output object
#' @param dictionary data dictionary
#' @param verbose verbose output?
#' 
#' @returns 
#' NCA output data with `auctau` added.
#' 
#' @export
nca_add_auctau <- function(
  output,
  dictionary,
  verbose = FALSE
) {
  
  aucint_cols <- c("aucint.last.dose", "aucint_last_dose", "aucint.last", "aucint_last")
  auc_col <- intersect(aucint_cols, names(output))[1]
  if(is.na(auc_col) && "auclast" %in% names(output)) {
    if(any(output$nca_end < Inf & !is.na(output$auclast)))
      cli::cli_alert_warning("Column `aucint.last.dose` / `aucint.last` not found in NCA data, using `auclast` for `auctau`. This may underestimate AUCtau when the last sample was drawn after the end of the interval.")
    auc_col <- "auclast"
  }
  if(!is.na(auc_col)) {
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
    cli::cli_alert_warning("Column `auclast` not found in NCA data, not adding `auctau` variable.")
  }

  output
  
}
