#' Add AUCtau to the NCA output, if multi-dose PK data
#' 
#' Will copy `auclast` to `auctau` in the NCA output, for
#' those intervals where the end of the interval is <Inf.
#' 
#' It will first check if the NCA was indeed multi-dose,
#' as this is not relevant variable for single-dose data.
#'
#' @param output PKNCA output object
#' @param dictionary data dictionary
#' @param verbose verbose output?
#'  
#' @export
#' 
nca_add_auctau <- function(
  output,
  dictionary,
  verbose = FALSE
) {
  
  if("auclast" %in% names(output)) {
    ## Check that there are intervals where we need to add it.
    ## We don't want to add auctau for single dose data
    n_rows_to_update <- output |>
      dplyr::filter(!is.na(.data$auclast) & .data$nca_end < Inf) |>
      nrow()
    if(n_rows_to_update == 0) {
      if(verbose)
        cli::cli_alert_info("Not adding `auctau`, no multi-dose intervals detected in NCA output")
    } else{
      if(verbose)
        cli::cli_alert_info("Adding auctau for relevant subjects / intervals")
      output <- output |>
        dplyr::mutate(
          auctau = dplyr::if_else(.data$nca_end < Inf, .data$auclast, NA)
        )
    }
  } else {
    cli::cli_alert_warning("Column `auclast` not found in NCA data, not adding `auctau` variable.")
  }

  output
  
}
