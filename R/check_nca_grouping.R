#' Check that `grouping` specified to PKNCA is likely to be correct
#' If the grouping results in most groups having too few datapoints to
#' perform NCA and calculate half-life, then the grouping is likely wrong.
#' 
#' @inheritParams run_nca
#' @param threshold the fraction of data to be allowed to have too few data
#' points for NCA.
#' 
#' @returns `TRUE` or `FALSE`
#' @export
#' 
check_nca_grouping <- function(
  data,
  groups,
  dictionary,
  settings = list(),
  threshold = 0.7,
  verbose = TRUE
) {
  
  ## Group data, and get number of observed datapoints
  if(is.null(dictionary$subject_id)) {
    cli::cli_abort("Need a `dictionary` with `subject_id` entry.")
  }
  grouped_data <- data |>
    dplyr::group_by_at(dplyr::all_of(c(dictionary$subject_id, groups)))
  if(!is.null(dictionary$conc)) {
    grouped_data <- grouped_data |>
      dplyr::filter(!is.na(!!dictionary$conc)) |>
      dplyr::filter(!!dictionary$conc > 0)
  }
  n_unique <- grouped_data |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull(n)

  ## Get number of groups where number of samples is lower than the minimum number of samples needed for half-life
  min_points <- settings$minPointsRegression
  if(is.null(min_points)) min_points <- 3
  fraction_insufficient <- sum(n_unique < min_points) / length(n_unique)

  ## Return result
  if(fraction_insufficient > threshold) {
    if(verbose) cli::cli_warn("Many groups {round(fraction_insufficient*100)}%) have too few data points to run NCA.")
    return(FALSE)
  } else {
    if(verbose) cli::cli_alert_info("Groups in data seem to have sufficient concentration points too run NCA.")
    return(TRUE)
  }
}