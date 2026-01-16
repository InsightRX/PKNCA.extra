#' NCA post-processing function to calculate accumulation rate 
#' 
#' Will only be able to calculate accumulation ratio when data is multi-
#' interval.
#' @param data NCA output data
#' @param dictionary data dictionary
#' @param options options for this function:
#' 
#' - `parameters`: vector of parameter names to calculate accumulation ratios for.
#' 
#' @param groups grouping variable
#' @param verbose verbosity
#' @export
#' 
nca_post_accumulation <- function(
  data,
  dictionary,
  options = list(
    parameters = c("auctau", "cmax")
  ),
  groups = NULL,
  verbose = FALSE
) {

  # Create grouping variables
  group_vars <- c(dictionary$subject_id, groups)
  
  # Check if there are any subjects with multiple intervals
  has_multiple_intervals <- data %>%
    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
    dplyr::summarise(n_intervals = dplyr::n_distinct(.data$nca_start)) %>%
    dplyr::filter(.data$n_intervals > 1) %>%
    nrow() > 0

  # Return original data if no subjects have multiple intervals
  if (!has_multiple_intervals) {
    if(verbose) cli::cli_alert_info("Single-interval data, not adding accumulation ratios.")
    return(data)
  }
  
  # Return original data if no parameters requested
  if (is.null(options$parameters) || length(options$parameters) == 0) {
    if(verbose) cli::cli_alert_info("No parameters requested to calculate accumulation ratio for.")
    return(data)
  }

  # Validate inputs
  parameters <- options$parameters[options$parameters %in% names(data)]
  if(length(parameters) == 0) {
    cli::cli_alert_warning(
      paste0(
        "None of the requested parameters were found, not calculating accumulation rates."
      )
    )
    return(data)
  }
  if(length(parameters) < length(options$parameters)) {
    cli::cli_alert_warning(
      paste0(
        "Not all requested parameters were found, not calculating accumulation for: ", 
        paste0(
          options$parameters[! options$parameters %in% names(data)], 
          collapse = ", "
        )
      )
    )
  }
  required_cols <- c(dictionary$subject_id, "nca_start", "nca_end", parameters)
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns: ", 
         paste(setdiff(required_cols, names(data)), collapse = ", "))
  }
  
  # Ensure required columns exist
  required_cols <- c(dictionary$subject_id, "nca_start", "nca_end")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  if(verbose) cli::cli_alert_info("Calculating accumulation ratios")

  # Calculate accumulation ratios
  out <- data %>%
    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
    dplyr::arrange(.data$nca_start, .by_group = TRUE) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(parameters),
        ~ .x / dplyr::first(.x),
        .names = "accum_{.col}"
      )
    ) %>%
    dplyr::ungroup()
  
  return(out)
}

#' NCA post-processing function to calculate relative bioavailability
#' 
#' Will only be able to calculate relative bioavailability when data 
#' has a grouper to distinguish the test and reference arms, e.g.
#' 
#' 
#' 
#' @param data NCA output data
#' @param dictionary data dictionary
#' @param options options for this function:
#' 
#' - `parameters`: vector of parameter names to calculate ratio for.
#' - `arm`: the column name used to indicate the varios arms of the study, e.g.
#' fasting / fed, or iv / oral, etc. Can have multiple test arms, but only one
#' `reference`.
#' - `reference`: the label of the reference arm in `group` column, e.g. "fasted"
#' 
#' @param groups grouping variable
#' @param verbose verbosity
#' @export
#' 
nca_post_bioavailability <- function(
    data,
    dictionary,
    options = list(
      parameters = c("auclast", "cmax"),
      arm = NULL,
      reference = NULL
    ),
    groups = NULL,
    verbose = FALSE
) {
  
  # Validate inputs
  required_cols <- c(dictionary$subject_id, groups, options$parameters)
  if (!all(required_cols %in% names(data))) {
    cli::cli_alert_warning(paste0(
      "Missing required columns: ", paste(setdiff(required_cols, names(data)), collapse = ", ")
    ), ". Skipping bioavailability post-processing step.")
    return(data)
  }
  
  if(is.null(options$parameters) || length(options$parameters) == 0) {
    cli::cli_alert_warning("`parameters` for bioavailability calculation not specified. Skipping bioavailability post-processing step.")
    return(data)
  }
  if(! all(options$parameters %in% names(data))) {
    cli::cli_alert_warning("Not all `parameters` were found in data. Skipping bioavailability post-processing step.")
    return(data)
  }
  
  # Check if reference treatment exists
  if(is.null(options$arm)) {
    cli::cli_alert_warning("Column indicating `arm` for bioavailability calculation not specified. Skipping bioavailability post-processing step.")
    return(data)
  }
  if(! options$arm %in% names(data)) {
    cli::cli_alert_warning("Column indicating `arm` not found in data. Skipping bioavailability post-processing step.")
    return(data)
  }
  if(! options$reference %in% data[[options$arm]]) {
    cli::cli_alert_warning(paste0("Reference treatment '", options$reference, "' not found in data. Skipping bioavailability post-processing step."))
    return(data)
  }
  
  # Get reference values for each subject
  reference_values <- data %>%
    dplyr::filter(!!rlang::sym(options$arm) == options$reference) %>%
    dplyr::select(!!dictionary$subject_id, !!!rlang::syms(options$parameters))
  
  if (verbose) {
    cli::cli_alert_info(paste0(
      "Calculating relative bioavailability using '", 
      options$reference, 
      "' as reference"
    ))
  }
  
  # Calculate relative bioavailability
  result <- data %>%
    # Join with reference values
    dplyr::left_join(
      reference_values,
      by = dictionary$subject_id,
      suffix = c("", "_ref")
    )
  
  # Add ratio columns one by one
  for (param in options$parameters) {
    ref_col <- paste0(param, "_ref")
    ratio_col <- paste0("relbio_", param)
    result[[ratio_col]] <- result[[param]] / result[[ref_col]]
  }
  
  # Remove reference columns
  result <- result %>%
    dplyr::select(-dplyr::ends_with("_ref"))

  result
  
}
