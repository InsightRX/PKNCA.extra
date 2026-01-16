#' Create a table for NCA results
#' 
#' Confirms with layout of table commonly used in PK reports, i.e. where
#' stats are outputted in long format using a single column.
#' 
#' @param nca_data output data from NCA, generated using `run_nca()`
#' @param parameters vector of parameters to output, e.g. `c("AUCALL", "CMAX",
#' "TMAX")`. If `NULL`, defaults to output all available parameters.
#' @param groups name of variable in dataset to group statistics by, e.g. 
#' `"ACTARM"`
#' @param path optional, path to filename to save output table to.
#' @param format output as table in wide or long format.
#' @param style style of output table. Default is `simple`, in which the output
#' is essentially a regular data.frame that can easily be used for further 
#' parsing. The `pretty` style is intended for printing or inclusion in reports: 
#' when multiple rows are in the table for a single parameter or interval, it 
#' will leave blanks on the subsequent rows for parameter and description.
#' @param description include description of parameter names into table?
#' @param specification specification file with settings for NCA. Needs to match
#' the `specification` that was used in `run_nca()` to properly map column 
#' names.
#' @param prefix prefix all column names with this, e.g. `superpos_`.
#' @param verbose Verbose?
#' 
#' @export
create_nca_table <- function(
    nca_data,
    parameters = NULL,
    groups = NULL,
    path = NULL,
    format = c("wide", "long"),
    style = c("simple", "pretty"),
    specification = "default",
    description = TRUE,
    prefix = NULL,
    verbose = TRUE
) {
  
  ## arguments
  format <- match.arg(format)
  style <- match.arg(style)
  
  ## Parameters
  if(is.null(parameters)) {
    parameters <- get_parameter_names_from_nca_data(nca_data)
  }
  ## Add `accum_<parameter>` parameters, if in dataset
  accum_params <- names(nca_data)[grep("accum_", names(nca_data))]
  if(length(accum_params) > 0) {
    if(verbose) cli::cli_alert_info("Adding accumulation ratio parameters")
    parameters <- unique(c(parameters, accum_params))
  }
  
  format <- match.arg(format, c("long", "wide"))
  dict <- utils::read.csv(file = system.file(
    package = "PKNCA.extra", "md/data_dictionary_nca.csv"
  ))
  if(length(accum_params) > 0) {
    for(par in accum_params) {
      dict <- dplyr::bind_rows(
        dict,
        data.frame(
          "Object" = par,
          "Type" = "parameter",
          "Description" = paste0("Accum. ratio ", par)
        )
      )
    }
  }
  ## Add partial AUC to dictionary
  idx_auc_partial <- grep("auc_partial", names(nca_data))
  if(length(idx_auc_partial) > 0) {
    for(d in idx_auc_partial) {
      auc_name <- names(nca_data)[d]
      auc_intv <- stringr::str_split(auc_name, "_")[[1]]
      dict <- dplyr::bind_rows(
        dict,
        data.frame(
          "Object" = auc_name,
          "Type" = "parameter",
          "Description" = paste0("Partial AUC (", auc_intv[3], " - ", auc_intv[4], ")")
        )
      )
    }
  }
  ## add copy of description in dictionary for parameters with underscores instead of dots
  underscore_copy <- dict |>
    dplyr::filter(stringr::str_detect(.data$Object, "\\.")) |>
    dplyr::mutate(Object = stringr::str_replace_all(.data$Object, "\\.", "_"))
  if(nrow(underscore_copy) > 0) {
    dict <- dplyr::bind_rows(dict, underscore_copy)
  }
  
  spec <- read_spec_from_file(specification)
  mapping <- spec$nca$pknca.parameter_mapping
  if(!is.null(mapping) && length(mapping) > 0) {
    tmp <- data.frame(
      Object = unlist(mapping),
      Object.mapped = names(mapping)
    )
    dict <- dict %>%
      dplyr::left_join(tmp) %>%
      dplyr::mutate(Object.mapped = dplyr::if_else(is.na(.data$Object.mapped), .data$Object, .data$Object.mapped))
  } else {
    dict$Object.mapped <- dict$Object
  }
  parameters <- parameters[parameters %in% names(nca_data)]
  if(length(parameters) == 0) {
    message("Requested parameters not found in data. Please check NCA specification and arguments to run_nca().")
    return()
  }
  tmp <- nca_data %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(parameters)) %>%
    dplyr::group_by(.data$name, .data$nca_interval, .add = TRUE) %>%
    dplyr::rename(Interval = "nca_interval")

  if(!is.null(groups)) {
    tmp <- tmp %>%
      dplyr::group_by_at(c("name", groups))
  }

  ## capturing statistics
  nca_table <- tmp %>%
    dplyr::summarise(
      Interval = .data$Interval[1],
      geom_mean = mean_geom(.data$value, na.rm = TRUE),
      geom_cv_pct = cv_geom(.data$value, na.rm = TRUE),
      arithm_mean = mean(.data$value, na.rm = TRUE),
      arithm_sd = stats::sd(.data$value, na.rm = TRUE),
      arithm_cv_pct = 100*.data$arithm_sd/.data$arithm_mean,
      median = stats::median(.data$value, na.rm = TRUE),
      pct_5 = stats::quantile(.data$value, .05, na.rm = TRUE),
      pct_95 = stats::quantile(.data$value, .95, na.rm = TRUE),
      min = suppressWarnings(min(.data$value, na.rm = TRUE)),
      max = suppressWarnings(max(.data$value, na.rm = TRUE)),
      nca_start = .data$nca_start[1],
      nca_end = .data$nca_end[1]
    ) %>%
    dplyr::mutate(dplyr::across("geom_mean":"max", ~ round(.x, 2) )) %>%
    dplyr::arrange_at(c("name", "nca_start", "nca_end")) %>%
    dplyr::rename(Parameter = "name") %>%
    dplyr::select(-dplyr::all_of(c("nca_start", "nca_end")))
  
  ## grouping and formatting output
  if(format == "long") {
    nca_table <- nca_table %>%
      tidyr::pivot_longer(cols = c("geom_mean":"max"))
    if(!is.null(groups)) {
      nca_table <- nca_table %>%
        tidyr::pivot_wider(names_from = tidyselect::all_of(groups))
    }
    nca_table <- nca_table %>%
      dplyr::rename(Statistic = "name") %>%
      dplyr::group_by(.data$Parameter)
  }
  
  ## add description of parameters
  if(isTRUE(description) && length(dict$Object.mapped) > 0) {
    nca_table <- nca_table %>%
      dplyr::left_join(
        dplyr::select(dict, Object = "Object.mapped", Description = "Description"),
        by = c("Parameter" = "Object")
    ) %>%
      dplyr::select("Parameter", "Description", dplyr::everything())
  }
  
  if(style == "pretty") {
    nca_table <- nca_table %>%
      dplyr::mutate(
        Description = ifelse(duplicated(.data$Description), "", .data$Description)
      ) |>
      dplyr::mutate(
        Parameter = ifelse(duplicated(.data$Parameter), "", .data$Parameter)
      )
  }
  
  ## Prefix?
  if(!is.null(prefix)) {
    if(length(prefix) != 1 || ! inherits(prefix, "character")) {
      cli::cli_abort("`prefix` argument can only be a character object with length of 1.")
    }
    names(nca_table) <- paste0(prefix[1], names(nca_table))
  }

  ## save to file
  if(!is.null(path)) {
    utils::write.csv(
      nca_table, 
      file = path, 
      row.names = FALSE, 
      quote = FALSE
    )
  }
  
  nca_table
}

## stats functions
mean_geom <- function(x, ...) exp(mean(log(x), ...))
cv_geom <- function(x, ...) 100 * exp(stats::var(log(x), ...) - 1)

#' Get parameter names from a run_nca() output object
#' 
#' @param nca_data output data from NCA, generated using `run_nca()`
get_parameter_names_from_nca_data <- function(nca_data) {
  tmp <- attr(nca_data, "PKNCA_object")
  unique(tmp$result$PPTESTCD)
}
