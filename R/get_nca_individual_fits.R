#' Extract individul fit data from a PKNCA results object, and return as 
#' data.frame convenient for plotting.
#' 
#' @param nca_obj object returned from PKNCA with NCA results.
#' @param n_grid_points number of grid points to simulate for the fitted line.
#' Usually 40 is more than enough.
#' @param exclusions a vector of points to remove ## TODO
#' @param dictionary list object, mapping column names to expected variables.
#' Any specified elements, if present in `nca_obj$data$conc$data`, will be 
#' exported in the output object.
#' 
#' @importFrom irxutils underscores_to_dots
#' 
#' @export
#' 
get_nca_individual_fits <- function(
  nca_obj = NULL,
  n_grid_points = 6,
  dictionary = NULL
) {
  if(!inherits(nca_obj, "PKNCAresults")) {
    stop("The object supplied to `nca_obj` is not a result object returned from PKNCA.")
  }
  
  ## Get grouping info
  group_names <- nca_obj$data$conc$columns$groups$group_vars
  
  ## get fit parameters
  no_dots <- any(grepl("_", nca_obj$result$PPTESTCD))
  reqd_params <- c("tlast", "clast.pred", "lambda.z", "lambda.z.time.first", "r.squared", "adj.r.squared")
  if(no_dots) {
    nca_obj$result$PPTESTCD <- underscores_to_dots(nca_obj$result$PPTESTCD)
  }
  fit_data_wide <- nca_obj$result %>%
    tidyr::pivot_wider(names_from = PPTESTCD, values_from = PPORRES)
  if(!all(reqd_params %in% names(fit_data_wide))) {
    stop("Not all fit parameters available, cannot create individual fit data.")    
  }
  fit_data <- fit_data_wide %>%
    dplyr::filter(!is.na(lambda.z)) %>%
    dplyr::select(
      !!group_names, 
      start, tlast, clast.pred, 
      lambda.z, lambda.z.n.points, lambda.z.time.first,
      r.squared, adj.r.squared
    ) %>%
    dplyr::group_by_at(group_names) %>%
    dplyr::mutate(
      intercept = log(clast.pred) + tlast * lambda.z, 
      slope = -lambda.z
    )

  ## Get variable info from PKNCA object
  vars <- nca_obj$data$conc$columns
  conc_vars <- all.vars(nca_obj$data$conc$formula)
  export <- NULL
  if(!is.null(dictionary) && inherits(dictionary, "list")) {
    export <- unlist(dictionary)
    if(!all(export %in% names(nca_obj$data$conc$data))) {
      stop("Not all requested columns in `dictionary` are present in input data.")
    }
  }
  obs_all <- nca_obj$data$conc$data[, unique(c(conc_vars, export, "exclude"))]
  
  ## Generate predictions for a grid from tfirst to tlast
  predictions <- dplyr::bind_rows(
    purrr::pmap(
      .l = fit_data,
      .f = function(...) {
        x <- dplyr::tibble(...)
        grid <- seq(from = x$lambda.z.time.first, to = x$tlast, length.out = 40)
        data.frame(
          time = x$start + grid,
          prediction = exp(x$intercept + grid * x$slope)
        ) %>%
          dplyr::bind_cols(x %>% dplyr::select(!!group_names)) %>%
          dplyr::rename(!!vars$time := time)
      }
    )
  )
  
  ## Get all observed data, including points not included in fits
  ## Add a flag `used_in_fit` for datapoints that were used in fit
  comb <- dplyr::left_join(
    obs_all,
    fit_data %>%
      dplyr::select(!!group_names, lambda.z.n.points, r.squared, adj.r.squared),
    by = group_names
  ) %>%
    dplyr::group_by_at(group_names) %>%
    dplyr::arrange_at(c(group_names, "exclude", vars$time)) %>%
    ## Make sure not to flag BLQ points as used_in_fit
    dplyr::mutate(blq = dplyr::if_else(.data[[vars$concentration]] == 0, 1, 0)) %>%
    dplyr::mutate(tmp_idx = length(exclude):1 - sum(blq)) %>% 
    dplyr::mutate(used_in_fit = ifelse(tmp_idx < lambda.z.n.points & tmp_idx >= 0, 1, 0)) %>%
    dplyr::select(-tmp_idx) %>%
    dplyr::arrange_at(c(group_names, vars$time)) %>%
    dplyr::rename(
      n_points = lambda.z.n.points,
      r_squared = r.squared,
      adj_r_squared = adj.r.squared
    )

  ## Combine in a single data.frame, and add single unique grouping column
  out <- dplyr::bind_rows(
    comb %>%
      dplyr::mutate(
        excluded = as.integer(!is.na(exclude)),
        exclude_reason = exclude
    ) %>%
      dplyr::select(-exclude),
    predictions %>%
      dplyr::mutate(used_in_fit = 0)
  ) %>%
    dplyr::arrange_at(c(group_names, vars$time))
  group_names_clean <- group_names[! group_names %in% vars$subject]
  if(length(group_names_clean) > 0) {
    out <- out %>%
      tidyr::unite(treatment_group, all_of(group_names_clean), remove = FALSE)
  }

  out
}