#' Add AUCtau to the NCA output, if multi-dose PK data
#' 
#' Will copy the interval AUC to `auctau` in the NCA output, for
#' those intervals where the end of the interval is <Inf.
#' 
#' The interval AUC is taken from `auc_col` if specified (as done by
#' [run_nca()], using the values from [compute_auctau()]), otherwise from the
#' first available column of `aucint.last.dose`, `aucint.last` (or their
#' `no_dots` variants). These
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
#' @param auc_col name of the column in `output` to use for `auctau`. If
#' `NULL` (default), the column is detected as described above.
#' @param verbose verbose output?
#' 
#' @returns 
#' NCA output data with `auctau` added.
#' 
#' @export
nca_add_auctau <- function(
  output,
  dictionary,
  auc_col = NULL,
  verbose = FALSE
) {
  
  if(is.null(auc_col) || !auc_col %in% names(output)) {
    aucint_cols <- c("aucint.last.dose", "aucint_last_dose", "aucint.last", "aucint_last")
    auc_col <- intersect(aucint_cols, names(output))[1]
  }
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

#' Compute AUCtau for all bounded NCA intervals
#'
#' Runs a separate PKNCA pass over all intervals with a finite end in the
#' `PKNCAdata` object, requesting only interval AUCs. This avoids adding these
#' parameters (and their dependencies, such as lambda-z) to the main NCA
#' output.
#'
#' With dose data, AUCtau is PKNCA's `aucint.last.dose`, which interpolates to
#' the end of the interval. When the end of the interval is the time of the
#' next dose, `aucint.inf.obs.dose` is used instead: it extrapolates from the
#' last sample before the dose using lambda-z, and does not use samples taken
#' after that dose. If lambda-z could not be estimated, it falls back to
#' `aucint.last.dose`. Without dose data, `aucint.last` is used.
#'
#' Note: PKNCA's `aucint.*.dose` parameters do not take the route of
#' administration into account at the start of the interval, so for IV bolus
#' the start of the interval is handled by the imputation settings.
#'
#' @param comb_obj `PKNCAdata` object
#'
#' @returns data.frame with group columns, `start`, `end` and `auctau`, or
#' `NULL` if there are no bounded intervals or the calculation failed.
#'
#' @keywords internal
compute_auctau <- function(comb_obj) {
  intervals <- as.data.frame(comb_obj$intervals)
  intervals <- intervals[is.finite(intervals$end), , drop = FALSE]
  if(nrow(intervals) == 0) return(NULL)
  has_dose <- inherits(comb_obj$dose, "PKNCAdose")
  param_cols <- setdiff(intersect(names(PKNCA::get.interval.cols()), names(intervals)), c("start", "end"))
  intervals[param_cols] <- FALSE
  if(has_dose) {
    intervals$aucint.last.dose <- TRUE
    intervals$aucint.inf.obs.dose <- is_end_at_dose(intervals, comb_obj)
  } else {
    intervals$aucint.last <- TRUE
  }
  auctau_obj <- comb_obj
  auctau_obj$intervals <- unique(intervals)
  res <- tryCatch(
    suppressWarnings(PKNCA::pk.nca(auctau_obj)),
    error = function(e) {
      cli::cli_alert_warning(paste0("Calculation of `auctau` using `aucint` failed: ", conditionMessage(e)))
      NULL
    }
  )
  if(is.null(res)) return(NULL)
  result <- as.data.frame(res$result)
  value_col <- ifelse("PPSTRES" %in% names(result), "PPSTRES", "PPORRES")
  keys <- c(intersect(comb_obj$conc$columns$groups$group_vars, names(result)), "start", "end")
  auc_last <- result[result$PPTESTCD %in% c("aucint.last.dose", "aucint.last"), c(keys, value_col)]
  auc_inf <- result[result$PPTESTCD %in% "aucint.inf.obs.dose", c(keys, value_col)]
  names(auc_last)[names(auc_last) == value_col] <- "auc_last"
  names(auc_inf)[names(auc_inf) == value_col] <- "auc_inf"
  auc_last |>
    dplyr::left_join(auc_inf, by = keys) |>
    dplyr::mutate(auctau = dplyr::coalesce(.data$auc_inf, .data$auc_last)) |>
    dplyr::select(dplyr::all_of(c(keys, "auctau"))) |>
    as.data.frame()
}

#' Check for each interval whether its end is at the time of a dose
#'
#' @param intervals data.frame with intervals, optionally with group columns
#' @param comb_obj `PKNCAdata` object
#'
#' @returns logical vector, one value per row in `intervals`
#'
#' @keywords internal
is_end_at_dose <- function(intervals, comb_obj) {
  dose_data <- as.data.frame(comb_obj$dose$data)
  dose_time <- comb_obj$dose$columns$time
  group_vars <- intersect(comb_obj$dose$columns$groups$group_vars, names(intervals))
  if(length(group_vars) == 0) {
    return(intervals$end %in% dose_data[[dose_time]])
  }
  dose_keys <- do.call(paste, c(dose_data[c(group_vars, dose_time)], sep = "\r"))
  interval_keys <- do.call(paste, c(intervals[c(group_vars, "end")], sep = "\r"))
  interval_keys %in% dose_keys
}

#' Add dosing intervals skipped by PKNCA for multi-dose data
#'
#' For groups with multiple doses, PKNCA only selects a dosing interval when a
#' concentration was measured exactly at both dose times, and the interval
#' after the last dose only when a concentration was measured exactly at the
#' last dose time + tau. So when the trough sample was drawn slightly late or
#' early, the dosing interval is not analyzed at all. This function adds those
#' dosing intervals (with `auclast`, `cmax` and `tmax`, as PKNCA does), when:
#'
#' - for intervals between two doses: there is at least one concentration
#' within the interval.
#' - for the interval after the last dose (with tau determined by PKNCA): there
#' is at least one concentration within the interval, and one at or after its
#' end.
#'
#' @param comb_obj `PKNCAdata` object
#' @param verbose verbose output?
#'
#' @returns `PKNCAdata` object with updated `intervals`
#'
#' @keywords internal
add_missing_dosing_intervals <- function(comb_obj, verbose = FALSE) {
  if(!inherits(comb_obj$dose, "PKNCAdose")) return(comb_obj)
  group_vars <- comb_obj$dose$columns$groups$group_vars
  if(!all(group_vars %in% names(comb_obj$intervals))) return(comb_obj)
  dose_data <- as.data.frame(comb_obj$dose$data)
  dose_time <- comb_obj$dose$columns$time
  conc_data <- as.data.frame(comb_obj$conc$data)
  conc_data <- conc_data[!is.na(conc_data[[comb_obj$conc$columns$concentration]]), , drop = FALSE]
  conc_time <- comb_obj$conc$columns$time
  existing <- as.data.frame(comb_obj$intervals)
  existing_keys <- do.call(paste, c(existing[c(group_vars, "start", "end")], sep = "\r"))

  dose_groups <- unique(dose_data[group_vars])
  new_intervals <- list()
  for(i in seq_len(nrow(dose_groups))) {
    group <- dose_groups[i, , drop = FALSE]
    in_group <- function(dat) {
      Reduce(`&`, lapply(group_vars, function(g) dat[[g]] %in% group[[g]]))
    }
    time_dose <- sort(unique(dose_data[[dose_time]][in_group(dose_data)]))
    if(length(time_dose) < 2) next
    time_conc <- conc_data[[conc_time]][in_group(conc_data)]
    tau <- PKNCA::find.tau(time_dose)
    starts <- time_dose
    ends <- c(time_dose[-1], max(time_dose) + tau)
    for(j in seq_along(starts)) {
      if(is.na(ends[j])) next
      has_conc <- any(time_conc > starts[j] & time_conc < ends[j])
      if(j == length(starts)) {
        has_conc <- has_conc && any(time_conc >= ends[j])
      }
      key <- do.call(paste, c(as.list(group), starts[j], ends[j], sep = "\r"))
      if(has_conc && !key %in% existing_keys) {
        new_intervals[[length(new_intervals) + 1]] <- cbind(
          PKNCA::check.interval.specification(
            data.frame(start = starts[j], end = ends[j], auclast = TRUE, cmax = TRUE, tmax = TRUE)
          ),
          group,
          row.names = NULL
        )
      }
    }
  }
  if(length(new_intervals) > 0) {
    if(verbose)
      cli::cli_alert_info(paste0("Adding ", length(new_intervals), " dosing interval(s) without a sample exactly at the dose time"))
    comb_obj$intervals <- dplyr::bind_rows(comb_obj$intervals, new_intervals) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c(group_vars, "start"))))
  }
  comb_obj
}
