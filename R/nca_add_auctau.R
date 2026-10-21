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
#' dose for interpolation. If none of these columns is available, `auctau` is
#' set to `NA` (with a warning). `auclast` is not used, since it only
#' integrates up to the last sample at or before the end of the interval and
#' therefore underestimates AUCtau when that sample was drawn late.
#' 
#' `auctau` is only added when there is at least one interval with a finite
#' end, as it is not a relevant variable for e.g. single-dose data analyzed
#' over `[0, Inf]`. Rows for intervals with an infinite end get `NA`.
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
  is_bounded <- output$nca_end < Inf
  if(!any(is_bounded)) {
    if(verbose)
      cli::cli_alert_info("Not adding `auctau`, no intervals with a finite end detected in NCA output")
    return(output)
  }
  if(verbose)
    cli::cli_alert_info("Adding auctau for relevant subjects / intervals")
  if(is.na(auc_col)) {
    cli::cli_alert_warning("No interval AUC (`aucint.*`) found in NCA data, setting `auctau` to NA. `auclast` is not used, since it underestimates AUCtau when the last sample was drawn after the end of the interval.")
    output$auctau <- NA_real_
  } else {
    output$auctau <- dplyr::if_else(is_bounded, as.numeric(output[[auc_col]]), NA_real_)
  }

  output
  
}

#' Compute the AUC over given intervals in a separate PKNCA pass
#'
#' Runs a separate PKNCA pass over the given intervals, requesting only the
#' interval AUC. This avoids adding these parameters (and their dependencies,
#' such as lambda-z) to the main NCA output.
#'
#' With dose data, the AUC is PKNCA's `aucint.last.dose`, which interpolates to
#' the start and end of the interval (`aucint.last` without dose data). When
#' there is no usable concentration at exactly the end of the interval, and
#' the end cannot be interpolated because there is no measurable concentration
#' after it, or because it is at the time of a dose (samples after a dose
#' cannot be used), `aucint.last(.dose)` would set the concentration at the end
#' to zero. In that case `aucint.inf.obs.dose` (`aucint.inf.obs` without dose
#' data) is used instead, which extrapolates from the last sample using
#' lambda-z. If lambda-z could not be estimated, the AUC is `NA` and a warning
#' is shown. Excluded samples are not considered usable.
#'
#' Intervals spanning one or more doses are split at those doses, and the AUC
#' is computed for each part as described here, and summed. So the
#' concentration just before a dose without a sample at that time is
#' extrapolated using lambda-z of that part.
#'
#' PKNCA computes `aucint.*` from the non-imputed concentrations, and the
#' dose-aware variants don't take the route of administration into account.
#' So for a part starting at a dose without a usable sample at that time, the
#' AUC from the dose to the last measurable sample in the part is corrected:
#' `aucint` is replaced by `auclast`, which uses the start-of-interval
#' imputation (e.g. C0 back-extrapolation for an IV bolus).
#'
#' @param comb_obj `PKNCAdata` object
#' @param intervals data.frame with `start` and `end`, and optionally group
#' columns. Rows without group columns apply to all groups.
#' @param label name of the calculated AUC, used in warnings.
#'
#' @returns data.frame with group columns, `start`, `end` and `auc`, or
#' `NULL` if there are no intervals or the calculation failed.
#'
#' @keywords internal
compute_interval_auc <- function(comb_obj, intervals, label = "AUC") {
  intervals <- as.data.frame(intervals)
  if(nrow(intervals) == 0) return(NULL)
  param_cols <- setdiff(intersect(names(PKNCA::get.interval.cols()), names(intervals)), c("start", "end"))
  intervals[param_cols] <- FALSE
  has_dose <- inherits(comb_obj$dose, "PKNCAdose")
  param_last <- ifelse(has_dose, "aucint.last.dose", "aucint.last")
  param_inf <- ifelse(has_dose, "aucint.inf.obs.dose", "aucint.inf.obs")

  ## needs to be per group, to check sample (and dose) times
  group_vars <- comb_obj$conc$columns$groups$group_vars
  missing_groups <- setdiff(group_vars, names(intervals))
  if(length(missing_groups) > 0) {
    groups <- unique(as.data.frame(comb_obj$conc$data)[missing_groups])
    intervals <- merge(intervals, groups, by = NULL) # cartesian product
  }
  keys <- c(intersect(group_vars, names(intervals)), "start", "end")

  ## Split intervals at doses within the interval, and compute the AUC for
  ## each part separately. Each part is then handled as a dosing interval:
  ## extrapolation to the next dose, and imputation at the start of the dose.
  intervals$.parent <- seq_len(nrow(intervals))
  parents <- intervals
  if(has_dose) {
    intervals <- split_at_doses(intervals, comb_obj)
  }

  ## Extrapolate when the end of the interval can't be interpolated from usable
  ## samples: no sample at `end`, and either no measurable sample after `end`
  ## or a dose at `end` (samples after the dose can't be used).
  is_dose_at_end <- rep(FALSE, nrow(intervals))
  if(has_dose) {
    is_dose_at_end <- is_time_in_data(intervals, comb_obj$dose)
  }
  is_bracketed <- is_time_in_data(intervals, comb_obj$conc, observed = TRUE) |
    (is_conc_after_end(intervals, comb_obj$conc) & !is_dose_at_end)
  needs_extrap <- is.finite(intervals$end) & !is_bracketed
  intervals[[param_last]] <- !needs_extrap
  intervals[[param_inf]] <- needs_extrap
  intervals$.id <- seq_len(nrow(intervals))

  ## PKNCA computes `aucint.*` from the non-imputed concentrations, and does
  ## not take the route into account, so the start-of-interval imputation
  ## (e.g. C0 back-extrapolation for an IV bolus, or the pre-dose
  ## concentration) is not used at doses. For each dose in the interval
  ## without a usable sample at that time, correct the AUC over the dose
  ## segment, from the dose to the last measurable sample before the next dose
  ## (or the end of the interval): replace `aucint` by `auclast`, which uses
  ## the imputation. The segment uses the same `aucint` parameter as the full
  ## interval, so the dose is handled identically.
  segments <- data.frame(.id = integer(0), start = numeric(0), end = numeric(0))
  if(has_dose) {
    segments <- get_dose_segments(intervals, comb_obj)
  }
  part_seg <- intervals[match(segments$.id, intervals$.id), , drop = FALSE]
  part_seg$start <- segments$start
  part_seg$end <- segments$end
  part_seg$auclast <- rep(TRUE, nrow(part_seg))
  part_seg$.seg <- seq_len(nrow(part_seg))

  res_int <- run_interval_nca(comb_obj, intervals, c(param_last, param_inf), keys, label)
  part_seg_nca <- part_seg
  part_seg_nca$.seg <- NULL
  res_seg <- run_interval_nca(comb_obj, part_seg_nca, c(param_last, param_inf, "auclast"), keys, label)
  if(is.null(res_int) || is.null(res_seg)) return(NULL)
  n_failed <- sum(res_int$PPTESTCD %in% param_inf & is.na(res_int$value))
  if(n_failed > 0) {
    cli::cli_alert_warning(paste0(
      "`", label, "` not computed for ", n_failed, " interval(s) ending after the last measurable sample ",
      "or at a dose without a sample at that time, because lambda-z could not be estimated for extrapolation."
    ))
  }

  ## Combine per interval. `param` is the parameter to take for each row in
  ## `part`, `id_col` identifies the rows.
  get_values <- function(part, res, param, id_col) {
    part$PPTESTCD <- param
    out <- merge(part[c(id_col, keys, "PPTESTCD")], res, by = c(keys, "PPTESTCD"), all.x = TRUE)
    out$value[match(part[[id_col]], out[[id_col]])]
  }
  param_int <- ifelse(needs_extrap, param_inf, param_last)
  auc <- get_values(intervals, res_int, param_int, ".id")
  correction <- rep(0, nrow(intervals))
  if(nrow(part_seg) > 0) {
    seg_correction <- get_values(part_seg, res_seg, rep("auclast", nrow(part_seg)), ".seg") -
      get_values(part_seg, res_seg, param_int[match(part_seg$.id, intervals$.id)], ".seg")
    seg_correction <- tapply(seg_correction, part_seg$.id, sum)
    idx <- match(as.numeric(names(seg_correction)), intervals$.id)
    correction[idx] <- seg_correction
  }
  auc_parent <- tapply(auc + correction, intervals$.parent, sum)
  out <- parents[keys]
  out$auc <- as.vector(auc_parent[as.character(parents$.parent)])
  rownames(out) <- NULL
  out
}

#' Split intervals at the doses within each interval
#'
#' @param intervals data.frame with `start`, `end`, `.parent` and group columns
#' @param comb_obj `PKNCAdata` object
#'
#' @returns data.frame with one row per part of each interval. If imputation
#' methods are specified per interval (`impute` column), it is set to `NA` for
#' parts not starting at the start of the original interval, so it is assigned
#' again for the start of the part.
#'
#' @keywords internal
split_at_doses <- function(intervals, comb_obj) {
  dose_obj <- comb_obj$dose
  dose_data <- as.data.frame(dose_obj$data)
  dose_time <- dose_data[[dose_obj$columns$time]]
  group_vars <- intersect(dose_obj$columns$groups$group_vars, names(intervals))
  impute_col <- comb_obj$impute
  has_impute_col <- length(impute_col) == 1 && !is.na(impute_col) && impute_col %in% names(intervals)
  parts <- lapply(seq_len(nrow(intervals)), function(i) {
    row <- intervals[i, , drop = FALSE]
    in_group <- Reduce(`&`, lapply(group_vars, function(g) dose_data[[g]] %in% row[[g]]), rep(TRUE, nrow(dose_data)))
    times <- sort(unique(dose_time[in_group & dose_time > row$start & dose_time < row$end]))
    if(length(times) == 0) return(row)
    breaks <- c(row$start, times, row$end)
    out <- row[rep(1, length(breaks) - 1), , drop = FALSE]
    out$start <- breaks[-length(breaks)]
    out$end <- breaks[-1]
    if(has_impute_col) {
      out[[impute_col]][-1] <- NA
    }
    out
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' Run a separate PKNCA pass for the given intervals
#'
#' @param comb_obj `PKNCAdata` object
#' @param intervals data.frame with intervals, including group columns and
#' the requested parameters.
#' @param params names of the parameters to return
#' @param keys group, start and end columns
#' @param label name of the calculated AUC, used in warnings.
#'
#' @returns data.frame with `keys`, `PPTESTCD` and `value`, or `NULL` if the
#' calculation failed.
#'
#' @keywords internal
run_interval_nca <- function(comb_obj, intervals, params, keys, label = "AUC") {
  if(nrow(intervals) == 0) {
    out <- intervals[keys]
    out$PPTESTCD <- character(0)
    out$value <- numeric(0)
    return(out)
  }
  intervals$.id <- NULL
  intervals$.parent <- NULL
  intervals <- unique(intervals)
  auc_obj <- comb_obj
  auc_obj$intervals <- PKNCA::check.interval.specification(intervals)
  ## If imputation methods are specified per interval (column in intervals),
  ## assign them for intervals that don't have one yet (e.g. partial AUC, or
  ## parts of an interval starting at a dose).
  impute_col <- comb_obj$impute
  if(length(impute_col) == 1 && !is.na(impute_col) &&
     impute_col %in% names(comb_obj$intervals)) {
    impute_orig <- intervals[[impute_col]]
    if(is.null(impute_orig)) impute_orig <- rep(NA_character_, nrow(intervals))
    if(anyNA(impute_orig)) {
      auc_obj$intervals[[impute_col]] <- NULL
      auc_obj <- assign_nca_impute(auc_obj)
      if(identical(auc_obj$impute, impute_col)) {
        keep <- !is.na(impute_orig)
        auc_obj$intervals[[impute_col]][keep] <- impute_orig[keep]
      }
    }
  }
  res <- tryCatch(
    suppressWarnings(PKNCA::pk.nca(auc_obj)),
    error = function(e) {
      cli::cli_alert_warning(paste0("Calculation of `", label, "` failed: ", conditionMessage(e)))
      NULL
    }
  )
  if(is.null(res)) return(NULL)
  result <- as.data.frame(res$result)
  value_col <- ifelse("PPSTRES" %in% names(result), "PPSTRES", "PPORRES")
  result <- result[result$PPTESTCD %in% params, , drop = FALSE]
  result$value <- result[[value_col]]
  result[c(keys, "PPTESTCD", "value")]
}

#' Compute AUCtau for all bounded NCA intervals
#'
#' Computes the AUC for all intervals with a finite end in the `PKNCAdata`
#' object, using [compute_interval_auc()].
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
  out <- compute_interval_auc(comb_obj, intervals, label = "auctau")
  if(!is.null(out)) {
    names(out)[names(out) == "auc"] <- "auctau"
  }
  out
}

#' Compute partial AUC for all subjects / groups
#'
#' Computes the AUC over the `partial_auc` interval for every group in the
#' `PKNCAdata` object, using [compute_interval_auc()]. Done separately from
#' the main NCA, since for multi-dose data PKNCA selects the intervals from
#' the dosing schedule.
#'
#' Only the AUC is reported, so other NCA parameter columns in `partial_auc`
#' are ignored (with a warning). An `impute` column is ignored as well: PKNCA
#' computes the `aucint.*` parameters from the full, non-imputed concentration
#' data of the group, interpolating to the start and end of the interval.
#'
#' @param comb_obj `PKNCAdata` object
#' @param partial_auc data.frame with `start` and `end`
#'
#' @returns data.frame with group columns, `start`, `end` and `auc`
#'
#' @keywords internal
compute_partial_auc <- function(comb_obj, partial_auc) {
  ignored_cols <- setdiff(
    intersect(c(names(PKNCA::get.interval.cols()), "impute"), names(partial_auc)),
    c("start", "end")
  )
  if(length(ignored_cols) > 0) {
    cli::cli_alert_warning(paste0(
      "Only the AUC (interpolated to the interval boundaries, without imputation) is reported for `partial_auc`, ignoring columns: ",
      paste(ignored_cols, collapse = ", ")
    ))
  }
  compute_interval_auc(comb_obj, partial_auc[, c("start", "end")], label = "partial AUC")
}

#' Check for each interval whether its end matches a time in PKNCA data
#'
#' @param intervals data.frame with `end` and group columns
#' @param obj `PKNCAconc` or `PKNCAdose` object
#' @param observed only consider rows with a non-missing, non-excluded
#' concentration?
#' @param column column in `intervals` with the time to check
#'
#' @returns logical vector, one value per row in `intervals`
#'
#' @keywords internal
is_time_in_data <- function(intervals, obj, observed = FALSE, column = "end") {
  dat <- as.data.frame(obj$data)
  if(observed) {
    dat <- usable_conc_data(obj)
  }
  time_col <- obj$columns$time
  group_vars <- intersect(obj$columns$groups$group_vars, names(intervals))
  data_keys <- do.call(paste, c(unname(dat[c(group_vars, time_col)]), sep = "\r"))
  interval_keys <- do.call(paste, c(unname(intervals[c(group_vars, column)]), sep = "\r"))
  interval_keys %in% data_keys
}

#' Get the dose segments within each interval that need a start correction
#'
#' For each dose within `[start, end)` of an interval without a usable
#' concentration at the time of the dose, returns the segment from the dose to
#' the last measurable concentration before the next dose (or the end of the
#' interval). Doses without a measurable concentration after them within the
#' segment are skipped.
#'
#' @param intervals data.frame with `.id`, `start`, `end` and group columns
#' @param comb_obj `PKNCAdata` object
#'
#' @returns data.frame with `.id`, `start` and `end` of each segment
#'
#' @keywords internal
get_dose_segments <- function(intervals, comb_obj) {
  conc_obj <- comb_obj$conc
  dose_obj <- comb_obj$dose
  conc_data <- usable_conc_data(conc_obj)
  conc_time <- conc_data[[conc_obj$columns$time]]
  conc_value <- conc_data[[conc_obj$columns$concentration]]
  dose_data <- as.data.frame(dose_obj$data)
  dose_time <- dose_data[[dose_obj$columns$time]]
  group_vars <- intersect(dose_obj$columns$groups$group_vars, names(intervals))
  in_group <- function(dat, i) {
    Reduce(`&`, lapply(group_vars, function(g) dat[[g]] %in% intervals[[g]][i]), rep(TRUE, nrow(dat)))
  }
  segments <- list()
  for(i in seq_len(nrow(intervals))) {
    times_dose <- sort(unique(dose_time[in_group(dose_data, i)]))
    conc_group <- in_group(conc_data, i)
    start <- intervals$start[i]
    end <- intervals$end[i]
    for(d in times_dose[times_dose >= start & times_dose < end]) {
      if(any(conc_group & conc_time == d)) next
      seg_end <- min(c(times_dose[times_dose > d], end))
      t_seg <- conc_time[conc_group & conc_value > 0 & conc_time > d & conc_time <= seg_end]
      if(length(t_seg) == 0) next
      segments[[length(segments) + 1]] <- data.frame(
        .id = intervals$.id[i],
        start = d,
        end = max(t_seg)
      )
    }
  }
  if(length(segments) == 0) {
    return(data.frame(.id = integer(0), start = numeric(0), end = numeric(0)))
  }
  do.call(rbind, segments)
}

#' Add dosing intervals skipped by PKNCA for multi-dose data
#'
#' For groups with multiple doses, PKNCA only selects a dosing interval when a
#' concentration was measured exactly at both dose times, and the interval
#' after the last dose only when a concentration was measured exactly at the
#' last dose time + tau. So when the trough sample was drawn slightly late or
#' early, the dosing interval is not analyzed at all. This function adds those
#' dosing intervals (with `auclast`, `cmax` and `tmax`, as PKNCA does) when
#' there is at least one concentration within the interval. The interval after
#' the last dose uses tau as determined by PKNCA. When sampling stopped before
#' the end of an interval, AUCtau is extrapolated using lambda-z (see
#' [compute_interval_auc()]).
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

#' Check for each interval whether there is a measurable concentration after
#' its end
#'
#' @param intervals data.frame with `end` and group columns
#' @param obj `PKNCAconc` object
#'
#' @returns logical vector, one value per row in `intervals`
#'
#' @keywords internal
is_conc_after_end <- function(intervals, obj) {
  dat <- usable_conc_data(obj)
  dat <- dat[dat[[obj$columns$concentration]] > 0, , drop = FALSE]
  time_col <- obj$columns$time
  group_vars <- intersect(obj$columns$groups$group_vars, names(intervals))
  data_keys <- do.call(paste, c(unname(dat[group_vars]), sep = "\r"))
  interval_keys <- do.call(paste, c(unname(intervals[group_vars]), sep = "\r"))
  if(length(group_vars) == 0) {
    data_keys <- rep("", nrow(dat))
    interval_keys <- rep("", nrow(intervals))
  }
  tlast <- tapply(dat[[time_col]], data_keys, max)
  tlast_interval <- unname(tlast[interval_keys])
  !is.na(tlast_interval) & tlast_interval > intervals$end
}

#' Get concentration data with non-missing, non-excluded concentrations
#'
#' @param obj `PKNCAconc` object
#'
#' @returns data.frame
#'
#' @keywords internal
usable_conc_data <- function(obj) {
  dat <- as.data.frame(obj$data)
  keep <- !is.na(dat[[obj$columns$concentration]])
  exclude_col <- obj$columns$exclude
  if(!is.null(exclude_col) && exclude_col %in% names(dat)) {
    keep <- keep & (is.na(dat[[exclude_col]]) | dat[[exclude_col]] %in% "")
  }
  dat[keep, , drop = FALSE]
}
