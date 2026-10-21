#' Imputation methods for the concentration at the start of an NCA interval
#'
#' Custom PKNCA imputation methods used by `run_nca()`. Unlike PKNCA's
#' built-in `start_conc0`, these methods never overwrite a concentration that
#' was observed at the start of the interval.
#'
#' - `start_obs_predose_conc0`: keep the observed concentration at the
#'   interval start if present. Otherwise use the pre-dose concentration
#'   (PKNCA's `start_predose` logic, i.e. the last sample before the interval
#'   start within 5% of the interval length). If there is no such sample,
#'   impute a concentration of 0. Used for extravascular doses and infusions.
#' - `start_obs_c0_logslope`: for IV bolus doses. Keep the observed
#'   (non-zero) concentration at the interval start if present. Otherwise
#'   back-extrapolate C0 using a log-linear regression of the first two
#'   concentrations after the dose (`PKNCA::pk.calc.c0()`, methods
#'   `"c0"`, `"logslope"`, `"c1"`).
#'
#' The function names follow the PKNCA convention
#' (`PKNCA_impute_method_<name>`) so they can be referred to as `<name>` in
#' the `impute` argument of `PKNCA::PKNCAdata()`.
#'
#' @param conc,time concentration and time in the interval
#' @param start,end start and end time of the interval
#' @param conc.group,time.group all concentrations and times for the group
#' @param ... ignored
#' @param options PKNCA options
#'
#' @returns data.frame with columns `conc` and `time`
#' @name nca_impute_methods
#' @export
PKNCA_impute_method_start_obs_predose_conc0 <- function(
  conc,
  time,
  start = 0,
  end = Inf,
  conc.group = NULL,
  time.group = NULL,
  ...,
  options = list()
) {
  ret <- drop_missing_start_conc(conc, time, start)
  if(any(ret$time %in% start)) {
    return(ret)
  }
  ## Use the last pre-dose sample if it is within 5% of the interval length
  ## before the interval start. Implemented here rather than using PKNCA's
  ## `start_predose`, which compares absolute times against the window and
  ## therefore accepts nearly any earlier sample when `start > 0`.
  if(!is.null(conc.group) && !is.null(time.group)) {
    shift_end <- if(is.infinite(end)) max(c(ret$time, start)) else end
    max_shift <- 0.05 * (shift_end - start)
    mask_predose <- time.group < start & !is.na(conc.group)
    if(any(mask_predose)) {
      time_predose <- max(time.group[mask_predose])
      if((start - time_predose) <= max_shift) {
        conc_predose <- conc.group[mask_predose & time.group == time_predose][1]
        ret <- rbind(data.frame(conc = conc_predose, time = start), ret)
      }
    }
  }
  if(!any(ret$time %in% start)) {
    ret <- PKNCA::PKNCA_impute_method_start_conc0(
      conc = ret$conc,
      time = ret$time,
      start = start
    )
  }
  ret
}

#' @rdname nca_impute_methods
#' @export
PKNCA_impute_method_start_obs_c0_logslope <- function(
  conc,
  time,
  start = 0,
  end = Inf,
  ...,
  options = list()
) {
  ret <- data.frame(conc = conc, time = time)
  c0 <- suppressWarnings(
    PKNCA::pk.calc.c0(
      conc = conc,
      time = time,
      time.dose = start,
      method = c("c0", "logslope", "c1"),
      check = FALSE
    )
  )
  if(is.na(c0)) {
    ## nothing to extrapolate from, fall back to observed / pre-dose / 0
    return(PKNCA_impute_method_start_obs_predose_conc0(conc, time, start, end, ...))
  }
  mask_start <- ret$time %in% start
  if(any(mask_start)) {
    ret$conc[mask_start] <- c0
  } else {
    ret <- rbind(ret, data.frame(conc = c0, time = start))
    ret <- ret[order(ret$time), ]
  }
  ret
}

#' @rdname nca_impute_methods
#' @details
#' - `start_c0_logslope`: for IV bolus doses where a concentration observed at
#'   the time of the dose is the pre-dose concentration (e.g. the trough of
#'   the previous dosing interval). Always back-extrapolate C0 from the
#'   concentrations after the dose (`PKNCA::pk.calc.c0()`, methods
#'   `"logslope"`, `"c1"`), replacing the concentration at the interval start.
#'   Used by `run_nca()` when computing AUCtau and partial AUC.
#' @export
PKNCA_impute_method_start_c0_logslope <- function(
  conc,
  time,
  start = 0,
  end = Inf,
  ...,
  options = list()
) {
  ret <- data.frame(conc = conc, time = time)
  ret <- ret[!(ret$time %in% start), , drop = FALSE]
  c0 <- suppressWarnings(
    PKNCA::pk.calc.c0(
      conc = ret$conc,
      time = ret$time,
      time.dose = start,
      method = c("logslope", "c1"),
      check = FALSE
    )
  )
  if(is.na(c0)) {
    return(PKNCA_impute_method_start_obs_predose_conc0(conc, time, start, end, ...))
  }
  ret <- rbind(ret, data.frame(conc = c0, time = start))
  ret[order(ret$time), , drop = FALSE]
}

#' Drop rows at the interval start that have a missing concentration, so they
#' are treated as not observed.
#' @noRd
drop_missing_start_conc <- function(conc, time, start) {
  ret <- data.frame(conc = conc, time = time)
  ret[!(ret$time %in% start & is.na(ret$conc)), , drop = FALSE]
}

#' Normalize a route column to the values accepted by PKNCA
#' ("intravascular" / "extravascular").
#' @noRd
normalize_nca_route <- function(x) {
  iv <- tolower(trimws(as.character(x))) %in% c(
    "iv", "intravenous", "intravascular", "intra-arterial", "intraarterial",
    "ia", "iv bolus", "bolus", "iv infusion", "infusion"
  )
  ifelse(iv, "intravascular", "extravascular")
}

#' Assign start-of-interval imputation methods to each NCA interval
#'
#' Uses `start_obs_c0_logslope` for intervals starting at an IV bolus dose
#' (route `"intravascular"` and duration 0), and `start_obs_predose_conc0`
#' otherwise.
#'
#' @param comb_obj `PKNCAdata` object
#' @returns `PKNCAdata` object with `impute` set
#' @noRd
assign_nca_impute <- function(comb_obj) {
  default_method <- "start_obs_predose_conc0"
  bolus_method <- "start_obs_c0_logslope"
  dose_obj <- comb_obj$dose
  bolus <- NULL
  if(inherits(dose_obj, "PKNCAdose")) {
    dose_df <- as.data.frame(dose_obj$data)
    route <- dose_df[[dose_obj$columns$route]]
    duration <- dose_df[[dose_obj$columns$duration]]
    if(is.null(duration)) duration <- rep(0, nrow(dose_df))
    is_bolus <- !is.na(route) & route == "intravascular" &
      (is.na(duration) | duration == 0)
    if(any(is_bolus)) {
      dose_groups <- dose_obj$columns$groups$group_vars
      bolus <- dose_df[is_bolus, c(dose_groups, dose_obj$columns$time), drop = FALSE]
      names(bolus)[ncol(bolus)] <- "start"
      bolus <- unique(bolus)
      bolus$is_bolus <- TRUE
    }
  }
  if(is.null(bolus)) {
    comb_obj$impute <- default_method
    return(comb_obj)
  }

  ## Make intervals group-specific, so the method can differ per subject
  intervals <- as.data.frame(comb_obj$intervals)
  conc_groups <- comb_obj$conc$columns$groups$group_vars
  missing_groups <- setdiff(conc_groups, names(intervals))
  if(length(missing_groups) > 0) {
    groups_df <- unique(as.data.frame(comb_obj$conc$data)[, conc_groups, drop = FALSE])
    intervals <- merge(
      intervals,
      groups_df,
      by = intersect(conc_groups, names(intervals))
    )
  }
  intervals$.row_order <- seq_len(nrow(intervals))
  intervals <- merge(
    intervals,
    bolus,
    by = intersect(names(bolus), names(intervals)),
    all.x = TRUE
  )
  intervals <- intervals[order(intervals$.row_order), , drop = FALSE]
  intervals$impute <- ifelse(
    !is.na(intervals$is_bolus) & intervals$is_bolus,
    bolus_method,
    default_method
  )
  intervals$.row_order <- NULL
  intervals$is_bolus <- NULL
  rownames(intervals) <- NULL
  comb_obj$intervals <- intervals
  comb_obj$impute <- "impute"
  comb_obj
}

#' Make the custom imputation methods visible to PKNCA
#'
#' PKNCA looks up imputation functions by name from its own namespace (and
#' from there the search path), so they are only found when PKNCA.extra is
#' attached. If it is not, temporarily attach an environment holding them.
#' Returns a function that undoes the attach.
#' @noRd
expose_nca_impute_methods <- function() {
  env_name <- "PKNCA.extra:impute_methods"
  if("package:PKNCA.extra" %in% search() || env_name %in% search()) {
    return(function() invisible(NULL))
  }
  env <- new.env(parent = emptyenv())
  env$PKNCA_impute_method_start_obs_predose_conc0 <- PKNCA_impute_method_start_obs_predose_conc0
  env$PKNCA_impute_method_start_obs_c0_logslope <- PKNCA_impute_method_start_obs_c0_logslope
  env$PKNCA_impute_method_start_c0_logslope <- PKNCA_impute_method_start_c0_logslope
  attach(env, name = env_name, warn.conflicts = FALSE)
  function() {
    if(env_name %in% search()) detach(env_name, character.only = TRUE)
  }
}
