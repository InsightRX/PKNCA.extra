#' Parse data into dataset for NCA
#' 
#' The actual time since last dose (`ATSLD`) is simulated from the nominal
#' time (`PCTPTNUM`) by adding log-normally distributed noise (7% CV). To make
#' the output reproducible, the noise is generated using a fixed random seed
#' (argument `seed`). The global random number generator state of the session is
#' not affected.
#' 
#' @param data list containing SDTM tables as data.frames
#' @param seed integer random seed used to simulate `ATSLD`. Default is
#' `12345`, which makes the output reproducible across calls. If `NULL`, the
#' current random number generator state of the session is used (and
#' advanced), so results will differ between calls unless `set.seed()` is
#' called beforehand.
#' 
#' @returns data.frame with data for NCA package
#' 
#' @export
parse_data_for_nca <- function(data, seed = 12345) {
  if (!is.null(seed) && !(is.numeric(seed) && length(seed) == 1 && !is.na(seed))) {
    cli::cli_abort("`seed` should be a single number, or `NULL`.")
  }
  
  ## Define metadata / settings for NCA
  ## These are things that cannot be read easily from EX dataset
  ## and are therefore hardcoded for now.

  route <- irxforge::get_route_from_data_column(data$ex$exroute)
  md <- list(
    PROFTYPE = "SD", # single dose
    DAY = 1,
    TIMEUNIT = "hours",
    AUCMETHD = "Linear Log",
    ADM = ifelse(route == "iv", "INTRAVENOUS", "EXTRAVASCULAR"),
    ADUR = ifelse(route == "iv", NA, 0), # TODO: handle for IV drugs
    NDUR = ifelse(route == "iv", NA, 0)  # TODO: handle for IV drugs
  )
  
  ## Some cohorts we don't want to analyze in NCA, e.g. because they're not
  ## actually in the study or did not receive the drug.
  remove_cohorts <- c("Screen Failure", "Placebo")

  merged_data <- data$pc %>%
    dplyr::mutate(
      sampleid = paste0(
        stringr::str_replace_all(.data$usubjid, "\\-", ""), "-", 
        stringr::str_replace_all(.data$pcdtc, "[-:]", "")
      )
    ) %>%
    dplyr::mutate(
      # actual time since last dose:
      atsld = round(.data$pctptnum * exp(rnorm_with_seed(dplyr::n(), 0, .07, seed = seed)), 2)
    ) %>%
    dplyr::rename_with(toupper) %>%
    merge(
      data$dm %>% # join with study-arm data
      dplyr::rename_with(toupper) %>%
      dplyr::select("USUBJID", "ACTARM"),
      by = "USUBJID"
    ) %>%
    merge(
      data$ex %>%  # join with dose administration data
      dplyr::rename_with(toupper) %>%
      dplyr::select("USUBJID", "EXDOSE", "EXDOSU", "EXROUTE", "VISIT", "VISITNUM"),
      by = c("USUBJID", "VISITNUM")
    ) %>%
    dplyr::filter(!.data$ACTARM %in% remove_cohorts)
  arms <- unique(merged_data$ACTARM)
  
  nca_data <- merged_data %>%
    dplyr::mutate(
      ANALYTE = .data$PCTEST,
      MATRIX = .data$PCSPEC,
      COMPOUND = .data$PCTEST,
      PROFILE = .data$USUBJID,
      GROUP = .data$ACTARM,
      GROUPN = match(.data$ACTARM, arms),
      GROUPU = .data$EXDOSU,
      ATIME = .data$PCTPTNUM,
      NTIME = .data$PCTPTNUM,
      ACONC = .data$PCSTRESC,
      CONCUNIT = .data$PCSTRESU,
      LLOQ = .data$PCLLOQ,
      DOSE = .data$EXDOSE,
      DOSEUNIT = .data$EXDOSU,
      PROFTYPE = md$PROFTYPE, # single dose
      DAY = md$DAY,
      TIMEUNIT = md$TIMEUNIT,
      AUCMETHD = md$AUCMETHD,
      ADM = md$ADM,
      ADUR = md$ADUR,
      NDUR = md$NDUR
    )

  nca_data
}

#' Draw from a normal distribution using a specific seed
#'
#' Draws random numbers with `stats::rnorm()` using the given `seed` and R's
#' default random number generators, and restores the global random number
#' generator state and kind afterwards, so that the session's random number
#' stream is not affected.
#'
#' @param n number of draws
#' @param mean mean of the normal distribution
#' @param sd standard deviation of the normal distribution
#' @param seed integer random seed. If `NULL`, draws from the current random
#' number generator state without restoring it.
#'
#' @returns numeric vector of length `n`
rnorm_with_seed <- function(n, mean = 0, sd = 1, seed = NULL) {
  if (is.null(seed)) {
    return(stats::rnorm(n, mean, sd))
  }
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  ## Use R's default generators, so results do not depend on session settings
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  stats::rnorm(n, mean, sd)
}
