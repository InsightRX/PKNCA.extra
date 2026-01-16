#' Parse data into dataset for NCA
#' 
#' @returns data.frame with data for NCA package
#' 
#' @param data list containing SDTM tables as data.frames
#' 
#' @export
#' 
parse_data_for_nca <- function(data) {
  
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
      atsld = round(.data$pctptnum * exp(stats::rnorm(dplyr::n(), 0, .07)), 2)
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
