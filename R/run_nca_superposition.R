#' Run an NCA on superpositioned data, either just extrapolated to steady state
#' for the current dose levels, or using linear extrapolation to a different 
#' dose and interval.
#' 
#' @inheritParams run_nca
#' 
#' @param regimen a list that specifies at least the `interval`, and 
#' potentially `dose`. E.g. `list(interval = 8)` to calculate steady state
#' concentration data using superposition. If a `dose` is also specified,
#' it will scale the superposition data linearly compared to the original 
#' input `dose`, e.g. `list(dose = 500, interval = 12)`.
#' @param parameters description...
#' 
#' @returns a PKNCA output object based on superposition data
#' 
#' @export
#' 
run_nca_superposition <- function(
  data,
  dose_data = NULL,
  regimen,
  dictionary,
  parameters = c("auclast", "cmax"),
  groups = c(),
  verbose = TRUE
) {
  
  if(is.null(dictionary$dose_level)) {
    cli::cli_abort("A dictionary entry `dose_level` and corresponding column in `data` is required")
  }
  
  ## If multiple dose levels in data:
  dose_levels <- unique(data[[dictionary$dose_level]])
  dose_levels <- dose_levels[!is.na(dose_levels)]
  
  ## Check if evid column in data
  evid <- names(data)[tolower(names(data)) == "evid"]
  if(! length(evid) > 0) {
    cli::cli_abort("Data requires an EVID column")
  }
  
  if(verbose) 
    cli::cli_alert_info("Creating steady state data using superposition")
  
  ## Make sure there is a datapoint at t=0 for each subject_id, if not already present
  has_zero_time <- data |>
    dplyr::filter(!!rlang::sym(dictionary$time) == 0 & !!rlang::sym(evid) == 0) |>
    dplyr::group_by_at(c(dictionary$subject_id, groups)) |>
    dplyr::pull(!!rlang::sym(dictionary$subject_id))
  zero_time <- data |>
    dplyr::filter(!!rlang::sym(evid) == 0) |> 
    dplyr::mutate(
      !!rlang::sym(dictionary$time) := 0,
      !!rlang::sym(dictionary$conc) := 0
    ) |> 
    dplyr::select_at(c(dictionary$subject_id, dictionary$conc, dictionary$time, dictionary$dose_level, groups)) |> 
    dplyr::distinct() %>%
    dplyr::filter(!(!!rlang::sym(dictionary$subject_id) %in% has_zero_time))
  
  steady_state <- data.frame()
  for (dose in dose_levels){
    if(verbose) 
      cli::cli_alert_info(paste0("- Dose level: ", dose))
    conc_raw <- data |>
      dplyr::filter(!!rlang::sym(evid) == 0) |> 
      dplyr::filter(!is.na(!!rlang::sym(dictionary$dose_level))) |>
      dplyr::filter(!!dictionary$time > 0) |> 
      dplyr::select_at(unique(c(dictionary$subject_id, dictionary$conc, dictionary$time, groups, dictionary$dose_level))) |> 
      dplyr::bind_rows(zero_time) |>
      dplyr::arrange_at(c(dictionary$subject_id, dictionary$time)) |> 
      dplyr::filter(!!rlang::sym(dictionary$dose_level) == dose)
    
    ## Create conc data object
    conc_formula <- as.formula(
      paste(dictionary$conc, "~", dictionary$time, "|", paste(ifelse(is.null(groups), "", paste(paste(groups, collapse="+"), "+")),  dictionary$subject_id))
    )
    conc_obj <- PKNCA::PKNCAconc(
      data = conc_raw %>% 
        dplyr::filter(!is.na(!!rlang::sym(dictionary$conc))),
      formula = conc_formula # e.g. CONC ~ NOMTIME | ID + TREATMENT_GROUP
    )
    
    ## Create superposition data object
    if(is.null(regimen$dose)) { # use same dose, just extrapolate to steady state
      tmp <- PKNCA::superposition(
        conc_obj,
        tau = regimen$interval,
        dose.input = dose
      ) |>
        dplyr::mutate(!!rlang::sym(dictionary$dose_level) := dose)
    } else { # use a different dose (PKNCA will just scale linearly)
      tmp <- PKNCA::superposition(
        conc_obj,
        tau = regimen$interval,
        dose.amount = regimen$dose,
        dose.input = dose,
        check.blq = FALSE
      ) |>
        dplyr::mutate(!!rlang::sym(dictionary$dose_level) := dose)
    }
    if("data_conc" %in% names(tmp)) { # error class cannot use bind_rows
      tmp <- tmp |>
        dplyr::mutate(error = as.character(.data$data_conc)) %>%
        dplyr::select(-"data_conc")
    }
    steady_state <- steady_state |> 
      dplyr::bind_rows(tmp)
  }

  if(verbose) 
    cli::cli_alert_info("Computing NCA parameters on superposition data")
  suppressWarnings({
    nca_steady_state_results <- run_nca(
      data = steady_state |>
        dplyr::mutate(
          !!rlang::sym(dictionary$time) := .data$time, 
          !!rlang::sym(dictionary$conc) := .data$conc,
          !!rlang::sym(dictionary$dose) := !!rlang::sym(dictionary$dose_level)
        ),
      groups = groups,
      dictionary = dictionary
    )
  })

  nca_steady_state_results
  
}
