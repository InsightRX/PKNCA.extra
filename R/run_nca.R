#' Run a non-compartmental analysis
#' 
#' This function runs an NCA on a pre-parsed dataset. The NCA will be performed
#' using the PKNCA package. The NCA output object will be saved to an
#' rds file if `path` is specified.
#' 
#' There are four possible types of input datasets that can be used to run the
#' NCA:
#' 
#' 1. Using a separate data.frame for PK data (`data`) and dose data
#' (`dose_data`), with each PK or dosing event a row in the respective datasedt. 
#' Can be used for both single and multi-dose data.
#' 
#' 2. Using a single data.frame for PK and dose data (`data`), with event type 
#' separated using an evid column (specified in `dictionary`). PK data rows 
#' should be set to `evid=0`, and doses to `evid=1`. This is essentially a 
#' NONMEM-style dataset. Rows with other `evid` values will be ignored.
#' Can be used for both single and multi-dose data, the dosing occasions will
#' be determined from the dosing events. If an `evid` column is present in 
#' `data`, it will not attempt to use approach 3.
#' 
#' 3. Using a single data.frame (`data`) with PK data as rows, and a column for 
#' dose (with colum name specified in `dictionary`). A `dose_data` data.frame 
#' will be constructed from the dose column. Can only be used for single-dose 
#' data, unless `groups` are used to separate the dosing occasions. 
#' 
#' 4. Using a single data.frame (`data`) with PK data as rows, but no dosing 
#' info in the data. NCA can still compute AUC and Cmax etc, but parameters that
#' require the dose such as CL, V, etc will be returned as NA. Can only be used
#' for single dose, unless `groups` are used to separate the dosing occasions.
#' 
#' @param data dataset in pre-parse format, preferrably created using
#' `create_nca_data()`. Can also be a NONMEM-style dataset with EVID=0|1 column
#' indicating PK an dosing data.
#' @param dose_data optional, a data.frame with all dosing info. Useful for 
#' multi-dose NCAs. Alternative to specifying a NONMEM-style datasett to `data`.
#' @param path path to file to store output object from PKNCA as RDS (optional)
#' @param groups optional grouping variable, e.g. `ACTARM`.
#' @param intervals specify time intervals for NCA calculations. The passed argument
#' should be a data.frame, with at least the `start` and `end` time for each
#' row that specifies the interval. `Inf` is allowed to specify the end of the
#' interval. Example:
#' ```
#' intervals = bind_rows(
#'    c(start = 0, end = 24), 
#'    c(start = 120, end = Inf)
#' )
#' ```
#' The `run_nca()` function will compute all parameters that are specified in
#' `settings()` or in the default settings.
#' @param partial_auc a data.frame containing at least the `start` and `end`
#' time for each row that specifies the start- and endtime for the partial AUC
#' calculation. At least the AUC_0-t (`auclast`) will be calculated for each 
#' row. Any other parameters specified in the data.frame as column will also be 
#' calculated, e.g. `cmax=TRUE`. Currently, only a single row can be supplied
#' as argument.
#' @param auc_tau compute AUCtau whenever relevant? TRUE (default) / FALSE. 
#' For multi-dose PK data, this will add the parameter `auc_tau` to the NCA 
#' output data as the AUC for each interval. In essence, it will just make a 
#' copy of the AUClast for the interval. See exact logic in function 
#' help page.
#' @param include_cols Optional. Character vector of column names in `data` that
#' should be merged in with NCA results (including only a single value for each
#' row). Merge will happen by subject ID, any supplied groupings, and 
#' `nca_start` time.
#' @param specification use specific options for the NCA and for mapping of
#' input / output data.
#' @param blq optional, string to match in concentration data, e.g. "<" or 
#' "BLQ>. If `conc` column in the data is `character` class, then if `blq` is
#' not NULL, it will detect any values that match this string and set those 
#' values to zero. If NULL, or `conc` data is numeric, the data will be used 
#' as-is.
#' @param dictionary abridged data dictionary for the input dataset, specified
#' as list object. Requires at least entries for `subject_id`, `time`, `conc`, 
#' `dose`. Optional entries are: `visit`.
#' If required entries are specified only partially, then default values 
#' (i.e. CDISC) nomenclature will be imputed for missing identifiers.
#' @param settings list of settings for NCA. Provided setting names can either 
#' be settings recognized directly by `pknca`, or settings that are included in 
#' the map provided by `nca_settings_map()` (which are then mapped to `pknca` 
#' setting names).
#' @param sequence_from optional. Argument should specify the column name 
#' that indicates the treatment e.g. for food-effect cross-over trials. It
#' will then add sequence group and sequence description columns to the 
#' output data.
#' @param post named list of lists, indicating which hard-coded
#' post-processing steps to include, and options for each step. 
#' Possible post-processing steps include:
#' 
#' - `accumulation`: if NCA calculated for multiple non-overlapping intervals,
#' will add accumulation ratios for the parameters listed in the `parameters` 
#' argument to the NCA output table for each interval.
#' 
#' Other options may be added in future. Default is:
#' `list("accumulation" = list("parameters" = c("auclast", "cmax")))`.
#' 
#' @param exclude_points list of points to exclude, based on name(s) in the 
#' list that should match column name in the input dataset. 
#' E.g. `list("sample_id" = c("12345", "23456"))`. This could similarly be used
#' to exclude entire subjects, e.g. `list("subject_id = c("103, "105")`
#' @param no_dots if `TRUE` (default) will replace any dots in parameter names
#' (e.g. `aucinf.obs`) with underscores (`aucinf_obs`).
#' @param verbose verbose output?
#' @param ... parameters passed to `PKNCA::pk.nca()`` function.
#' 
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' run_nca(
#'   data = data,
#'   path = "./outputs/nca.rds"
#' )
#' }
run_nca <- function(
  data,
  dose_data = NULL,
  specification = "default",
  dictionary = NULL,
  settings = NULL,
  blq = "<",
  groups = NULL,
  include_cols = NULL,
  intervals = data.frame(start = 0, end = Inf),
  partial_auc = NULL,
  exclude_points = NULL,
  exclude_subjects = NULL,
  sequence_from = NULL,
  add_auctau = TRUE,
  post = list(
    "accumulation" = list(
      "parameters" = c(
        "auctau",
        "cmax"
      )
    )
  ),
  no_dots = TRUE,
  path = NULL,
  verbose = FALSE,
  ...
) {

  time_start <- Sys.time() 
  
  ## Read default NCA specs (parameters, and parameter mapping)
  spec <- read_spec_from_file(specification)

  ## If user provided any settings (PKNCA options), override any default 
  ## options with the user-provided ones
  pknca_options <- settings
  if(is.null(pknca_options)) {
    md <- jsonlite::read_json(system.file(package = "PKNCA.extra", file.path("nca", "pknca.json")))
    pknca_options <- md$defaults
  }
  pknca_options <- map_nca_settings(
    settings = pknca_options,
    mapping = "pknca"
  )

  ## Apply options
  pknca_options <- apply_pknca_options(
    pknca_options,
    nca_parameters = spec$nca$pknca.parameters,
    verbose = FALSE
  )

  ## Set up data dictionary. If any missing values, use CDISC names
  default_dictionary <- list( 
    subject_id = "USUBJID",
    conc = "PCORRES",
    time = "PCTPTNUM",
    dose = "EXDOSE"
  )
  if(is.null(dictionary)) {
    dictionary <- default_dictionary
  }
  missing_dict <- setdiff(names(default_dictionary), names(dictionary))
  if(length(missing_dict) > 0) {
    cli::cli_alert_info(paste0("Assuming default data dictionary names for: ", paste0(missing_dict, collapse = ", ")))
    dictionary[missing_dict] <- default_dictionary[missing_dict]
  }

  ## Define concentration / dose / time columns for PKNCA
  conc_formula <- as.formula(
    paste(dictionary$conc, "~", dictionary$time, "|", paste(ifelse(is.null(groups), "", paste(paste(groups, collapse="+"), "+")),  dictionary$subject_id))
  )
  dose_formula <- as.formula(
    paste(dictionary$dose, "~", dictionary$time, "|", paste(ifelse(is.null(groups), "", paste(paste(groups, collapse="+"), "+")),  dictionary$subject_id))
  )
  
  if(verbose) cli::cli_alert_info(paste0("Gathering input data for NCA"))
  
  ## Prepare `conc` column: set any datapoints matching BLQ string to 0
  if(inherits(data[[dictionary$conc]], "character") & !is.null(blq)) {
    suppressWarnings( # unhandled character conversion throws warnings that we don't need
      data <- data %>%
        dplyr::mutate(!!dictionary$conc := ifelse(stringr::str_detect(.data[[dictionary$conc]], blq), 0, as.numeric(.data[[dictionary$conc]])))
    )
  }

  ## Infer data input approach (see man page for description of approaches)
  ##
  evid <- names(data)[tolower(names(data)) == "evid"]
  if(!is.null(dose_data)) { # approach 1: separate datasets
    nca_approach <- 1
    pk_data <- data
  } else { # approach 2, 3, 4
    if(length(evid) > 0) { # approach 2: NONMEM-style dataset
      nca_approach <- 2
      pk_data <- data %>%
        dplyr::filter(!!rlang::sym(evid) == 0)
      dose_data <- data %>%
        dplyr::filter(!!rlang::sym(evid) == 1)
    } else {
      if(dictionary$dose %in% names(data)) { # approach 3: take dosing info from column
        nca_approach <- 3
        pk_data <- data
        dose_data <- data %>% # infer doses from PK data alone
          dplyr::group_by_at(unique(c(dictionary$subject_id, groups))) %>%
          dplyr::slice(1) %>%
          dplyr::mutate(
            !!dictionary$time := 0
          )
      } else { # approach 4, only PK data
        nca_approach <- 4
        pk_data <- data
      }
    }
  }
  if(verbose) {
    approaches <- list(
      "Separate data.frames for PK and dosing",
      "data.frame with EVID column (NONMEM-style)",
      "data.frame with PK data and dose column",
      "data.frame with PK data, no dose data"
    )
    cli::cli_alert_info("NCA input data type: {approaches[[nca_approach]]}")
  }

  ## Prepare id and time columns, and parse into an NCA-ready dataset
  ## Catching errors here, because we often have grouping errors that 
  ## we want to communicate more clearly to the user.
  tryCatch({
    conc_obj <- PKNCA::PKNCAconc(
      data = pk_data %>%
        dplyr::mutate(!!dictionary$time := dplyr::if_else(.data[[dictionary$time]] < 0, 0, .data[[dictionary$time]])),
      formula = conc_formula
    )
  }, error = function(e) {
    msg <- ""
    if(grep("not unique per group", e$message)) {
      msg <- "It seems like concentration times are not unique for each NCA. Make sure you use the `groups` argument if concentration data were not collected following multiple dosing events."
    }
    cli::cli_abort(
      paste(
        "An error occurred parsing the concentration data.",
        msg,
        "\n\nFull error from PKNCA:\n",
        e$message,
        sep = "\n"
      )
    )
  })

  ## Get dose data
  if(!is.null(dose_data)) {
    dose_obj <- PKNCA::PKNCAdose(
      data = dose_data,
      formula = dose_formula
    )
  } else {
    dose_obj <- NULL
  }

  ## Handle exclusions
  if(!is.null(exclude_points)) {
    if(verbose) cli::cli_alert_info("Applying exclusion of data points")
    exclusions <- parse_exclusions(conc_obj$data, exclude_points)
    ## Use PKNCA::exclude to keep track of excluded points
    if(any(exclusions$mask)) {
      conc_obj <- PKNCA::exclude(
        conc_obj,
        reason = exclusions$reason,
        mask = exclusions$mask
      )
    } else {
      cli::cli_alert_warning("Requested datapoints to be excluded not found in data")
    }
  }
  if(!is.null(exclude_subjects)) {
    if(verbose) cli::cli_alert_info("Applying exclusion of subjects / groups")
    exclusions_conc <- parse_exclusions(conc_obj$data, exclude_subjects)
    exclusions_dose <- parse_exclusions(dose_obj$data, exclude_subjects)
    ## can't use PKNCA::exclude() function for exclusion of entire groups/subjects, 
    ## need to hard-remove from the data:
    conc_obj$data <- conc_obj$data[!exclusions_conc$mask, ]
    dose_obj$data <- dose_obj$data[!exclusions_dose$mask, ]
  }

  ## Set computation options for intervals, if `intervals` is specified
  ## Create new intervals object from template, if not specified.
  if(!is.null(intervals)) {
    if(!inherits(intervals, "data.frame") || !all(c("start", "end") %in% names(intervals))) {
      stop("Argument `intervals` not specified in correct format (`data.frame` with entries `start` and `end`)")
    }
    ## switch on/off parameter calculation based on current options for single
    cols_specified <- names(intervals)
    auc_options_template <- pknca_options$single.dose.aucs %>%
      dplyr::select(-!!cols_specified)
    intervals <- intervals %>%
      dplyr::mutate(auc_options_template)
  } else {
    intervals <- PKNCA::PKNCA.options()$single.dose.aucs
  }

  ## Add the intervals info for partial AUCs, where we're only going to compute
  ## the AUC for the given time periods
  if(!is.null(partial_auc)) {
    if(!inherits(partial_auc, "data.frame") || !all(c("start", "end") %in% names(partial_auc))) {
      stop("Argument `partial_auc` not specified in correct format (`data.frame` with entries `start` and `end`)")
    }
    if(nrow(partial_auc) > 1) {
      cli::cli_alert_warning("Currently, only a single time-vector can be requested as partial AUC. Only first row of `partial_auc` argument will be used.")
      partial_auc <- partial_auc[1,]
    }
    chk_duplicate <- paste(partial_auc$start, partial_auc$end) == paste(intervals$start, intervals$end)
    if(any(chk_duplicate)) {
      cli::cli_alert_warning(paste0("Partial AUC interval [", paste(partial_auc$start, partial_auc$end), "] already included in `intervals` argument and will be ignored"))
      partial_auc <- NULL
    } else {
      ## switch on `auclast` calculation. Every other parameter can stay off.
      partial_auc <- partial_auc %>%
        dplyr::mutate(auclast = TRUE)
      intervals <- dplyr::bind_rows(intervals, partial_auc) %>%
        dplyr::mutate(across(all_of(names(.)), function(x) ifelse(is.na(x), FALSE, x)))
    }
  }
  
  ## Check that start of interval always has sample at that time.
  ## If not, add (unless `auto_start_sample = FALSE`).
  
  ## Set imputation default.
  ## We need to specify an impute, otherwise the NCA will throw an error
  ## if the time doesn't match up exactly witht the interval
  ## TODO: we may want to add some logic for this in the future
  if(is.null(intervals$impute)) {
    ## If `impute` not specified, set to use conc=0
    intervals$impute <- rep("start_conc0", length(intervals$start))
  }
  
  ## Combine to input object for PKNCA
  if(!is.null(dose_obj)) {
    comb_obj <- PKNCA::PKNCAdata(
      data.conc = conc_obj,
      data.dose = dose_obj,
      impute = "start_conc0,start_predose",
      options = list(
        single.dose.aucs = intervals
      )
    )
  } else {
    comb_obj <- PKNCA::PKNCAdata(
      data.conc = conc_obj,
      intervals = intervals,
      impute = "start_conc0,start_predose",
      options = list(
        single.dose.aucs = intervals
      )
    )
  }
  
  ## Perform NCA
  if(verbose) cli::cli_alert_info("Performing NCA using PKNCA package")
  res <- PKNCA::pk.nca(
    comb_obj,
    verbose = verbose,
    ...
  )
  
  if(no_dots) {
    res$result$PPTESTCD <- gsub("\\.", "_", res$result$PPTESTCD)
  }
  
  ## Combine results and parse into output structure
  if(verbose) cli::cli_alert_info("Parsing output from PKNCA")
  res_inf <- res$result %>%
    dplyr::select(-exclude) %>%
    tidyr::pivot_wider(
      names_from = PPTESTCD, 
      values_from = PPORRES
    ) %>%
    dplyr::mutate(
      is_partial = is.na(tmax) & # all columns with start/end matching partial_auc, and where other NCA params are NA
        (start %in% partial_auc$start & end %in% partial_auc$end)
    )
  res_partial <- res_inf %>%
    dplyr::filter(is_partial)

  ## Keep columns in data that are often used for grouping.
  ## User can also specify `groups` as argument, this is just for convenience.
  common_group_names <- c("ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE")
  common_group_names <- common_group_names[common_group_names %in% names(data)]
  
  cols_groups <- unique(c(common_group_names, groups))
  cols_id_group <- c(dictionary$subject_id, cols_groups)
  cols_pknca <- intersect(
    get_pk_parameters_from_pknca_options(
      pknca_options$single.dose.aucs, 
      no_dots = no_dots
    ),
    unique(res$result$PPTESTCD)
  )
  cols_out <- c(cols_id_group, "start", "end", cols_pknca)
  joiner <- c(dictionary$subject_id, setNames(groups, groups))
  nca_output <- res_inf %>%
    dplyr::filter(!is_partial) %>%
    dplyr::left_join(
      data %>%
        dplyr::group_by_at(c(dictionary$subject_id, cols_groups)) %>%
        dplyr::slice(1) %>%
        dplyr::select(!!dictionary$subject_id, !!cols_groups),
      by = joiner
    ) %>%
      dplyr::select(!!cols_out) %>%
      data.frame()
  
  ## Add interval identifier, for easier stratifying by interval (e.g. for plotting)
  intv <- intervals %>%
    dplyr::select(start, end) %>%
    dplyr::mutate(interval = paste(start, end, sep = " - "))
  nca_output <- nca_output %>%
    dplyr::left_join(intv, by = c("start", "end"))
  
  ## Bind partial AUC data together with overall NCA data
  name_partial_auc <- c()
  if(nrow(res_partial) > 0) {
    name_partial_auc <- paste0("auc_partial_", partial_auc$start, "_", partial_auc$end)
    res_partial[[name_partial_auc]] <- res_partial$auclast
    res_partial <- res_partial %>%
      dplyr::select(!!joiner, !!name_partial_auc) %>%
      dplyr::left_join(
        data %>%
          dplyr::group_by_at(c(dictionary$subject_id, cols_groups)) %>%
          dplyr::slice(1) %>%
          dplyr::select(!!dictionary$subject_id, !!cols_groups),
        by = joiner
      ) %>%
      data.frame()
    nca_output <- dplyr::full_join(
      nca_output, 
      res_partial,
      by = intersect(names(nca_output), names(res_partial))
    )
  }

  ## Remap NCA parameter names, if requested, and remove undesired parameters
  is_mapping_specified <- isTRUE(!is.null(spec$nca$pknca.parameter_mapping) && length(spec$nca$pknca.parameter_mapping) > 0)
  if(is_mapping_specified) {
    comb_map <- spec$nca$pknca.parameter_mapping
    if(verbose) cli::cli_alert_info(paste0("Remapping NCA parameter names to '", specification, "' output specifations"))
  } else {
    comb_map <- list()
  }
  
  ## Attach names of computed NCA paramters
  attr(nca_output, "parameters") <- unique(c(cols_pknca, name_partial_auc))
  
  # if mapping specified, then remap only the mapped variables, and return everything else normally
  nca_output <- remap_nca_output(
    nca_output,
    mapping = comb_map,
    exclude_unmapped = FALSE,
    keep = c(dictionary$subject_id, cols_groups, "start", "end", "interval", name_partial_auc)
  ) %>%
    dplyr::rename(
      nca_start = start,
      nca_end = end
    ) |>
    dplyr::mutate(nca_interval = paste0(nca_start, " - ", nca_end)) |>
    dplyr::select(-interval)

  ## Add any input data columns?
  if(!is.null(include_cols) && length(include_cols) > 0) {
    if(verbose) 
      cli::cli_alert_info(paste0("Adding columns from input data to NCA output: ", paste0(include_cols, collapse = ", ")))
    join_data <- pk_data %>%
      dplyr::select_at(c(dictionary$subject_id, dictionary$time, cols_groups, include_cols))
    nca_output <- nca_output %>%
      dplyr::left_join(join_data, by = c(dictionary$subject_id, cols_groups)) %>%
      dplyr::group_by_at(c(dictionary$subject_id, "nca_interval", cols_groups)) %>%
      dplyr::filter(!!rlang::sym(dictionary$time) > nca_start & # pick values from first obs > nca_start
                      !!rlang::sym(dictionary$time) <= nca_end) %>% 
      dplyr::slice(1) %>%
      dplyr::select(- !!dictionary$time) %>%
      dplyr::ungroup()
  }
  
  ## Add sequence
  if(!is.null(sequence_from)) {
    if(length(sequence_from) != 1 || !inherits(sequence_from, "character")) {
      cli::clia_abort("`sequence_from` argument should be a character value of length 1.")
    } else {
      cli::cli_alert_info("Adding columns for treatment sequence")
    }
    seq_dat <- get_treatment_sequence(
      dat = data,
      dictionary = dictionary,
      variable = sequence_from,
      group = cols_groups
    )
    nca_output <- nca_output |>
      dplyr::left_join(seq_dat, by = dictionary$subject_id)
  }
  
  ## Add AUCtau (only for multi-dose data)?
  if(add_auctau) {
    nca_output <- nca_add_auctau(
      output = nca_output,
      dictionary = dictionary,
      verbose = verbose
    )    
  }
  
  ## Post-processing steps
  if(!is.null(post) & length(names(post)) > 0) {
    allowed_post_steps <- c(
      "accumulation", 
      "bioavailability"
    )
    for(key in names(post)) {
      if(key %in% allowed_post_steps) {
        nca_output <- do.call(
          paste0("nca_post_", key), args = list(
            data = nca_output,
            dictionary = dictionary,
            options = post[[key]],
            groups = cols_groups,
            verbose = verbose
          )
        )
      } else {
        cli::cli_alert_warning(paste0("Requested post-processing step ", key, " not recognized, ignoring."))
      }
    }
  }
  
  ## Arrange output
  nca_output <- nca_output %>%
    dplyr::arrange_at(c(dictionary$subject_id, cols_groups, "nca_start"))
  
  ## save PKNCA object to file?
  if(!is.null(path)) {
    saveRDS(res, path)
  }
  
  ## Attach PKNCA output
  attr(nca_output, "PKNCA_object") <- res
  
  ## Attach excluded subjects/groups, if any
  if(!is.null(exclude_subjects)) {
    attr(nca_output, "excluded_subjects") <- exclude_subjects
  }
  ## Attach parameters dictionary
  attr(nca_output, "dictionary") <- dictionary
  
  time_end <- Sys.time()
  time_all <- round(as.numeric(time_end - time_start), 1)
  if (verbose) cli::cli_alert_success(paste0("Done (", time_all, "s)"))
  
  nca_output
}

#' When PKNCA::PKNCA.options() is invoked without arguments, it will
#' return a list of its configuration. Among the options is the data.frame
#' `single.dose.aucs` which will list the exported PK parameters for both 
#' the AUC0-24 and AUCinf computations. Wherever the parameters are set to TRUE
#' PKNCA will compute those and include in output. So in that case we can use 
#' those parameters as well and should make them available in the output from 
#' `run_nca()`.
#' 
#' @param options parameters for interval specification for NCA
#' @param no_dots if `TRUE` (default) will replace any dots in parameter names
#' (e.g. `aucinf.obs`) with underscores (`aucinf_obs`).
#' 
get_pk_parameters_from_pknca_options <- function(options = NULL, 
                                                 no_dots = FALSE) {
  if(is.null(options)) {
    options <- PKNCA::PKNCA.options()$single.dose.aucs
  }
  pk_par_names <- names(options)
  cols <- c()
  for(i in 1:length(options$start)) {
    suppressWarnings( # some may not be T/F type parameters, we don't need warning 
      tmp <- pk_par_names[
        as.numeric(options[i, ]) == 1
      ]
    )
    tmp <- tmp[!is.na(tmp)]
    cols <- c(cols, tmp)
  }
  if(no_dots) {
    cols <- gsub("\\.", "_", cols)
  }
  cols
}

#' Creates a data.frame specifying start and end time, and stores it as option
#' `single.dose.auc` in PKNCA.options()
#' 
#' @inheritParams run_nca
#' @param parameters list of parameters to calculate, indicated by TRUE and 
#' FALSE, e.g. `list(auc.inf.pred=TRUE, cmax.pred=FALSE, ...)`
#' 
set_pknca_interval_parameters <- function(time, parameters) {
  if (!isTRUE(length(time) == 2)) {
    stop("`time` argument can only be of length 2 (start and end of NCA).")
  }
  intv_table <- data.frame(
    start = time[1],
    end = time[2],
    parameters
  )
  PKNCA::PKNCA.options(single.dose.aucs = intv_table)
}