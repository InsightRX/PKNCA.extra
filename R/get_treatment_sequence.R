#' Function to generate a sequence variable for e.g. a crossover trial design
#' 
#' @inheritParams run_nca
#' @param variable variable name to base the sequence detection on
#' 
#' @returns a data frame with the subject_id, sequence number, and full sequence
#' description.
#' 
#' @export
#'  
get_treatment_sequence <- function(
  data, 
  dictionary,
  variable = "treatment",
  groups = NULL
) {
  seq_dat <- data |> 
    dplyr::select_at(c(dictionary$subject_id, dictionary$time, variable, groups)) |>
    dplyr::group_by_at(c(dictionary$subject_id, variable, groups)) |>
    dplyr::slice_head() |>
    dplyr::ungroup() |>
    dplyr::arrange_at(c(dictionary$subject_id, dictionary$time)) |>
    dplyr::group_by(!!rlang::sym(dictionary$subject_id)) |>
    dplyr::summarise(treatment_sequence = stringr::str_c(!!rlang::sym(variable), collapse = " -> ")) %>%
    dplyr::mutate(
      sequence = dplyr::dense_rank(
        treatment_sequence
      )
    ) %>%
    dplyr::mutate(character_sequence = LETTERS[sequence]) %>%
    dplyr::right_join(data, by = dictionary$subject_id) |>
    dplyr::select_at(c(dictionary$subject_id, "sequence", "treatment_sequence")) |>
    dplyr::distinct()
  seq_dat
}