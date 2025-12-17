#' Helper function to get a mask based on a list of vectors with identifiers.
#' This function is used to gather exclusions in the `run_nca()` function.
#' 
#' @param data data.frame
#' @param exclusions a named list of lists, with the names corresponding
#' to columns in the dataset. Each sub-list should contain vectors `id` and 
#' `reason`. E.g. 
#' 
#' `list(
#'   SAMPLEID = list(
#'     id = c("017011111-20120908T120000", "017011028-20130719T010000",
#'            "017011033-20140318T040000"), 
#'    reason = c("unexplained anomaly", "manual lambda-z check", "assay error")
#'   )
#' )`
#' 
#' @returns a list with: the mask (vector of T/F) that indicates the rows in the data to
#' exclude, and a vector of reasons for exclusions (NA if not excluded)
#' 
parse_exclusions <- function(data, exclusions) {
  excl_cols <- names(exclusions)
  mask_all <- rep(FALSE, length(data[,1]))
  reason <- rep(NA, length(data[,1]))
  for(col in excl_cols) {
    mask_tmp <- data[[col]] %in% exclusions[[col]]$id
    reason[mask_tmp] <- exclusions[[col]]$reason
    mask_all <- mask_tmp | mask_all
  }
  list(
    mask = mask_all,
    reason = reason
  )
}
