#' Function to export output from PKNCA to CDISC- or other mappings
#' 
#' This function could be temporary in this package, PKNCA may support 
#' remapping in the future, at least to CDISC format.
#' 
#' @param data output object from `PKNCA::pk.nca()`
#' @param mapping a list mapping PKNCA parameters to custom parameter names,
#' e.g. `list(CMAX_custom = "CMAX", "auc_last = "AUCLAST")`. PKNCA parameter
#' names are list values, new parameter names are list names.
#' @param exclude_unmapped exclude unmapped variables?
#' @param keep parameters to keep when remapping and excluding unmapped 
#' parameters.
#' 
#' @returns a data.frame with column names matching the desired mapping
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' nca <- run_nca(...)
#' nca_cdisc <- remap_nca_output(nca, "cdisc")
#' }
#' 
remap_nca_output <- function(
  data, 
  mapping = NULL, 
  exclude_unmapped = TRUE,
  keep = c()
) {
  if(! inherits(mapping, "list")) {
    stop("Provided NCA parameter mapping not in expected format.")
  }
  if(! inherits(data, "data.frame")) {
    stop("Please provide a data.frame or tibble as `data` argument.")
  }
  ## Only remap columns that are actually in the data
  mapping_filtered <- mapping[as.character(unlist(mapping)) %in% names(data)]
  out <- data %>% 
    dplyr::rename(!!!mapping_filtered)
  if(exclude_unmapped) {
    out <- out %>%
      dplyr::select(all_of(keep), !! names(mapping_filtered))
  }
  
  ## remap names of computed NCA paramters
  par_names <- attr(out, "parameters")
  par_names[match(unlist(mapping_filtered), par_names)] <- names(mapping_filtered)
  par_names <- par_names[par_names %in% names(out)]
  attr(out, "parameters") <- par_names
  
  out
}
