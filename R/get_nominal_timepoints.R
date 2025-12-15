#' Get nominal timepoints based on vector of timepoints
#' 
#' @param t vector of timepoints
#' Get nominal timepoints for a given vector of timepoints
#' 
#' @param ... arguments passed to `stats::density` function, e.g `bw` or 
#' `adjust`. The easiest way to adjust the sensitivity to peaks in the data
#' is to use `adjust`. A factor of 0.5 often works well for PK data.
#' 
#' @returns a vector of approximate nominal timepoints estimated from the data
#' 
#' @export
#' 
get_nominal_timepoints <- function(
    t, 
    adjust = 0.5,
    ...
) {
  kernel <- stats::density(t, adjust = adjust, ...)
  peaks <- data.frame(
    time = kernel$x,
    y = kernel$y
  ) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate( # find peaks (where dy/dt changes negative)
      delta = c(diff(y), 0),
      delta_prv = c(0, delta[-length(delta)]),
      peak = delta < 0 & delta_prv >= 0
    ) %>%
    dplyr::filter(peak)

  t_nom <- c(floor(peaks$time[1:(nrow(peaks)-1)]), ceiling(tail(peaks$time,1)))

  t_nom
}