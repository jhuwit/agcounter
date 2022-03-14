
#' Get Actigraph Counts
#'
#' @param result A `data.frame`/matrix with columns `X`, `Y`, `Z`.
#' @param time a vector of times from the original data to be condensed
#' using the `epoch`
#' @param epoch_in_seconds epoch to calculate the counts for, in seconds
#'
#' @return A `data.frame` of result and the time column
#' @export
add_time_column = function(
  time,
  result,
  epoch_in_seconds = 1L
) {
  time = unique(lubridate::floor_date(
    time,
    unit = paste0(epoch_in_seconds, " seconds")))
  epoch_in_seconds = check_epoch(epoch_in_seconds)
  result = as.data.frame(result)
  stopifnot(
    length(time) == nrow(result) ||
      length(time) == (nrow(result) + 1)
  )
  cn = colnames(result)
  result = cbind(time[1:nrow(result)], result)
  colnames(result) = c("HEADER_TIME_STAMP", cn)
  result
}
