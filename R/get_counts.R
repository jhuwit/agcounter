
get_time_col = function(df) {
  colvec = c("time", "HEADER_TIME_STAMP", "HEADER_TIMESTAMP")
  # stopifnot(
  #   any(colvec %in% colnames(df))
  # )
  res = intersect(colvec, colnames(df))
  if (length(res) == 0) {
    res = NULL
  }
  res
}
get_time = function(df) {
  timecol = get_time_col(df)
  if (length(timecol) != 1) {
    stop("Time column not distinct or not found! failing")
  }
  time = df[[timecol]]
  stopifnot(!is.null(time))
  time
}
get_sample_rate = function(df, sample_rate = NULL) {
  if (!is.null(sample_rate)) {
    return(sample_rate)
  }
  if (is.null(sample_rate) || is.na(sample_rate)) {
    sample_rate = attr(df, "sample_rate")
  }
  if ((is.null(sample_rate) || is.na(sample_rate)) &&
      any(c("time", "HEADER_TIME_STAMP", "HEADER_TIMESTAMP")
          %in% colnames(df))) {
    warning("Guessing sample_rate from the data")
    time = get_time(df)
    time = diff(time)
    units(time) = "secs"
    if (all(time > 1)) {
      # minute level data
      sample_rate = unique(1 / as.numeric(time))
    } else {
      sample_rate = unique(round(1 / as.numeric(time)))
    }
    stopifnot(length(sample_rate) == 1)
  }
  stopifnot(!is.null(sample_rate))
  return(sample_rate)
}

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

rms = function(x, y, z) {
  sqrt(x^2 + y^2 + z^2)
}



check_sample_rate = function(sample_rate, df) {
  sample_rate = get_sample_rate(df, sample_rate)
  sample_rate = as.integer(sample_rate)
  accepted_frequencies = c(30, 40, 50, 60, 70, 80, 90, 100)
  if (!sample_rate %in% accepted_frequencies) {
    warning("sample_rate does not seem to be in the accepted sample_rates!")
  }
  sample_rate
}

check_epoch = function(epoch_in_seconds) {
  if (!is.wholenumber(epoch_in_seconds)) {
    stop("epoch in seconds is not a whole number!")
  }
  epoch_in_seconds = as.integer(epoch_in_seconds)
  epoch_in_seconds
}

# Parameters
# ----------
#   raw : ndarray, shape (n_samples, 3)
# Raw data matrix, in x, y, z directions for 1st, 2nd, 3rd columns.
# freq : int
# Sampling frequency, has to be 30, 40, 50, 60, 70, 80, 90 or 100 Hz.
# epoch : bool
# Epoch length (seconds).
# fast:
#   Use fast implementation
#
# Returns
# -------
#   counts : ndarray, shape (n_epochs, 3)
# The counts, n_epochs = ceil(n_samples/freq).
#' Get Actigraph Counts
#'
#' @param df A `data.frame` with columns `X`, `Y`, `Z`, and
#' a time column either `time`, `HEADER_TIME_STAMP`, or `HEADER_TIMESTAMP`
#' @param epoch_in_seconds epoch to calculate the counts for, in seconds
#' @param sample_rate The sampling rate for the data.  If `NULL`, then
#' it will try to be guessed from `df`
#' @param fast Should the fast implementation be used?  You may want to set
#' this as `FALSE` if your data is *very* big
#' @param verbose print diagnostic messages
#' @param save_memory Should each column be run separately?
#'
#' @return A `data.frame` of each axis count and the RMS of them
#' in the `AGCOUNT` column with a time column
#' @export
#'
#' @examples
#' x = download_actgraph_test_data()
#' epoch = 10
#' sample_rate = 30
#' search_string = paste0("raw_", epoch, "_", sample_rate)
#' testfile = x$ActiLifeCounts
#' testfile = testfile[grepl(search_string, testfile)]
#' file = x$raw
#' file = file[grepl(search_string, file)]
#' if (requireNamespace("readr", quietly = TRUE)) {
#'    df = readr::read_csv(file, col_names = FALSE)
#'    colnames(df) = c("Y", "X", "Z")
#'    df = df[, c("X", "Y", "Z")]
#'    # title_epoch_frequency
#'    out_mem = get_counts(df, sample_rate = sample_rate,
#'    save_memory = TRUE, epoch = epoch, verbose = 2)
#'    out = get_counts(df, sample_rate = sample_rate, epoch = epoch, verbose = 2)
#'    out$AGCOUNT = NULL
#'    check = readr::read_csv(testfile, skip = 10)
#'    stopifnot(all(check == out))
#' }
get_counts = function(
  df,
  epoch_in_seconds = 1L,
  sample_rate = NULL,
  fast = TRUE,
  save_memory = FALSE,
  verbose = TRUE
) {
  f = get_ag_functions()
  py_get_counts = f$get_counts
  rm(f)

  sample_rate = check_sample_rate(sample_rate, df)
  epoch_in_seconds = check_epoch(epoch_in_seconds)

  timecol = get_time_col(df)
  if (!is.null(timecol)) {
    time = get_time(df)
    time = unique(lubridate::floor_date(
      time,
      unit = paste0(epoch_in_seconds, " seconds")))
  }
  xyz = c("X", "Y", "Z")
  cn = colnames(df)
  if (all(tolower(xyz) %in% cn) && !all(xyz %in% cn)) {
    xyz = tolower(xyz)
  }
  df = df[,xyz, drop = FALSE]
  df = as.matrix(df)
  fast = as.logical(fast)
  if (save_memory) {
    result = NULL
    for (i in xyz) {
      if (verbose > 0) {
        message("Running ", i)
      }
      raw_data = df[, i, drop = FALSE]
      df = df[, setdiff(colnames(df), i), drop = FALSE]
      raw_data = py_get_counts(raw = raw_data,
                               epoch = epoch_in_seconds,
                               freq = sample_rate,
                               fast = fast,
                               verbose = as.integer(verbose))
      result = cbind(result, raw_data)
      rm(raw_data)
    }
  } else {
    result = py_get_counts(raw = df,
                           epoch = epoch_in_seconds,
                           freq = sample_rate,
                           fast = fast,
                           verbose = as.integer(verbose))
  }
  colnames(result) = xyz
  result = as.data.frame(result)
  if (!is.null(timecol)) {
    stopifnot(length(time) == nrow(result))
    result = cbind(time, result)
    colnames(result) = c(timecol, xyz)
  }
  result$AGCOUNT = rms(result$X, result$Y, result$Z)
  result
}


resample_data = function(
  df,
  epoch_in_seconds = 1L,
  sample_rate = NULL,
  save_memory = FALSE,
  verbose = TRUE
) {
  f = get_ag_functions()
  resample_func = f$`_resample`
  rm(f)

  sample_rate = check_sample_rate(sample_rate, df)
  epoch_in_seconds = check_epoch(epoch_in_seconds)

  timecol = get_time_col(df)
  if (!is.null(timecol)) {
    time = get_time(df)
    time = unique(lubridate::floor_date(
      time,
      unit = paste0(epoch_in_seconds, " seconds")))
  }
  xyz = c("X", "Y", "Z")
  cn = colnames(df)
  if (all(tolower(xyz) %in% cn) && !all(xyz %in% cn)) {
    xyz = tolower(xyz)
  }
  df = df[,xyz, drop = FALSE]
  df = as.matrix(df)
  if (save_memory) {
    result = NULL
    for (i in xyz) {
      if (verbose > 0) {
        message("Running ", i)
      }
      raw_data = df[, i, drop = FALSE]
      df = df[, setdiff(colnames(df), i), drop = FALSE]
      raw_data = resample_func(raw = raw_data,
                               epoch_seconds = epoch_in_seconds,
                               frequency = sample_rate,
                               verbose = as.integer(verbose))
      result = cbind(result, raw_data)
      rm(raw_data)
    }
  } else {
    result = resample_func(raw = df,
                           epoch_seconds = epoch_in_seconds,
                           frequency = sample_rate,
                           verbose = as.integer(verbose))
  }

  colnames(result) = xyz
  result = as.data.frame(result)
  if (!is.null(timecol)) {
    stopifnot(length(time) == nrow(result))
    result = cbind(time, result)
    colnames(result) = c(timecol, xyz)
  }
  result$AGCOUNT = rms(result$X, result$Y, result$Z)
  result
}

