
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
  if (is.null(df)) {
    return(NULL)
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
  sqrt(1/3 * (x^2 + y^2 + z^2))
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
#' @rdname get_counts
get_counts_py = function(
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
                               verbose = verbose > 1)
      result = cbind(result, raw_data)
      rm(raw_data)
    }
  } else {
    result = py_get_counts(raw = df,
                           epoch = epoch_in_seconds,
                           freq = sample_rate,
                           fast = fast,
                           verbose = verbose > 1)
  }
  colnames(result) = xyz
  result = as.data.frame(result)
  if (!is.null(timecol)) {
    stopifnot(
      length(time) == nrow(result) ||
        length(time) == (nrow(result) + 1)
    )

    result = cbind(time[1:nrow(result)], result)
    colnames(result) = c(timecol, xyz)
  }
  # if (all(c("X", "Y")))
  # result$AGCOUNT = rms(result$X, result$Y, result$Z)
  result
}

#' @rdname get_counts
#' @param file name of CSV file to run, must have columns `X`, `Y`, `Z`
#' @export
extract_counts_csv = function(
  file,
  sample_rate,
  epoch_in_seconds = 1L,
  fast = TRUE,
  verbose = TRUE
) {
  f = get_ag_functions()

  sample_rate = check_sample_rate(sample_rate, df = NULL)
  epoch_in_seconds = check_epoch(epoch_in_seconds)

  file = normalizePath(path.expand(file), mustWork = TRUE)
  fast = as.logical(fast)
  f$get_counts_csv(file = file,
                   epoch = epoch_in_seconds,
                   freq = sample_rate,
                   fast = fast,
                   verbose = verbose)
}


#' Make Unique Floored  date/time POSIXct
#'
#' @param x a vector of date-time objects
#' @param n number of seconds
#'
#' @return A `POSIXct` vector
#' @export
#'
#' @examples
#' x <- lubridate::ymd_hms("2009-08-03 12:01:29.23")
#' x = seq(x, x + 2000, by = 1/10)
#' floor_unique_seconds(x, n = 30)
#' floor_unique_seconds(x, n = 60)
#' floor_unique_seconds(x, n = 60*5)
#' x = as.POSIXlt(x)
#' floor_unique_seconds(x, n = 60)
#' floor_unique_seconds(x, n = 60*5)
floor_unique_seconds = function(x, n = 60L) {
  if (!is.wholenumber(n)) {
    stop("n is not a whole number!")
  }
  stopifnot(lubridate::is.POSIXct(x) ||
            lubridate::is.POSIXlt(x))
  tzone = lubridate::tz(x)
  x = as.numeric(x)
  x = (x%/%n) * n
  x = unique(x)
  as.POSIXct(x, tz = tzone, origin = lubridate::origin)
}

#' @rdname get_counts
#' @param ... additional arguments to pass to [readr::read_csv]
#' @export
get_counts_csv = function(
  file,
  sample_rate = NULL,
  epoch_in_seconds=60L,
  fast = TRUE,
  verbose = TRUE,
  ...) {
  stopifnot(reticulate::py_module_available("pandas"))

  epoch_in_seconds = check_epoch(epoch_in_seconds)

  # reading in the data - need for time!
  df = readr::read_csv(file, ...)
  cn = colnames(df)
  df$X = df$Y = df$Z = NULL
  gc()

  sample_rate = check_sample_rate(sample_rate, df = df)

  # timecol = get_time_col(df)
  df = get_time(df)
  df = floor_unique_seconds(df, n = epoch_in_seconds)
  time = df;
  rm(df)
  gc()
  if (verbose) {
    message("Time Extracted")
  }
  xyz = intersect(cn, c("X", "Y", "Z"))
  result = extract_counts_csv(
    file,
    sample_rate = sample_rate,
    epoch_in_seconds = epoch_in_seconds,
    fast = fast,
    verbose = verbose > 1)
  colnames(result) = xyz
  result = as.data.frame(result)
  if (verbose) {
    message("Counts created")
  }
  result = add_time_column(
    time,
    result = result,
    epoch_in_seconds = epoch_in_seconds
  )
  result
}

#' @rdname get_counts
#' @export
get_counts = function(
  df,
  epoch_in_seconds = 1L,
  sample_rate = NULL,
  save_memory = FALSE,
  verbose = TRUE
) {
  f = get_ag_functions()

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
  xyz = intersect(colnames(df), xyz)
  df = df[,xyz, drop = FALSE]
  df = as.matrix(df)
  if (save_memory) {
    result = NULL
    for (i in xyz) {
      if (verbose > 0) {
        message("Running ", i)
      }
      raw = df[, i, drop = FALSE]
      df = df[, setdiff(colnames(df), i), drop = FALSE]
      gc()
      if (verbose > 0) {
        message("Resampling Data")
      }
      raw = f$`_resample`(raw = raw,
                          frequency = sample_rate,
                          verbose = verbose > 1)
      gc()
      if (verbose > 0) {
        message("Filtering Data")
      }
      raw = f$`_bpf_filter`(downsample_data = raw,
                            verbose = verbose > 1)
      if (verbose > 0) {
        message("Trimming Data")
      }
      raw = f$`_trim_data`(bpf_data = raw,
                           lfe_select = FALSE,
                           verbose = verbose > 1)
      if (verbose > 0) {
        message("Resampling 10Hz")
      }
      raw = f$`_resample_10hz`(trim_data = raw,
                               verbose = verbose > 1)
      if (verbose > 0) {
        message("Getting counts")
      }
      raw = f$`_sum_counts`(downsample_10hz = raw,
                            epoch_seconds = epoch_in_seconds,
                            verbose = verbose > 1)
      raw = t(raw)
      result = cbind(result, raw)
      rm(raw)
    }
  } else {
    result = extract_counts(raw = df,
                            epoch_in_seconds = epoch_in_seconds,
                            sample_rate = sample_rate,
                            verbose = verbose > 1)
  }
  colnames(result) = xyz
  result = as.data.frame(result)
  if (!is.null(timecol)) {
    stopifnot(length(time) == nrow(result) ||
                length(time) == (nrow(result) + 1)
    )

    result = cbind(time[1:nrow(result)], result)
    colnames(result) = c(timecol, xyz)
  }
  # result$AGCOUNT = rms(result$X, result$Y, result$Z)
  result
}

#' @rdname get_counts
#' @export
#' @param raw a raw matrix of numeric values
extract_counts = function(
  raw,
  epoch_in_seconds = 1L,
  sample_rate = NULL,
  verbose = TRUE
) {
  f = get_ag_functions()
  epoch_in_seconds = as.integer(epoch_in_seconds)
  verbose = as.integer(verbose)

  if (verbose > 0) {
    message("Resampling Data")
  }
  raw = f$`_resample`(raw = raw,
                      frequency = sample_rate,
                      verbose = verbose > 1)
  gc()
  if (verbose > 0) {
    message("Filtering Data")
  }
  raw = f$`_bpf_filter`(downsample_data = raw,
                        verbose = verbose > 1)
  if (verbose > 0) {
    message("Trimming Data")
  }
  raw = f$`_trim_data`(bpf_data = raw,
                       lfe_select = FALSE,
                       verbose = verbose > 1)
  if (verbose > 0) {
    message("Resampling 10Hz")
  }
  raw = f$`_resample_10hz`(trim_data = raw,
                           verbose = verbose > 1)
  if (verbose > 0) {
    message("Getting counts")
  }
  raw = f$`_sum_counts`(downsample_10hz = raw,
                        epoch_seconds = epoch_in_seconds,
                        verbose = verbose > 1)
  raw = t(raw)
  raw
}


#' @rdname get_counts
#' @export
resample_data = function(
  df,
  sample_rate = NULL,
  verbose = TRUE
) {
  f = get_ag_functions()

  xyz = c("X", "Y", "Z")
  cn = colnames(df)
  if (all(tolower(xyz) %in% cn) && !all(xyz %in% cn)) {
    xyz = tolower(xyz)
  }
  xyz = intersect(colnames(df), xyz)
  df = df[,xyz, drop = FALSE]
  df = as.matrix(df)

  sample_rate = check_sample_rate(sample_rate, df)
  f$`_resample`(raw = df,
                frequency = sample_rate,
                verbose = verbose > 1)
}


#' @rdname get_counts
#' @export
filter_data = function(
  df,
  verbose = TRUE
) {
  f = get_ag_functions()

  df = as.matrix(df)
  ddf = dim(df)
  if (ddf[1] > ddf[2]) {
    warning("Transposing the data to be wide, needed for filtering")
    df = t(df)
  }
  if (verbose > 1) {
    message(paste("Dim df: ", paste(dim(df), collapse = " ")))
  }
  if (verbose > 0) {
    message("Filtering Data")
  }
  f$`_bpf_filter`(downsample_data = df,
                  verbose = verbose > 1)
}

#' @rdname get_counts
#' @export
trim_data = function(
  df,
  verbose = TRUE
) {
  f = get_ag_functions()

  df = as.matrix(df)
  ddf = dim(df)
  if (ddf[1] > ddf[2]) {
    warning("Transposing the data to be wide, needed for trimming")
    df = t(df)
  }
  if (verbose > 1) {
    message(paste("Dim df: ", paste(dim(df), collapse = " ")))
  }
  if (verbose > 0) {
    message("Trimming Data")
  }
  f$`_trim_data`(bpf_data = df,
                 lfe_select = FALSE,
                 verbose = verbose > 1)
}


#' @rdname get_counts
#' @export
resample_10hz = function(
  df,
  verbose = TRUE
) {
  f = get_ag_functions()

  df = as.matrix(df)
  ddf = dim(df)
  if (ddf[1] > ddf[2]) {
    warning("Transposing the data to be wide, needed for trimming")
    df = t(df)
  }
  if (verbose > 1) {
    message(paste("Dim df: ", paste(dim(df), collapse = " ")))
  }
  if (verbose > 0) {
    message("Resampling 10Hz")
  }
  f$`_resample_10hz`(trim_data = df,
                     verbose = verbose > 1)

}


#' @rdname get_counts
#' @export
sum_counts = function(
  df,
  epoch_in_seconds = 1L,
  verbose = TRUE
) {
  f = get_ag_functions()

  df = as.matrix(df)
  ddf = dim(df)
  if (ddf[1] > ddf[2]) {
    warning("Transposing the data to be wide, needed for trimming")
    df = t(df)
  }
  if (verbose > 1) {
    message(paste("Dim df: ", paste(dim(df), collapse = " ")))
  }
  if (verbose > 0) {
    message("Getting counts")
  }
  f$`_sum_counts`(downsample_10hz = df,
                  epoch_seconds = epoch_in_seconds,
                  verbose = verbose > 1)

}

