testthat::test_that("Checking python output", {
  testthat::skip_if_not_installed("readr")
  module_version = function(module = "numpy") {
    x <- reticulate::import(module)
    x$version$full_version
  }
  check_module_version = function(module, version) {
    if (!reticulate::py_module_available(module)) {
      return(FALSE)
    }
    ver = module_version(module)
    ver = as.package_version(ver)
    ver >= as.package_version(version)
  }

  testthat::skip_if(
    !check_module_version("numpy", "1.20.0")
  )
  testthat::skip_if(
    !check_module_version("scipy", "1.1.0")
  )
  x = download_actgraph_test_data()

  eg = expand.grid(epoch_in_seconds = c(10, 30), sample_rate = c(30, 40))
  eg$raw_file = file.path(tempdir(), "data", "raw",
                          paste0("raw_", eg$epoch_in_seconds, "_", eg$sample_rate,
                                 ".csv"))
  eg$ag_file = file.path(tempdir(), "data", "ActiLifeCounts",
                          paste0("raw_", eg$epoch, "_", eg$sample_rate,
                                 "_counts", eg$epoch_in_seconds, "sec.csv"))
  read_raw = function(file) {
    df = readr::read_csv(file, col_names = FALSE)
    colnames(df) = c("Y", "X", "Z")
    df[, c("X", "Y", "Z")]
  }
  check_it = function(epoch_in_seconds, sample_rate, raw_file, ag_file) {
    df = read_raw(raw_file)
    out_slow = get_counts(df, sample_rate = sample_rate,
                          epoch_in_seconds = epoch_in_seconds,
                          save_memory = TRUE)
    out = get_counts(df, sample_rate = sample_rate,
                     epoch_in_seconds = epoch_in_seconds)
    testthat::expect_equal(out, out_slow)
    out_slow$AGCOUNT = NULL
    out$AGCOUNT = NULL
    colnames(out) = c("Axis1", "Axis2", "Axis3")
    check = readr::read_csv(ag_file, skip = 10)
    check = as.data.frame(check)
    out = as.data.frame(out)
    testthat::expect_equal(out, check)
  }
  for (ifile in seq(nrow(eg))) {
    ieg = eg[ifile,]
    check_it(ieg$epoch_in_seconds, ieg$sample_rate, ieg$raw_file, ieg$ag_file)
  }
})
