#' @rdname get_counts
#' @export
download_actgraph_test_data = function() {
  exdir = tempdir()
  data_dir = file.path(exdir, "data", fsep = "/")
  if (dir.exists(data_dir)) {
    warning("data dir exists in tempdir(), simply listing")
    x = list.files(data_dir, full.names = TRUE,
                   all.files = TRUE, no.. = TRUE,
                   recursive = TRUE)
  } else {
    tfile = tempfile(fileext = ".zip")
    url = "https://github.com/actigraph/agcounts/raw/main/data.zip"
    download.file(url, tfile, mode = "wb")
    dir.create(exdir)
    unzip(tfile, exdir = exdir)
    x = list.files(data_dir, full.names = TRUE,
                   all.files = TRUE, no.. = TRUE,
                   recursive = TRUE)
    x = split(x, basename(dirname(x)))
  }
  x
}

