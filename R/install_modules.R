#' Install Python Modules for `agcounts`
#'
#' @param ... additional arguments to pass to [reticulate::py_install]
#'
#' @return An invisible `NULL`
#' @export
#'
#' @examples
#' install_agcounts_modules()
install_agcounts_modules = function(...) {

  packages = c("numpy", "scipy", "pandas")
  me = sapply(packages, reticulate::py_module_available)
  if (!all(me)) {
    packages = packages[!me]
    reticulate::py_install(packages = packages, ...)
  }
  get_ag_functions()
  stopifnot(reticulate::py_module_available("pandas"))

  return(invisible(NULL))
}
