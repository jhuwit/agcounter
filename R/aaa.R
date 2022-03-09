
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

stop_module_version = function(module, version) {
  res = check_module_version(module, version)
  if (!res) {
    stop(module, " does not have version >= ", version,
         "please use\n", "reticulate::py_install('", module, "')")
  }
}

get_ag_functions = function() {
  stop_module_version("numpy", "1.20.0")
  stop_module_version("scipy", "1.1.0")

  file = system.file("extract.py", package = "agcounts")
  # file = "inst/extract.py"
  functions = reticulate::py_run_file(file)
}
