.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("foreach"),
           requireNamespace, quietly = TRUE)
  ))
}

.onUnload <- function (libpath) {
  library.dynam.unload("R3DFEM", libpath)
  invisible()
}
