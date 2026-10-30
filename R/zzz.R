.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("foreach"),
           requireNamespace, quietly = TRUE)
  ))
}
