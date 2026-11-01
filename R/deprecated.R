#' Deprecated functions
#'
#' These functions are kept for compatibility with R3DFEM 0.1 and will be
#' removed in a future version.
#' \itemize{
#' \item \code{run_3DCMCCFEM_5.7}: use \code{\link{run_3DCMCCFEM}}, that now
#' runs the version 5.7 of the model.
#' \item \code{make_meteo_EOBS_30.0}: use \code{\link{make_meteo_EOBS}} with
#' \code{version = "30.0"} (the default).
#' }
#'
#' @param ... Arguments passed to the new function.
#' @return The value of the new function.
#' @name R3DFEM-deprecated
#' @keywords internal
NULL

#' @rdname R3DFEM-deprecated
#' @export
run_3DCMCCFEM_5.7 <- function(...) {
  .Deprecated("run_3DCMCCFEM", package = "R3DFEM")
  run_3DCMCCFEM(...)
}

#' @rdname R3DFEM-deprecated
#' @export
make_meteo_EOBS_30.0 <- function(...) {
  .Deprecated("make_meteo_EOBS", package = "R3DFEM")
  make_meteo_EOBS(..., version = "30.0")
}
