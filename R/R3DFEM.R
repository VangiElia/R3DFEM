#' R3DFEM: an R package for running the 3D-CMCC-FEM model (version 5.7)
#'
#' The R3DFEM package provides functions to install the model, create and check
#' the inputs of the simulations, run single simulations or batches of
#' simulations of the 3D-CMCC-FEM (version 5.7), read and plot inputs and
#' outputs. See \code{vignette("R3DFEM")} and \code{vignette("input-files")}
#' for an overview of the package functionality.
#' @note See the
#'   \href{https://www.forest-modelling-lab.com/the-3d-cmcc-model}{official web page
#'   } for a complete documentation of the model.
#' @docType package
#' @name R3DFEM
#' @author Elia Vangi
#' @importFrom utils read.table write.table
#' @importFrom stats complete.cases
#' @importFrom graphics arrows par segments text
NULL

# columns used with non-standard evaluation (ggplot2, data.table, dplyr) and
# foreach iterators
utils::globalVariables(c(".data", ".SD", "..cols", ":=", "YEAR", "MONTH", "DAY", "SPECIE", "LAYER",
                         "value", "variable", "rmean", "meteo_date", "rvalue", "doy", "i", "v", "path"))
