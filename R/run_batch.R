#' Run many simulations of the 3DCMCCFEM model
#'
#' \code{run_batch} runs a set of simulations (e.g. different sites,
#' scenarios, management options or species parameters), each one in its own
#' directory, sequentially or in parallel. The input files are copied for each
#' run, so the simulations do not interfere with each other.
#'
#' @param runs data.frame with one row per simulation. Columns named as the
#'   arguments of \code{\link{run_3DCMCCFEM}} (e.g. \code{site},
#'   \code{year_end}, \code{man}, \code{co2}) set that argument for the run.
#'   A column \code{inputdir} gives a different input directory to each run
#'   (e.g. different sites). A column \code{id} names the runs (default
#'   run_001, run_002, ...). Any other column named as a parameter of the
#'   species file (e.g. \code{LIGHT_TOL}) changes that parameter, for all the
#'   species of the run, in the copy of the species files.
#' @param inputdir Character: input directory used by the runs without an
#'   \code{inputdir} column.
#' @param outputdir Character: directory of the runs. Each run is saved in
#'   outputdir/id/input and outputdir/id/output. Keep it short: the model
#'   cannot write paths longer than 260 characters.
#' @param ... Arguments of \code{\link{run_3DCMCCFEM}} common to all runs
#'   (e.g. \code{species}, \code{year_start}, \code{output}).
#' @param parallel Logical: run the simulations in parallel with the future
#'   framework (package future.apply)? The future plan set by the user is
#'   used, e.g. \code{future::plan("multisession", workers = 4)}. Default to
#'   FALSE
#' @param stop_on_error Logical: stop at the first failed simulation? Default
#'   to FALSE: errors are reported in the result.
#'
#' @return A data.frame with the columns of \code{runs} and the columns
#'   \code{id}, \code{status} ("ok" or "error"), \code{message} (error
#'   message) and \code{output} (path of the main output file, to be read
#'   with \code{\link{read_output}}).
#' @seealso \code{\link{run_3DCMCCFEM}}, \code{\link{param_sensitivity}}
#' @examples
#' \dontrun{
#' inp <- system.file("extdata", "bilykriz", "rcp0p0", package = "R3DFEM")
#' res <- run_batch(data.frame(co2 = c("on", "off")), inputdir = inp,
#'                  outputdir = "C:/sim/co2", site = "BilyKriz",
#'                  species = "Piceaabies", year_start = 1997, year_end = 2050,
#'                  man = "off", output = "annual")
#' out <- read_output(res$output)
#' }
#' @export
run_batch <- function(runs, inputdir = NULL, outputdir, ..., parallel = FALSE, stop_on_error = FALSE) {
  stopifnot("runs must be a data.frame" = is.data.frame(runs),
            "parallel must be logical" = check_log(parallel),
            "stop_on_error must be logical" = check_log(stop_on_error))
  if (nrow(runs) == 0) stop("'runs' has no rows")
  runs <- as.data.frame(runs, stringsAsFactors = FALSE)
  common <- list(...)

  if (is.null(runs$id)) runs$id <- sprintf("run_%03d", seq_len(nrow(runs)))
  runs$id <- as.character(runs$id)
  if (any(duplicated(runs$id))) stop("Duplicated values in runs$id")
  if (is.null(runs$inputdir)) {
    if (is.null(inputdir)) stop("'inputdir' must be given (or a column inputdir in runs)")
    runs$inputdir <- inputdir
  }
  if (any(!dir.exists(runs$inputdir))) stop("Input directory not found: ", paste(unique(runs$inputdir[!dir.exists(runs$inputdir)]), collapse = ", "))

  run_args <- setdiff(names(formals(run_3DCMCCFEM)), c("inputdir", "outputdir"))
  bad <- intersect(names(common), c("inputdir", "outputdir"))
  if (length(bad) != 0) stop("Use the arguments of run_batch for: ", paste(bad, collapse = ", "))
  param_cols <- setdiff(colnames(runs), c(run_args, "id", "inputdir"))
  unknown <- setdiff(names(common), run_args)
  if (length(unknown) != 0) stop("Unknown argument(s) of run_3DCMCCFEM: ", paste(unknown, collapse = ", "))

  check_dir(outputdir)
  outputdir <- normalizePath(outputdir, winslash = "/")
  if (nchar(outputdir) + max(nchar(runs$id)) > 90) {
    warning("outputdir is long (", nchar(outputdir), " characters): the model cannot write paths longer than 260 characters")
  }

  # the options are not exported to the parallel workers: the executable is
  # found here and passed to each run
  exe <- model_path()

  one_run <- function(i) {
    withr::local_options(R3DFEM.exe = exe)
    r <- runs[i, , drop = FALSE]
    rundir <- file.path(outputdir, r$id)
    indir <- file.path(rundir, "input")
    dir.create(indir, recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(r$inputdir, full.names = TRUE), indir, overwrite = TRUE)

    args <- utils::modifyList(common, as.list(r[intersect(colnames(r), run_args)]))
    if (length(param_cols) != 0) {
      if (is.null(args$species)) stop("'species' is needed to change species parameters")
      for (s in unique(args$species)) {
        do.call(set_species_param, c(list(file = file.path(indir, paste0(s, ".txt"))), as.list(r[param_cols])))
      }
    }
    res <- tryCatch({
      files <- suppressMessages(
        do.call(run_3DCMCCFEM, c(args, list(inputdir = indir, outputdir = file.path(rundir, "output")))))
      list(status = "ok", message = "", output = unname(files[1]))
    }, error = function(e) {
      if (stop_on_error) stop("Run ", r$id, ": ", conditionMessage(e), call. = FALSE)
      list(status = "error", message = conditionMessage(e), output = NA_character_)
    })
    res
  }

  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) stop("Package 'future.apply' is needed for parallel = TRUE")
    res <- future.apply::future_lapply(seq_len(nrow(runs)), one_run, future.seed = TRUE)
  } else {
    res <- lapply(seq_len(nrow(runs)), function(i) {
      message("Run ", runs$id[i], " (", i, "/", nrow(runs), ")")
      one_run(i)
    })
  }

  runs$status <- vapply(res, `[[`, "", "status")
  runs$message <- vapply(res, `[[`, "", "message")
  runs$output <- vapply(res, `[[`, "", "output")
  if (any(runs$status == "error")) warning(sum(runs$status == "error"), " run(s) failed: see the column 'message'")
  runs
}

#' Sensitivity of the model to a species parameter
#'
#' \code{param_sensitivity} runs the model with different values of one
#' parameter of the species file (with \code{\link{run_batch}}) and plots the
#' annual values of the selected output variables.
#'
#' @param param Character: name of the parameter of the species file (e.g.
#'   "LIGHT_TOL").
#' @param value Numeric vector: values of the parameter.
#' @param variable Character vector: variables of the annual output to plot
#'   (e.g. c("GPP", "NPP")).
#' @inheritParams run_batch
#' @param ... Other arguments of \code{\link{run_3DCMCCFEM}} (at least site,
#'   species, year_start, year_end, man). The output is always annual.
#'
#' @return A list with \code{data} (data.frame with YEAR, the value of the
#'   parameter, variable and value) and \code{plot} (a ggplot object).
#' @seealso \code{\link{run_batch}}, \code{\link{set_species_param}}
#' @examples
#' \dontrun{
#' inp <- system.file("extdata", "bilykriz", "rcp0p0", package = "R3DFEM")
#' s <- param_sensitivity("LIGHT_TOL", 1:4, c("GPP", "NPP", "HEIGHT"),
#'                        inputdir = inp, outputdir = "C:/sim/sens",
#'                        site = "BilyKriz", species = "Piceaabies",
#'                        year_start = 1997, year_end = 2050, man = "off")
#' s$plot
#' }
#' @export
param_sensitivity <- function(param, value, variable, inputdir, outputdir, ..., parallel = FALSE) {
  stopifnot("param must be a character vector of length 1" = check_char(param),
            "value must be numeric" = check_num_vect(value),
            "variable must be a character vector" = is.character(variable))
  runs <- data.frame(id = paste0(param, "_", seq_along(value)))
  runs[[param]] <- value
  res <- run_batch(runs, inputdir = inputdir, outputdir = outputdir, ..., output = "annual",
                   parallel = parallel)
  ok <- res$status == "ok"
  if (!any(ok)) stop("All the runs failed: ", res$message[1])

  out <- lapply(which(ok), function(i) {
    o <- read_output(res$output[i])
    miss <- setdiff(variable, colnames(o))
    if (length(miss) != 0) stop("Variable(s) not in the annual output: ", paste(miss, collapse = ", "))
    o <- stats::aggregate(o[variable], by = list(YEAR = o$YEAR), FUN = sum)
    o[[param]] <- res[[param]][i]
    o
  })
  data <- reshape2::melt(do.call(rbind, out), id.vars = c("YEAR", param), variable.name = "variable")
  data[[param]] <- as.factor(data[[param]])

  p <- ggplot2::ggplot(data, ggplot2::aes(x = YEAR, y = value, colour = .data[[param]], group = .data[[param]])) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::theme_classic() +
    ggplot2::labs(x = NULL, y = NULL)
  list(data = data, plot = p)
}
