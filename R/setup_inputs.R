#' Prepare the input directory of a simulation
#'
#' \code{setup_inputs} creates in one call all the input files needed by
#' \code{\link{run_3DCMCCFEM}} for a site, named as required by the model, and
#' copies the species parameterizations of the version 5.7 shipped with the
#' package (see \code{\link{list_species}}).
#'
#' @param site Character: name of the site, used to name the files.
#' @param inputdir Character: input directory (created if it does not exist).
#' @param stand,soil,topo,meteo,co2,management Each input can be:
#'   \itemize{
#'   \item the path of an existing file, copied in \code{inputdir} with the
#'   name required by the model (e.g. site_stand.txt);
#'   \item a list of arguments of the corresponding helper
#'   (\code{\link{make_stand}}, \code{\link{make_soil}},
#'   \code{\link{make_topo}}, \code{\link{make_meteo}}, \code{\link{make_co2}},
#'   \code{\link{make_management}}), without \code{site} and \code{outdir};
#'   \item for \code{meteo}, a data.frame of daily data passed to
#'   \code{\link{make_meteo}}; for \code{stand} and \code{soil}, a data.frame
#'   with the columns of the file; for \code{co2}, a data.frame with year and
#'   CO2_ppm; for \code{topo}, the elevation of the site (m).
#'   }
#'   \code{management} is optional (needed with \code{man = "var"}).
#' @param species Character: species parameterizations to copy from the
#'   package. Default to the species of the stand file. Files already in
#'   \code{inputdir} (e.g. site-specific parameterizations) are kept.
#'
#' @return Invisibly, the named paths of the input files.
#' @seealso \code{\link{check_inputs}}, \code{\link{run_3DCMCCFEM}}
#' @examples
#' inp <- file.path(tempdir(), "Collelongo")
#' setup_inputs("Collelongo", inp,
#'              stand = list(year = 2000, age = 100, species = "Fagussylvatica",
#'                           n_trees = 800, avdbh = 30, height = 22),
#'              soil = list(lat = 41.85, lon = 13.59, clay = 20, silt = 40, sand = 40),
#'              topo = 1560,
#'              meteo = system.file("extdata", "soro", "Soroe_meteo.txt", package = "R3DFEM"),
#'              co2 = list(years = 1997:2099, co2 = 380))
#' list.files(inp)
#' @export
setup_inputs <- function(site, inputdir, stand, soil, topo, meteo, co2, management = NULL,
                         species = NULL) {
  stopifnot("site must be a character vector of length 1" = check_char(site))
  check_dir(inputdir)

  files <- c(stand = .place_input(stand, "stand", site, inputdir, make_stand, function(x, f) {
               utils::write.table(x, f, row.names = FALSE, quote = FALSE, sep = ",")
             }),
             soil = .place_input(soil, "soil", site, inputdir, make_soil, function(x, f) {
               utils::write.table(x, f, row.names = FALSE, quote = FALSE, sep = ",")
             }),
             topo = .place_input(topo, "topo", site, inputdir, make_topo, function(x, f) {
               make_topo(site, elev = x, outdir = inputdir)
             }),
             meteo = .place_input(meteo, "meteo", site, inputdir, make_meteo, function(x, f) {
               make_meteo(x, site = site, outdir = inputdir)
             }),
             co2 = .place_input(co2, "co2", site, inputdir, make_co2, function(x, f) {
               make_co2(site, years = x[[1]], co2 = x, outdir = inputdir)
             }))
  if (!is.null(management)) {
    files["management"] <- .place_input(management, "management", site, inputdir, make_management, NULL)
  }

  if (is.null(species)) species <- unique(.read_stand(files[["stand"]])$Species)
  local <- file.exists(file.path(inputdir, paste0(species, ".txt")))
  missing <- species[!local & !species %in% list_species()]
  if (length(missing) != 0) {
    stop("No parameterization for ", paste(missing, collapse = ", "),
         ": add the file(s) to inputdir. Available species: ", paste(list_species(), collapse = ", "))
  }
  if (any(!local)) copy_species(species[!local], inputdir)
  files <- c(files, stats::setNames(file.path(inputdir, paste0(species, ".txt")), species))
  invisible(stats::setNames(normalizePath(files, winslash = "/"), names(files)))
}

# write one input: x is a path (copied), a list (arguments of maker) or a
# value handled by write_value(x, file)
.place_input <- function(x, type, site, inputdir, maker, write_value) {
  file <- file.path(inputdir, paste0(site, "_", type, ".txt"))
  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    if (normalizePath(x, winslash = "/") != normalizePath(file, winslash = "/", mustWork = FALSE)) {
      file.copy(x, file, overwrite = TRUE)
    }
  } else if (is.list(x) && !is.data.frame(x)) {
    do.call(maker, c(list(site = site, outdir = inputdir), x))
  } else if (!is.null(write_value) && !is.character(x)) {
    write_value(x, file)
  } else {
    stop("'", type, "' must be the path of an existing file or a list of arguments of ",
         deparse(substitute(maker)))
  }
  file
}
