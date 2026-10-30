# Helpers to create the input files of the 3D-CMCC-FEM model

# recycle the arguments of length 1 to length n, error otherwise
.recycle <- function(args, n) {
  lapply(stats::setNames(names(args), names(args)), function(a) {
    v <- args[[a]]
    if (length(v) == 1) return(rep(v, n))
    if (length(v) != n) stop("Argument '", a, "' must be of length 1 or ", n)
    v
  })
}

#' Create stand input file for 3DCMCCFEM model
#'
#' \code{make_stand} writes the site_stand.txt file from user-provided values,
#' following the 3D-CMCC-FEM model specifications. Each element of the
#' vectors is a row of the stand file, i.e., a class (species, age, DBH,
#' height) in a given year.
#'
#' @param site Character: Name of the site. Used to construct output name
#' @param year Integer vector: reference year(s) of the stand data. The first
#'   year must be the \code{year_start} of the simulation. More years can be
#'   used with \code{man = "var"} and \code{pres_dens = "on"}.
#' @param age Numeric vector: age of the class (years)
#' @param species Character vector: species of the class. Must be the name of
#'   the parameterization file (without .txt), e.g. "Piceaabies"
#' @param n_trees Numeric vector: number of trees in the cell
#' @param avdbh Numeric vector: average DBH of the class (cm)
#' @param height Numeric vector: average height of the class (m)
#' @param management Character vector: "T" (timber) or "C" (coppice). Default
#'   to "T"
#' @param x,y Numeric: cell position. Default to 0
#' @param stool Numeric vector: number of stools (coppice). Default to 0
#' @param wf,wrc,wrf,ws,wbb,wres Numeric vectors: initial biomass of foliage,
#'   coarse roots, fine roots, stem, branch and bark, reserves. Default to 0
#'   (computed by the model)
#' @param lai Numeric vector: LAI of the class. Default to -9999 (computed by
#'   the model)
#' @param species_dir Character: optional path to the directory with the
#'   species parameterization files. If given, the existence of the files is
#'   checked
#' @param outdir Character: path to the output directory.
#'
#' @return Invisibly, the stand data.frame. The file site_stand.txt is saved
#'   in \code{outdir}.
#' @seealso \code{\link{run_3DCMCCFEM}}, \code{\link{virtual_stand_generator}}
#' @examples
#' \dontrun{
#' make_stand("BilyKriz", year = 1997, age = 16, species = "Piceaabies",
#'            n_trees = 2408, avdbh = 7.11, height = 5.63, outdir = tempdir())
#' }
#' @export
make_stand <- function(site, year, age, species, n_trees, avdbh, height,
                       management = "T", x = 0, y = 0, stool = 0,
                       wf = 0, wrc = 0, wrf = 0, ws = 0, wbb = 0, wres = 0, lai = -9999,
                       species_dir = NULL, outdir) {

  stopifnot("site must be a character vector of length 1" = check_char(site),
            "species must be a character vector" = is.character(species))
  check_dir(outdir)

  args <- list(year = year, age = age, species = species, n_trees = n_trees, avdbh = avdbh,
               height = height, management = management, x = x, y = y, stool = stool,
               wf = wf, wrc = wrc, wrf = wrf, ws = ws, wbb = wbb, wres = wres, lai = lai)
  n <- max(lengths(args))
  a <- .recycle(args, n)

  num <- c("year", "age", "n_trees", "avdbh", "height", "x", "y", "stool", "wf", "wrc", "wrf", "ws", "wbb", "wres", "lai")
  for (v in num) if (!is.numeric(a[[v]]) || anyNA(a[[v]])) stop("Argument '", v, "' must be numeric without NA")
  if (any(a$age < 0)) stop("'age' must be >= 0")
  if (any(a$n_trees <= 0)) stop("'n_trees' must be > 0")
  if (any(a$avdbh <= 0) || any(a$height <= 0)) stop("'avdbh' and 'height' must be > 0")
  if (!all(a$management %in% c("T", "C"))) stop("'management' must be 'T' or 'C'")
  if (grepl("\\s", paste(a$species, collapse = ""))) stop("'species' must not contain spaces (it is the name of the parameterization file)")
  if (!is.null(species_dir)) {
    miss <- unique(a$species[!file.exists(file.path(species_dir, paste0(a$species, ".txt")))])
    if (length(miss) != 0) stop("Missing parameterization file(s) in species_dir: ", paste0(miss, ".txt", collapse = ", "))
  }

  stand <- data.frame(Year = a$year, x = a$x, y = a$y, Age = a$age, Species = a$species,
                      Management = a$management, N = a$n_trees, Stool = a$stool,
                      AvDBH = a$avdbh, Height = a$height, Wf = a$wf, Wrc = a$wrc, Wrf = a$wrf,
                      Ws = a$ws, Wbb = a$wbb, Wres = a$wres, Lai = a$lai)
  stand <- stand[order(stand$Year), ]

  file <- file.path(outdir, paste0(site, "_stand.txt"))
  utils::write.table(stand, file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ",")
  message("file ", file, " saved")
  invisible(stand)
}

#' Create CO2 input file for 3DCMCCFEM model
#'
#' \code{make_co2} writes the site_co2.txt file with the yearly atmospheric
#' CO2 concentration. The values can be a constant, a vector with one value
#' per year, or a data.frame of known values that are linearly interpolated
#' over the requested years (and kept constant outside the known range).
#'
#' @param site Character: Name of the site. Used to construct output name
#' @param years Integer vector: years to be written (e.g. 1950:2100). Must
#'   include all the years of the simulation.
#' @param co2 Numeric of length 1 (constant concentration), numeric of the
#'   same length of \code{years}, or a data.frame with columns year and
#'   CO2_ppm (e.g. a scenario), or a path to an existing co2.txt file.
#' @param outdir Character: path to the output directory.
#'
#' @return Invisibly, the co2 data.frame. The file site_co2.txt is saved in
#'   \code{outdir}.
#' @seealso \code{\link{make_CCS}}, \code{\link{run_3DCMCCFEM}}
#' @examples
#' \dontrun{
#' make_co2("BilyKriz", years = 1997:2100,
#'          co2 = data.frame(year = c(1997, 2020, 2100), CO2_ppm = c(363, 412, 600)),
#'          outdir = tempdir())
#' }
#' @export
make_co2 <- function(site, years, co2, outdir) {

  stopifnot("site must be a character vector of length 1" = check_char(site),
            "years must be numeric" = check_num_vect(years))
  check_dir(outdir)
  years <- sort(unique(years))
  if (any(diff(years) != 1)) warning("'years' is not a continuous sequence")

  if (is.character(co2)) co2 <- .read_co2(co2)
  if (is.data.frame(co2)) {
    if (ncol(co2) < 2) stop("co2 data.frame must have two columns: year and CO2_ppm")
    if (any(years < min(co2[[1]]) | years > max(co2[[1]]))) {
      warning("Some years are outside the range of the co2 data: the first/last value is used")
    }
    value <- stats::approx(co2[[1]], co2[[2]], xout = years, rule = 2)$y
  } else if (is.numeric(co2) && length(co2) %in% c(1, length(years))) {
    value <- rep_len(co2, length(years))
  } else {
    stop("co2 must be a number, a numeric vector of the same length of years, a data.frame or a path to a co2 file")
  }
  if (anyNA(value) || any(value <= 0)) stop("CO2 concentrations must be positive and not NA")

  out <- data.frame(year = years, CO2_ppm = value)
  file <- file.path(outdir, paste0(site, "_co2.txt"))
  utils::write.table(out, file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
  message("file ", file, " saved")
  invisible(out)
}

#' Create management input file for 3DCMCCFEM model
#'
#' \code{make_management} writes the site_management.txt file used by the
#' model when \code{man = "var"}: years of thinning (and their intensity),
#' harvesting and regeneration.
#'
#' @param site Character: Name of the site. Used to construct output name
#' @param thinning Integer vector: years of thinning. Default to NULL
#' @param thinning_int Numeric vector: intensity of each thinning (\% of basal
#'   area removed). If NULL the parameter THINNING_INTENSITY of the species
#'   file is used. Default to NULL
#' @param harvesting Integer vector: years of harvesting. Default to NULL
#' @param regeneration Integer vector: years of regeneration. Default to NULL
#' @param outdir Character: path to the output directory.
#'
#' @return Invisibly, a named list with the management actions. The file
#'   site_management.txt is saved in \code{outdir}.
#' @seealso \code{\link{run_3DCMCCFEM}}, \code{\link{check_inputs}}
#' @examples
#' \dontrun{
#' make_management("BilyKriz", thinning = c(2030, 2060, 2090),
#'                 thinning_int = c(30, 20, 10), harvesting = 2102,
#'                 regeneration = 2103, outdir = tempdir())
#' }
#' @export
make_management <- function(site, thinning = NULL, thinning_int = NULL, harvesting = NULL,
                            regeneration = NULL, outdir) {

  stopifnot("site must be a character vector of length 1" = check_char(site))
  check_dir(outdir)

  actions <- list(Thinning = thinning, Thinning_int = thinning_int,
                  Regeneration = regeneration, Harvesting = harvesting)
  actions <- actions[lengths(actions) != 0]
  if (length(actions) == 0) stop("At least one management action must be provided")
  for (a in names(actions)) {
    if (!is.numeric(actions[[a]]) || anyNA(actions[[a]])) stop("'", tolower(a), "' must be numeric without NA")
  }
  if (!is.null(thinning_int)) {
    if (is.null(thinning)) stop("'thinning_int' requires 'thinning'")
    if (length(thinning_int) != length(thinning)) stop("'thinning' and 'thinning_int' must have the same length")
    if (any(thinning_int <= 0 | thinning_int > 100)) stop("'thinning_int' must be in (0, 100]")
  }
  if (any(duplicated(thinning))) stop("Duplicated years in 'thinning'")
  if (any(harvesting %in% thinning)) stop("Harvesting and thinning cannot be performed in the same year")

  lines <- vapply(names(actions), function(a) paste(c(a, actions[[a]]), collapse = ","), "")
  file <- file.path(outdir, paste0(site, "_management.txt"))
  writeLines(lines, file)
  message("file ", file, " saved")
  invisible(actions)
}

#' Create soil input file for 3DCMCCFEM model
#'
#' \code{make_soil} writes the site_soil.txt file from user-provided values
#' (e.g. field measurements). For European sites the texture can be taken
#' from SoilGrids with \code{\link{make_soil_ISRIC}}.
#'
#' @param site Character: Name of the site. Used to construct output name
#' @param lat,lon Numeric: latitude and longitude of the site (WGS84)
#' @param clay,silt,sand Numeric: percentage of clay, silt and sand. Their sum
#'   must be 100 (+-1)
#' @param soil_depth Numeric: soil depth (cm). Default to 100
#' @param fr Numeric: fertility rating (only LUE version). Default to 0.9
#' @param fn0 Numeric: value of fertility modifier when FR=0. Default to 0.5
#' @param fnn Numeric: Default to 0.5
#' @param m0 Numeric: value of 'm' when FR=0. Default to 0.2
#' @param litterc,littern,soilc,soiln,deadwoodc Numeric: initial carbon and
#'   nitrogen pools. Default to -9999 (computed by the model)
#' @param outdir Character: path to the output directory.
#'
#' @return Invisibly, the soil data.frame. The file site_soil.txt is saved in
#'   \code{outdir}.
#' @seealso \code{\link{make_soil_ISRIC}}, \code{\link{plot_soil_3DFEM}}
#' @export
make_soil <- function(site, lat, lon, clay, silt, sand, soil_depth = 100,
                      fr = 0.9, fn0 = 0.5, fnn = 0.5, m0 = 0.2,
                      litterc = -9999, littern = -9999, soilc = -9999, soiln = -9999,
                      deadwoodc = -9999, outdir) {

  stopifnot("site must be a character vector of length 1" = check_char(site))
  for (v in c("lat", "lon", "clay", "silt", "sand", "soil_depth")) {
    if (!check_num(get(v)) || is.na(get(v))) stop("'", v, "' must be a number")
  }
  check_dir(outdir)
  if (abs(lat) > 90 || abs(lon) > 180) stop("'lat' and 'lon' must be in decimal degrees")
  if (any(c(clay, silt, sand) < 0)) stop("clay, silt and sand must be >= 0")
  if (abs(clay + silt + sand - 100) > 1) stop("clay + silt + sand must be 100 (found ", round(clay + silt + sand, 2), ")")
  if (soil_depth <= 0) stop("'soil_depth' must be > 0 (cm)")

  soil <- data.frame(X = 0, Y = 0, LANDUSE = "F", LAT = lat, LON = lon,
                     CLAY_PERC = clay, SILT_PERC = silt, SAND_PERC = sand, SOIL_DEPTH = soil_depth,
                     FR = fr, FN0 = fn0, FNN = fnn, M0 = m0, LITTERC = litterc, LITTERN = littern,
                     SOILC = soilc, SOILN = soiln, DEADWOODC = deadwoodc)
  file <- file.path(outdir, paste0(site, "_soil.txt"))
  utils::write.table(soil, file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ",")
  message("file ", file, " saved")
  invisible(soil)
}

#' Read and modify species parameterization files
#'
#' \code{read_species} reads a species parameterization file (e.g.
#' Piceaabies.txt) as a data.frame. \code{set_species_param} changes the
#' value of one or more parameters, keeping comments and the order of the
#' file, and optionally writes the result in a new file (e.g. to create a new
#' parameterization or for sensitivity analysis).
#'
#' @param file Character: path to the species file
#' @param ... Named values of the parameters to be changed, e.g.
#'   \code{LIGHT_TOL = 3, SLA_AVG0 = 8}
#' @param outfile Character: path of the file to be written. Default to
#'   \code{file} (the file is overwritten)
#'
#' @return \code{read_species}: a data.frame with columns parameter and value.
#'   \code{set_species_param}: invisibly, the previous values of the changed
#'   parameters.
#' @examples
#' \dontrun{
#' sp <- read_species("Piceaabies.txt")
#' set_species_param("Piceaabies.txt", LIGHT_TOL = 3, outfile = "Piceaabies_lt3.txt")
#' }
#' @export
read_species <- function(file) {
  v <- .read_species_values(file)
  data.frame(parameter = names(v), value = unname(v))
}

#' @rdname read_species
#' @export
set_species_param <- function(file, ..., outfile = file) {
  new <- list(...)
  if (length(new) == 0 || is.null(names(new)) || any(!nzchar(names(new)))) {
    stop("Parameters must be given as named values, e.g. LIGHT_TOL = 3")
  }
  if (any(lengths(new) != 1) || !all(vapply(new, is.numeric, logical(1)))) {
    stop("Each parameter value must be a single number")
  }
  lines <- readLines(file, warn = FALSE)
  key <- sub("^\\s*([^[:space:]/]+).*$", "\\1", lines)
  is_param <- !grepl("^\\s*//", lines) & nzchar(trimws(lines))
  old <- stats::setNames(numeric(length(new)), names(new))
  for (p in names(new)) {
    i <- which(is_param & key == p)
    if (length(i) == 0) stop("Parameter '", p, "' not found in ", file)
    old[p] <- suppressWarnings(as.numeric(strsplit(trimws(gsub("//.*", "", lines[i[1]])), "\\s+")[[1]][2]))
    # replace the value, keep separator and trailing comment
    lines[i] <- sub("^(\\s*[^[:space:]]+\\s+)[^[:space:]/]+", paste0("\\1", format(new[[p]], scientific = FALSE, digits = 15)), lines[i])
  }
  writeLines(lines, outfile)
  invisible(old)
}

#' Create meteo input file for 3DCMCCFEM model from daily data
#'
#' \code{make_meteo} converts a data.frame of daily weather data (e.g. from a
#' weather station or a climate model) into the site_meteo.txt file required
#' by the model. For European sites the data can be taken from E-OBS with
#' \code{\link{make_meteo_EOBS_30.0}}.
#'
#' @param data data.frame with daily data. It must contain a column
#'   \code{date} (Date or "yyyy-mm-dd") or the columns \code{Year},
#'   \code{Month} and \code{n_days}, and the columns \code{Rg_f} (global
#'   radiation), \code{Tmax}, \code{Tmin} (°C), \code{Precip} (mm day-1) and
#'   \code{RH_f} (\%). Optional columns: \code{Ta_f} (mean temperature,
#'   computed as (Tmax+Tmin)/2 if missing), \code{Ts_f}, \code{SWC},
#'   \code{LAI}, \code{ET}, \code{WS_f} (set to -9999 if missing).
#' @param site Character: Name of the site. Used to construct output name
#' @param rad_unit Character: unit of \code{Rg_f}: "MJ" (MJ m-2 day-1, model
#'   unit) or "W" (W m-2, daily mean). Default to "MJ"
#' @param check Logical: check the result with \code{\link{check_meteo_3DFEM}}
#'   (without correcting it)? Default to TRUE
#' @param outdir Character: path to the output directory.
#'
#' @return Invisibly, the meteo data.frame. The file site_meteo.txt is saved in
#'   \code{outdir}.
#' @seealso \code{\link{make_meteo_EOBS_30.0}}, \code{\link{check_meteo_3DFEM}},
#'   \code{\link{make_CCS}}
#' @export
make_meteo <- function(data, site, rad_unit = c("MJ", "W"), check = TRUE, outdir) {

  stopifnot("data must be a data.frame" = is.data.frame(data),
            "site must be a character vector of length 1" = check_char(site))
  rad_unit <- match.arg(rad_unit)
  check_dir(outdir)
  data <- as.data.frame(data)

  if ("date" %in% colnames(data)) {
    d <- as.Date(data$date)
    if (anyNA(d)) stop("Column 'date' must be a Date or a character in the form yyyy-mm-dd")
  } else if (all(c("Year", "Month", "n_days") %in% colnames(data))) {
    d <- as.Date(sprintf("%04d-%02d-%02d", as.integer(data$Year), as.integer(data$Month), as.integer(data$n_days)))
    if (anyNA(d)) stop("Invalid dates in columns Year, Month, n_days")
  } else {
    stop("data must have a column 'date' or the columns 'Year', 'Month', 'n_days'")
  }
  data <- data[order(d), ]
  d <- sort(d)
  if (any(duplicated(d))) stop("Duplicated dates in data")
  full <- seq(min(d), max(d), by = "day")
  if (length(full) != length(d)) {
    stop(length(full) - length(d), " missing day(s) in data, e.g. ", paste(utils::head(full[!full %in% d], 5), collapse = ", "))
  }

  required <- c("Rg_f", "Tmax", "Tmin", "Precip", "RH_f")
  miss <- setdiff(required, colnames(data))
  if (length(miss) != 0) stop("Missing column(s) in data: ", paste(miss, collapse = ", "))
  opt <- function(v) if (v %in% colnames(data)) data[[v]] else -9999

  meteo <- data.frame(Year = as.integer(format(d, "%Y")),
                      Month = as.integer(format(d, "%m")),
                      n_days = as.integer(format(d, "%d")),
                      Rg_f = if (rad_unit == "W") data$Rg_f * 0.0864 else data$Rg_f,
                      Ta_f = if ("Ta_f" %in% colnames(data)) data$Ta_f else (data$Tmax + data$Tmin) / 2,
                      Tmax = data$Tmax,
                      Tmin = data$Tmin,
                      RH_f = data$RH_f,
                      Ts_f = opt("Ts_f"),
                      Precip = data$Precip,
                      SWC = opt("SWC"),
                      LAI = opt("LAI"),
                      ET = opt("ET"),
                      WS_f = opt("WS_f"))

  if (check) check_meteo_3DFEM(meteo, correct = FALSE, impute = FALSE, save = FALSE)

  file <- file.path(outdir, paste0(site, "_meteo.txt"))
  utils::write.table(meteo, file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
  message("file ", file, " saved")
  invisible(meteo)
}

#' Check the input files before running the 3DCMCCFEM model
#'
#' \code{check_inputs} performs the same consistency checks of
#' \code{\link{run_3DCMCCFEM}} and \code{\link{run_3DCMCCFEM_5.7}} (presence
#' of the input files, years in stand, meteo and co2 files, management and
#' replanting options) without running the model.
#'
#' @inheritParams run_3DCMCCFEM_5.7
#' @param rep_trees,rep_age,rep_dbh,rep_lai,rep_height,rep_species Replanting
#'   parameters (number of trees, age, average DBH, LAI, height and species).
#'   Default to -9999.
#' @param version Character: version of the model that will be used, "5.6"
#'   (\code{run_3DCMCCFEM}) or "5.7" (\code{run_3DCMCCFEM_5.7}). With "5.7"
#'   the species files must contain the parameter CONV_VOL_FACTOR. Default to
#'   "5.6"
#'
#' @return Invisibly TRUE if all checks are passed, otherwise an error is
#'   thrown. Warnings are issued for possible inconsistencies.
#' @seealso \code{\link{run_3DCMCCFEM}}, \code{\link{run_3DCMCCFEM_5.7}}
#' @export
check_inputs <- function(site, species, year_start, year_end, man, co2 = "on",
                         pres_dens = NULL, after_pres = NULL, reg = "off",
                         year_start_man = -9999,
                         rep_trees = -9999, rep_age = -9999, rep_dbh = -9999, rep_lai = -9999,
                         rep_height = -9999, rep_species = -9999, reg_species = -9999,
                         reg_trees = -9999, reg_age = -9999, reg_dbh = -9999, reg_lai = -9999,
                         reg_height = -9999, inputdir, check_meteo = FALSE,
                         version = c("5.6", "5.7")) {
  version <- match.arg(version)
  .check_run_options(co2, man, pres_dens, after_pres, reg, "annual")
  .check_run_inputs(site = site, species = species, year_start = year_start, year_end = year_end,
                    man = man, pres_dens = pres_dens, after_pres = after_pres, reg = reg,
                    year_start_man = year_start_man,
                    rep_par = list(rep_trees, rep_age, rep_dbh, rep_lai, rep_height, rep_species),
                    reg_par = list(reg_trees, reg_age, reg_dbh, reg_lai, reg_height),
                    extra_species = c(rep_species, reg_species),
                    inputdir = .normalize_dir(inputdir), check_meteo = check_meteo,
                    required_params = if (version == "5.7") "CONV_VOL_FACTOR")
  message("All checks passed")
  invisible(TRUE)
}
