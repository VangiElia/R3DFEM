# Internal helpers of run_3DCMCCFEM(), check_inputs() and run_batch()

# parameters of the species files required by the version 5.7 of the model
.required_params <- "CONV_VOL_FACTOR"

.normalize_dir <- function(x) {
  if (!dir.exists(x)) stop("Directory '", x, "' does not exist")
  sub("/+$", "", normalizePath(x, winslash = "/"))
}

# the model always writes in a directory called 'output' relative to the
# working directory: the wrapper makes sure that outputdir is named 'output'
.prepare_outputdir <- function(outputdir) {
  outputdir <- sub("/+$", "", gsub("\\\\", "/", outputdir))
  if (basename(outputdir) != "output") {
    message("outputdir should be named 'output'. outputdir will be renamed to meet the model specification")
    outputdir <- file.path(outputdir, "output")
  }
  dir.create(outputdir, recursive = TRUE, showWarnings = FALSE)
  outputdir <- normalizePath(outputdir, winslash = "/")
  # the model adds about 150 characters (output_<version>_<date>/<type>/<file>)
  if (.Platform$OS.type == "windows" && nchar(outputdir) > 100) {
    warning("outputdir is ", nchar(outputdir), " characters long: the model cannot write files whose ",
            "full path is longer than 260 characters. Use a shorter outputdir if the run fails.")
  }
  outputdir
}

# checks on the combination of options (always performed, also with check_input = FALSE)
.check_run_options <- function(co2, man, pres_dens, after_pres, reg, output) {
  if (!co2 %in% c("on", "off", "var")) stop("Argument 'co2' must be one of 'on', 'off', 'var'.")
  if (!man %in% c("on", "off", "var")) stop("Argument 'man' must be one of 'on', 'off', 'var'.")
  if (!reg %in% c("on", "off")) stop("Argument 'reg' must be one of 'on', 'off'.")
  if (length(output) != 1 || !output %in% c("daily", "monthly", "annual")) {
    stop("Argument 'output' must be one of 'daily', 'monthly', 'annual'.")
  }
  if (reg == "on" && man != "var") stop("\n'reg' = on: \n- 'man' must be = var.")
  if (man == "var" && is.null(pres_dens)) stop("\n'man' = var: \n- Argument 'pres_dens' is missing, with no default.")
  if (man == "var" && !pres_dens %in% c("on", "off")) stop("Argument 'pres_dens' must be one of 'on', 'off'.")
  if (man == "var" && pres_dens == "on" && is.null(after_pres)) {
    stop("\n'man' = var and 'pres_dens' = on: \n- Argument 'after_pres' is missing, with no default.")
  }
  invisible(TRUE)
}

# is the management file passed to the model?
.uses_management <- function(man, pres_dens, after_pres, reg) {
  man == "var" && !(pres_dens == "on" && after_pres == "off" && reg == "off")
}

# management file: one row per action ("Thinning,2030,2060"); returns a named
# list (lower case names) of numeric vectors
.read_management <- function(file) {
  lines <- trimws(gsub("//.*", "", readLines(file, warn = FALSE)))
  lines <- lines[nzchar(lines)]
  fields <- strsplit(lines, ",")
  values <- lapply(fields, function(x) {
    x <- suppressWarnings(as.numeric(trimws(x[-1])))
    x[!is.na(x)]
  })
  names(values) <- tolower(trimws(vapply(fields, `[`, "", 1)))
  values
}

# species parameterization file: named numeric vector of parameter values
.read_species_values <- function(file) {
  # files saved with a byte order mark (BOM) would give a wrong first parameter
  lines <- sub("^\xef\xbb\xbf", "", readLines(file, warn = FALSE), useBytes = TRUE)
  lines <- stringr::str_squish(gsub("//.*", "", lines))
  lines <- lines[nzchar(lines)]
  parts <- strsplit(lines, " ")
  stats::setNames(suppressWarnings(as.numeric(vapply(parts, `[`, "", 2))),
                  vapply(parts, `[`, "", 1))
}

# co2 file: first column year, second column CO2 concentration
.read_co2 <- function(file) {
  first <- readLines(file, n = 1, warn = FALSE)
  sep <- if (grepl(",", first)) "," else if (grepl(";", first)) ";" else ""
  co <- utils::read.table(file, header = TRUE, sep = sep)
  co[, vapply(co, function(x) !all(is.na(x)), logical(1)), drop = FALSE]
}

.read_stand <- function(file, ...) {
  utils::read.table(file, sep = ",", header = TRUE, strip.white = TRUE, ...)
}

# consistency checks of the input files (performed when check_input = TRUE)
.check_run_inputs <- function(site, species, year_start, year_end, man, pres_dens, after_pres,
                              reg, year_start_man, rep_par, reg_par, extra_species = NULL,
                              inputdir, check_meteo = FALSE, required_params = .required_params) {

  if (man == "on" && year_start_man == -9999) {
    stop("'man' = on: \n- Argument 'year_start_man' is missing, with no default.")
  }
  if (man == "var" && pres_dens == "off" && !is.null(after_pres)) {
    warning("'man' = var and 'pres_dens' = off: \n- Unused argument 'after_pres'.")
  }
  if (man %in% c("var", "off") && year_start_man != -9999) {
    warning(paste0("'man' = ", man, ": \n- Unused argument 'year_start_man'."))
  }

  use_man <- .uses_management(man, pres_dens, after_pres, reg)

  # input files
  extra_species <- setdiff(as.character(extra_species), "-9999")
  files_to_check <- c(paste0(site, c("_stand.txt", "_soil.txt", "_topo.txt", "_co2.txt", "_meteo.txt")),
                      if (use_man) paste0(site, "_management.txt"),
                      paste0(unique(c(species, extra_species)), ".txt"))
  missing <- files_to_check[!file.exists(file.path(inputdir, files_to_check))]
  if (length(missing) != 0) stop("\nMissing input file(s): ", paste(missing, collapse = ", "))

  # stand file
  st <- .read_stand(file.path(inputdir, paste0(site, "_stand.txt")))
  if (!year_start %in% st$Year) {
    stop(paste0("\nValue of argument 'year_start' not in '", site, "_stand.txt'."))
  }
  sp_stand <- unique(st$Species)
  sp_nofile <- sp_stand[!file.exists(file.path(inputdir, paste0(sp_stand, ".txt")))]
  if (length(sp_nofile) != 0) {
    stop("\nMissing parameterization file for species in '", site, "_stand.txt': ",
         paste0(sp_nofile, ".txt", collapse = ", "))
  }
  # parameters required by the version 5.7 (e.g. CONV_VOL_FACTOR)
  for (s in unique(c(sp_stand, species, extra_species))) {
    miss_par <- setdiff(required_params, names(.read_species_values(file.path(inputdir, paste0(s, ".txt")))))
    if (length(miss_par) != 0) {
      stop("\nParameter(s) ", paste(miss_par, collapse = ", "), " missing in '", s,
           ".txt' (required by the version 5.7 of the model, see list_species()).")
    }
  }

  # years between year_start and year_end must be in co2 and meteo files
  meteo_file <- file.path(inputdir, paste0(site, "_meteo.txt"))
  mt_year <- data.table::fread(meteo_file, select = 1)[[1]]
  co_year <- .read_co2(file.path(inputdir, paste0(site, "_co2.txt")))[[1]]
  y <- seq(year_start, year_end)
  if (!all(y %in% mt_year) || !all(y %in% co_year)) {
    stop(paste0("\nMissing simulated time span from '", site, "_meteo.txt' and/or '", site, "_co2.txt'."))
  }

  if (check_meteo) {
    check_meteo_3DFEM(file_meteo = meteo_file, correct = FALSE, impute = FALSE, save = FALSE)
  }

  rep_missing <- any(unlist(rep_par) == -9999)

  if (man %in% c("on", "var")) {
    sp_par <- lapply(stats::setNames(nm = species), function(s) {
      .read_species_values(file.path(inputdir, paste0(s, ".txt")))
    })
  }

  # man = on checks
  if (man == "on") {
    if (year_start_man < year_start) {
      stop("\n'man' = on: \n* Value of argument 'year_start_man' < 'year_start'.")
    }
    for (s in species) {
      p <- sp_par[[s]][c("ROTATION", "THINNING", "THINNING_INTENSITY")]
      if (anyNA(p) || any(p == -9999)) {
        stop(paste0("\n'man' = on: \n* Missing management parameter(s) in '", s, ".txt'."))
      }
      if (year_start_man - year_start > p[["THINNING"]]) {
        stop(paste0("\n'man' = on: \n* Value of 'year_start_man' > thinning interval from 'year_start'. \n* Parameter 'THINNING' in '",
                    s, ".txt' = ", p[["THINNING"]], "."))
      }
      # harvest happens when the oldest class of the species reaches ROTATION
      age <- st$Age[st$Year == year_start & st$Species == s]
      if (length(age) != 0 && year_start + p[["ROTATION"]] - max(age) < year_end && rep_missing) {
        stop("\n'man' = on and harvesting year ('ROTATION') is reached: \n* Replanting arguments are missing, with no default.")
      }
    }
  }

  # man = var checks
  if (man == "var") {
    if (pres_dens == "on" && length(unique(st$Year)) == 1) {
      warning(paste0("'man' = var and 'pres_dens' = on: \n* Single year in ", site, "_stand.txt."))
    }
    if (use_man) {
      mn <- .read_management(file.path(inputdir, paste0(site, "_management.txt")))
      if (!(pres_dens == "off" && reg == "on")) {
        harv <- mn$harvesting
        thin <- mn$thinning
        # with prescribed densities off only the year_start row of the stand file is used
        last_obs <- if (pres_dens == "on") max(st$Year) else year_start
        if (any(harv %in% year_start:year_end) && rep_missing) {
          stop("\n'man' = var and harvesting year ('Harvesting') is reached: \n* Replanting arguments are missing, with no default.")
        }
        if (length(thin) != 0 && !is.null(mn$thinning_int) && length(thin) != length(mn$thinning_int)) {
          stop("\n'man' = var: \n* Lengths of 'Thinning' and 'Thinning_int' are not the same.")
        }
        if (length(thin) != 0 && is.null(mn$thinning_int)) {
          for (s in species) {
            ti <- sp_par[[s]]["THINNING_INTENSITY"]
            if (is.na(ti) || ti == -9999) {
              stop(paste0("\n'man' = var and 'Thinning' defined in '", site,
                          "_management.txt': \n* Missing 'THINNING_INTENSITY' in '", s, ".txt'."))
            }
          }
        }
        if (any(thin < last_obs)) stop("\n'man' = var: \n* Value(s) of parameter 'Thinning' <  last observed stand density.")
        if (any(harv < last_obs)) stop("\n'man' = var: \n* Value(s) of parameter 'Harvesting' <  last observed stand density.")
        if (any(harv %in% thin)) stop("\n'man' = var: \n* Value(s) of parameter 'Harvesting' = 'Thinning'.")
        actions <- c(harv, thin)
        if (length(actions) != 0 && !any(actions %in% year_start:year_end)) {
          warning("'man' = var: \n* Management actions are outside simulated time frame.")
        }
      }
      if (reg == "on") {
        if (is.null(mn$regeneration)) {
          stop(paste0("\n'reg' = on: \n* Missing 'Regeneration' in '", site, "_management.txt'"))
        }
        if (any(mn$regeneration %in% year_start:year_end) && any(unlist(reg_par) == -9999)) {
          stop("\n'reg' = on and regeneration year ('Regeneration') is reached: \n* Regeneration (saplings) arguments are missing, with no default.")
        }
        if (!all(mn$regeneration %in% year_start:year_end)) {
          warning("'reg' = on: \n* Regeneration starts outside simulated time frame.")
        }
      }
    }
  }
  invisible(TRUE)
}

# with man = var and pres_dens = off the model needs a stand file with a single
# year: if the stand file has more years, only year_start is kept
.stand_file_for_run <- function(site, inputdir, man, pres_dens, year_start) {
  stand <- paste0(site, "_stand.txt")
  if (man == "var" && pres_dens == "off") {
    st <- .read_stand(file.path(inputdir, stand), colClasses = "character")
    if (length(unique(st$Year)) > 1) {
      st1 <- st[as.numeric(st$Year) == year_start, ]
      st1$Management <- "T"
      stand <- paste0(site, "_stand_pres_dens_off.txt")
      utils::write.csv(st1, file.path(inputdir, stand), row.names = FALSE, quote = FALSE)
      warning(paste0("'man' = var and 'pres_dens' = off: \n* Multiple years in ", site,
                     "_stand.txt. \n* Rewritten as ", stand, "."))
    }
  }
  stand
}

# list(PARAM = value) -> settings data.frame
.settings_table <- function(x) {
  values <- vapply(x, function(v) {
    if (is.numeric(v)) format(v, scientific = FALSE, trim = TRUE, digits = 15) else as.character(v)
  }, "")
  data.frame(Parameters = names(x), Values = unname(values))
}

.write_settings <- function(settings, inputdir, site, co2, man, year_start, year_end, output) {
  file <- paste0(site, "_settings_co2_", co2, "_man_", man, "_", year_start, "-", year_end, "_", output, ".txt")
  utils::write.table(settings, file = file.path(inputdir, file), col.names = FALSE, row.names = FALSE, quote = FALSE)
  message("Settings file written: ", file)
  file
}

.run_tag <- function(site, co2, man, pres_dens, reg, year_start, year_end) {
  if (man == "var") {
    paste0(site, "_co2_", co2, "_man_", man, "_pres_dens_", pres_dens, "_reg_", reg, "_", year_start, "-", year_end)
  } else {
    paste0(site, "_co2_", co2, "_man_", man, "_", year_start, "-", year_end)
  }
}

# launch the executable and (optionally) move and rename the outputs
.run_model <- function(exe, inputdir, outputdir, site, stand, settings, use_management,
                       tag, output, move_and_rename, start.time) {

  message("Start 3D-CMCC-FEM ", tag)
  q <- function(x) shQuote(x, type = "cmd")
  args <- c("-i", q(paste0(inputdir, "/")),
            "-p", q(paste0(inputdir, "/")),
            "-o", q("output/"),
            "-d", q(stand),
            "-m", q(paste0(site, "_meteo.txt")),
            "-s", q(paste0(site, "_soil.txt")),
            "-t", q(paste0(site, "_topo.txt")),
            "-c", q(settings),
            "-k", q(paste0(site, "_co2.txt")),
            if (use_management) c("-q", q(paste0(site, "_management.txt"))))
  log_file <- file.path(outputdir, paste0(tag, "_", output, ".txt"))
  launch_time <- Sys.time()

  # the model writes in 'output/' relative to the working directory
  status <- withr::with_dir(dirname(outputdir),
                            system2(exe, args, stdout = log_file, stderr = log_file))
  if (!identical(as.integer(status), 0L)) {
    warning("The model exited with status ", status, ". Check the model log: ", log_file)
  }

  if (move_and_rename) {
    # folder written by the model in this run (output_<version>_<date>)
    dirs <- list.dirs(outputdir, recursive = FALSE)
    dirs <- dirs[grepl("^output_", basename(dirs))]
    recent <- vapply(dirs, function(d) {
      f <- list.files(d, recursive = TRUE, full.names = TRUE)
      length(f) != 0 && max(file.mtime(f)) >= launch_time - 1
    }, logical(1))
    run_dir <- dirs[recent]
    no_output <- function() {
      log <- if (file.exists(log_file)) utils::tail(readLines(log_file, warn = FALSE), 15) else ""
      stop("The model did not produce any output. Last lines of the model log (", log_file, "):\n",
           paste(log, collapse = "\n"),
           "\nNote: the model cannot write files whose full path is longer than 260 characters;",
           " if 'Unable to create ... log' is reported, use a shorter outputdir (now ", nchar(outputdir), " characters).")
    }
    if (length(run_dir) == 0) no_output()
    run_dir <- run_dir[length(run_dir)]

    subdirs <- list.dirs(run_dir, recursive = FALSE)
    subdirs <- subdirs[basename(subdirs) != "input_data"]
    outfile <- list.files(subdirs, pattern = "\\.txt$", full.names = TRUE)
    if (length(outfile) == 0) no_output()

    newdebug <- file.path(outputdir, paste0(output, "_debug.txt"))
    file.copy(log_file, newdebug, overwrite = TRUE)
    unlink(log_file)

    newindir <- file.path(outputdir, "input_data")
    dir.create(newindir, showWarnings = FALSE)
    file.copy(list.files(file.path(run_dir, "input_data"), full.names = TRUE), newindir, overwrite = TRUE)

    # e.g. "annual_5.606_BilyKriz_..." -> "annual_", "annual_man_5.606_..." -> "annual_man_"
    prefix <- sub("\\d.*", "", basename(outfile))
    to <- file.path(outputdir, paste0(prefix, tag, ".txt"))
    file.copy(outfile, to, overwrite = TRUE)
    unlink(run_dir, recursive = TRUE, force = TRUE)
    # main output first, other outputs (e.g. annual_man) after; named by type
    names(to) <- sub("_$", "", prefix)
    to <- to[order(names(to) != output)]
  }

  message("Simulation complete. Running time: ", format(round(difftime(Sys.time(), start.time), 1)))
  invisible(if (move_and_rename) to else NULL)
}
