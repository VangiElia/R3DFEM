#' Create stand file for virtual forest stand based on the 3D-CMCC_FEM model simulation
#'
#' \code{virtual_stand_generator} launches the \code{run_3DCMCCFEM} function to
#' simulate the development of the forest over time. One or more stand files
#' corresponding to the age classes specified by the user with the
#' \code{stand_age} parameter are generated from the simulation output.
#'
#' @inheritParams run_3DCMCCFEM
#' @param stand_age Numeric vector: age of the output virtual stands
#'
#' @details As the \code{run_3DCMCCFEM} function,  the
#'   \code{virtual_stand_generator} function need at least 6 input .txt files,
#'   that must be in the same directory and should be named according to the
#'   name of the site or the species: "site_stand.txt", "site_soil.txt",
#'   "site_topo.txt", "site_meteo.txt", "site_co2.txt", "species.txt". A
#'   "site_management.txt" file must be provided if required. Usually meteo and
#'   co2 files are de-trended climate data make with the \code{make_CCS}
#'   function. The simulation starts from the first year of the stand file
#'   and ends with the last year of the meteo file. If the stand age of the virtual stand is not present in
#'   the simulation, a warning is throw and the user need to set the appropriate
#'   replanting parameters (man=on; year_start_man and all rep_* arguments). See
#'   the model specification for the correct use of replanting. This function
#'   can be used to create stand files for forest age classes for which there are
#'   no data.
#'
#' @return Used for its side effects. It saves in \code{outputdir/virtual_stand} a site_age_stand.txt file with
#'   stand charateristics and returns invisibly the virtual stands. For all details of the stand file
#'   please refer to the  \href{http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'    Guide}
#' @seealso \code{run_3DCMCCFEM}
#' @export




virtual_stand_generator <-
  function(site = NULL,
           species = NULL,
           stand_age,
           co2 = "on",
           man = NULL,
           pres_dens = NULL,
           after_pres = NULL,
           reg = "off",
           year_start_man = -9999,
           rep_trees = -9999,
           rep_age = -9999,
           rep_dbh = -9999,
           rep_lai = -9999,
           rep_height = -9999,
           rep_species = -9999,
           reg_trees = -9999,
           reg_age = -9999,
           reg_dbh = -9999,
           reg_lai = -9999,
           reg_height = -9999,
           co2_conc = 368.865,
           frac_maxasw = 1,
           cell = 100,
           pruning = "off",
           irrigation = "off",
           inputdir = NULL,
           outputdir = NULL) {

    stopifnot("Argument 'site' is missing, with no default." = !is.null(site),
              "Argument 'inputdir' is missing, with no default." = !is.null(inputdir),
              "Argument 'outputdir' is missing, with no default." = !is.null(outputdir))

    mt_year <- data.table::fread(file.path(inputdir, paste0(site, "_meteo.txt")), select = 1)[[1]]
    st <- .read_stand(file.path(inputdir, paste0(site, "_stand.txt")))

    if(any(stand_age %in% st$Age)){
      message("the stand age ",paste0(stand_age[stand_age %in% st$Age],collapse=", ")," are already present in the stand input file and will not be created.")
      stand_age <- stand_age[!stand_age %in% st$Age]
    }
    if (length(stand_age) == 0) stop("No virtual stand to create")

    year_start <- min(st$Year)
    year_end <- max(mt_year)
    if (!year_start %in% mt_year) stop("The first year of the stand file (", year_start, ") is not in the meteo file")

    age_start <- max(st$Age[st$Year == year_start])
    final_age <- age_start + year_end - year_start

    if(any(stand_age>final_age)){
      warning(
        "the stand age ",
        paste0(stand_age[stand_age > final_age], collapse = ", "),
        " are greater than the maximum age reached at the end of simulation (",
        final_age,
        ").\nMake sure to set appropriate replantng parameters to allow extracting a virtual stand younger than the real one.\nCheck parameters: man (should be on), year_start_man, rep_* "
      )
    }

    out_files <- run_3DCMCCFEM(site = site,
                               species = species,
                               year_start = year_start,
                               year_end = year_end,
                               co2 = co2,
                               man = man,
                               pres_dens = pres_dens,
                               after_pres = after_pres,
                               reg = reg,
                               year_start_man = year_start_man,
                               rep_trees = rep_trees,
                               rep_age = rep_age,
                               rep_dbh = rep_dbh,
                               rep_lai = rep_lai,
                               rep_height = rep_height,
                               rep_species = rep_species,
                               reg_trees = reg_trees,
                               reg_age = reg_age,
                               reg_dbh = reg_dbh,
                               reg_lai = reg_lai,
                               reg_height = reg_height,
                               output = "annual",
                               ps_accl = "on",
                               rs_accl = "on",
                               psn = "BGC",
                               co2_conc = co2_conc,
                               frac_maxasw = frac_maxasw,
                               cell = cell,
                               pruning = pruning,
                               irrigation = irrigation,
                               inputdir = inputdir,
                               outputdir = outputdir,
                               move_and_rename = TRUE)

    out <- utils::read.table(out_files[grepl("^annual_[^m]", basename(out_files))][1], header = TRUE, sep = ",")
    # the species column is SPECIE in 5.6 and SPECIES in 5.7 outputs
    colnames(out)[grepl("^SPECIE", colnames(out))] <- "SPECIE"
    virtual <- out[out$AGE %in% stand_age, ]

    virtual_stands <-
      data.frame(
        Year = year_start,
        x = 0,
        y = 0,
        Age = virtual$AGE,
        Species = virtual$SPECIE,
        Management = "T",
        N = virtual$LIVETREE,
        Stool = 0,
        AvDBH = virtual$DBH,
        Height = virtual$HEIGHT,
        Wf = 0,
        Wrc = 0,
        Wrf = 0,
        Ws = 0,
        Wbb = 0,
        Wres = 0,
        Lai = 0
      )
    new_outdir <- file.path(dirname(out_files[1]), "virtual_stand")
    dir.create(new_outdir, showWarnings = FALSE)

    # one file per age (with more classes or species the file has more rows)
    for (a in unique(virtual_stands$Age)) {
      utils::write.table(
        virtual_stands[virtual_stands$Age == a, ],
        file.path(new_outdir, paste0(site, "_", a, "_stand.txt")),
        row.names = F,
        col.names = T,
        quote = FALSE,
        sep = ","
      )
    }
    invisible(virtual_stands)
  }

