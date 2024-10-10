#' Wrapper for the 3D-CMCC-FEM model
#'
#' \code{run_3DCMCCFEM} is the wrapper for the 3D-CMCC-FEM model developed by the
#' Italian National Research Council (CNR). Originally written in C, the current
#' version implemented in this package is the 5.6 ISIMIP. For all details on
#' the model please refer to the
#' \href{https://www.forest-modelling-lab.com/the-3d-cmcc-model}{official web
#' page}. For the development version and original code please refer to the
#' \href{https://github.com/Forest-Modelling-Lab/3D-CMCC-FEM}{GitHub repository}
#'
#'
#' @param site Character: Site name. Used to construct output name
#' @param species Character: Species names (same as parameterization files)
#' @param year_start Integer: Starting year of simulation
#' @param year_end Integer: Ending year of simulation
#' @param co2 Character: CO2 concentration changes according to scenario;
#'   options: c("on","off","var")
#' @param man Character: Management options; options: c("on","off","var")
#' @param pres_dens Character: Prescribed densities in stand file if man =
#'   "var"; options: c("on","off")
#' @param after_pres Character: Management after last year of prescribed density
#'   if man = "var"; options: c("on","off")
#' @param reg Character: Regeneration; options: c("on","off")
#' @param year_start_man Integer: If man = "on"; Starting year of management;
#'   Default to -9999.
#' @param rep_trees Integer: Number of replanted trees; Default to -9999.
#' @param rep_age Integer: Age of replanted trees; Default to -9999.
#' @param rep_dbh Integer: Average DBH of replanted trees; Default to -9999.
#' @param rep_lai Integer: Average LAI of replanted trees; Default to -9999.
#' @param rep_height Integer: Average height of replanted trees; Default to
#'   -9999.
#' @param rep_species Integer: Species of replanted trees
#' @param # Set regeneration parameters
#' @param reg_trees Integer: Number of saplings; Default to -9999.
#' @param reg_age Integer: Age of saplings; Default to -9999.
#' @param reg_dbh Integer: Average DBH of saplings; Default to -9999.
#' @param reg_lai Integer: Average LAI of saplings; Default to -9999.
#' @param reg_height Integer: Average height of saplings; Default to -9999.
#' @param output Character: Output type; options: c("daily","monthly","annual");
#' @param ps_accl Character: Photosynthesis acclimation; options : c("on","off")
#' @param rs_accl Character: Respiration acclimation; options : c("on","off");
#' @param psn Character: Photosynthesis version (Farquhar, von Caemmerer & Berry
#'   or Light Use Efficiency); options: c("BGC","LUE")
#' @param co2_conc Numeric: Concentration of CO2 if co2  = "off"; Default to
#'   368.865
#' @param frac_maxasw Numeric: Percentage of water content in soil compared to
#'   maximum at the beginning of simulation (1 = 100%); Default to 1
#' @param cell Numeric: Size of the cell; Default to 100 (1 ha)
#' @param pruning Character: Do pruning be performed? options: c("on","off");
#'   Default to "off
#' @param irrigation Character: Do irrigation be performed? options:
#'   c("on","off"); Default to "off
#' @param inputdir Character: Path to input files directory
#' @param outputdir Character: Path to output files directory
#' @param check_meteo Logical: Should the validity of the meteo file be checked
#'   with the function check_meteo()? Default to FALSE
#' @param move_and_rename Logical: Should the outputs be moved and renamed to be
#'   easily identified? Default to TRUE
#'
#' @details The function need at least 6 input .txt files, that must be in the same
#'   directory and should be named according to the name of the site or the
#'   species: "site_stand.txt", "site_soil.txt", "site_topo.txt",
#'   "site_meteo.txt", "site_co2.txt", "species.txt". A "site_management.txt"
#'   file must be provided if required. The site name and species in the input
#'   file name must be the same of the argument \code{site} and \code{species}.
#'
#' @return Used for its side effects.It save in \code{outdir} a .txt file with
#'   the results of the simulations performed. For all details of the output
#'   please refer to the  \href{
#'   http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'    Guide}
#' @seealso \code{\link{virtual_stand_generator}}
#' @export



# Function to run model (NULL values must be defined)
run_3DCMCCFEM <- function(site = NULL,
                          species = NULL,
                          year_start = NULL,
                          year_end = NULL,
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
                          output = NULL,
                          ps_accl = "on",
                          rs_accl = "on",
                          psn = "BGC",
                          co2_conc = 368.865,
                          frac_maxasw = 1,
                          cell = 100,
                          pruning = "off",
                          irrigation = "off",
                          inputdir = NULL,
                          outputdir = NULL,
                          check_meteo=FALSE,
                          move_and_rename=TRUE
) {

  # Find system month name
  withr::local_locale(c("LC_TIME" = "C"))
  sysmonth <- toupper(months(Sys.Date()))
  # Set start time
  start.time <- Sys.time()

  # modeldir ----------------------------------------------------------------

  modeldir <- system.file("extdata/","Debug",package = "R3DFEM")

  # translate argument ------------------------------------------------------

  psn <- ifelse(psn == "BGC",0,1)
  daily <- ifelse(output %in% "daily","on","off")
  monthly <- ifelse(output %in% "monthly","on","off")
  annual <- ifelse(output %in% "annual","on","off")

  # checks ------------------------------------------------------------------

  #mandatory arguments
  stopifnot("Argument 'site' is missing, with no default."=!is.null(site),
            "Argument 'year_start' is missing, with no default."=!is.null(year_start),
            "Argument 'year_end' is missing, with no default."=!is.null(year_end),
            "Argument 'man' is missing, with no default."=!is.null(man),
            "Argument 'output' is missing, with no default."=!is.null(output),
            "Argument 'inputdir' is missing, with no default."=!is.null(inputdir),
            "Argument 'outputdir' is missing, with no default."=!is.null(outputdir))


  #output directory
  #the source code of the model create the output directory with the default name of 'output'
  #this piece of code ensure that the R wrapper does the same

    if (strsplit(outputdir, "/")[[1]][length(strsplit(outputdir, "/")[[1]])] !=
        "output") {
      message(
        "outputdir should be named 'output'. outputdir will be renamed to meet the model specification"
      )
      outputdir <-file.path(outputdir,"output")

    }
    dir.create(outputdir)


  # If man = on and year_start_man is missing, set it to year_start
  if(man == "on" && year_start_man==-9999) {
    year_start_man <- year_start
    warning("'man' = on: \n- Argument 'year_start_man' is missing. \n- 'year_start_man' = 'year_start'.")
  }
  #regeneration on just if man=var
  # If reg = on and man = on or off, throw error
  if(reg == "on"  && (man == "off" || man == "on")) {
    stop("\n'reg' = on: \n- 'man' must be = var.")
  }
  # If man = var and pres_dens is missing, throw error
  if(man == "var" && is.null(pres_dens)) {
    stop("\n'man' = var: \n- Argument 'pres_dens' is missing, with no default.")
  }
  # If man = var and pres_dens = on and after_pres is missing, throw error
  if(man == "var" && pres_dens == "on" && is.null(after_pres)) {
    stop("\n'man' = var and 'pres_dens' = on: \n- Argument 'after_pres' is missing, with no default.")
  }
  # If man = var and pres_dens = off and after_pres is not defined, throw warning
  if(man == "var" && pres_dens == "off" && !is.null(after_pres)) {
    warning("'man' = var and 'pres_dens' = off: \n- Unused argument 'after_pres'.")
  }
  # If man = var or off and year_start_man is defined, throw warning
  if((man == "var" || man == "off") && year_start_man!=-9999) {
    warning(paste0("'man' = ", man,": \n- Unused argument 'year_start_man'."))
  }


  # input files and consistency check ---------------------------------------

  # create file name according to model specification
  input_files <- list.files(path = inputdir, pattern = ".txt",full.names = T)

  # Input files check, depending on man
  if(man == "off" || man == "on" || (man == "var" && pres_dens == "on" && after_pres == "off" && reg == "off")) {
    files_to_check = c(
      paste0(site, "_stand.txt"),
      paste0(site, "_soil.txt"),
      paste0(site, "_topo.txt"),
      paste0(site, "_co2.txt"),
      paste0(site, "_meteo.txt"),
      paste0(species,".txt"))

    if(rep_species!=-9999) {
      files_to_check = c(files_to_check, paste0(rep_species,".txt"))
    }

    if(any(!files_to_check %in% basename(input_files))) {
      stop(paste("\nMissing input file:", files_to_check[!files_to_check %in% basename(input_files)]))
    }
    rm(files_to_check)
  }

  if((man == "var" && pres_dens == "off") || (man == "var" && pres_dens == "on" && after_pres == "on")) {
    files_to_check = c(
      paste0(site, "_stand.txt"),
      paste0(site, "_soil.txt"),
      paste0(site, "_topo.txt"),
      paste0(site, "_co2.txt"),
      paste0(site, "_meteo.txt"),
      paste0(site, "_management.txt"),
      paste0(species,".txt"))
    if(rep_species!=-9999) {
      files_to_check = c(files_to_check, paste0(rep_species,".txt"))
    }
    #input_files <- list.files(path = inputdir, pattern = ".txt$",recursive = T)
    if(any(!files_to_check %in% basename(input_files))) {
      stop(paste("\nMissing input file:", files_to_check[!files_to_check %in% basename(input_files)]))
    }
    rm(files_to_check)
  }

  # Consistency checks
  # year_start must be in stand file
  #read stand file
  st <- read.table(file=input_files[grep("stand.txt",input_files)], sep = ",", header = T)

  if(!any(st$Year%in%year_start)) {
    stop(paste0("\nValue of argument 'year_start' not in '", site, "_stand.txt'."))
  }
  # years between year_start and year_end must be in co2 and meteo files
  mt <- read.table(input_files[grep("meteo.txt",input_files)], sep = "\t", header = T)
  co <- read.table(input_files[grep("co2.txt",input_files)], sep = "\t", header = T)
  y <- seq(year_start, year_end)
  if(!all(y %in% mt$Year) || !all(y %in% co$year)) {
    stop(paste0("\nMissing simulated time span from '", site, "_meteo.txt' and/or '", site, "_co2.txt'."))
  }
  rm(mt, co, y)

  if(check_meteo){
    R3DFEM::check_meteo_3DFEM(file_meteo=input_files[grep("meteo.txt",input_files)],correct=FALSE)
  }

  #read species parametrization, used to check input for man
  sp <- read.table(text =str_squish(gsub("//.*","",readLines(paste0(inputdir, "/", species, ".txt")))),sep=" ")
  #sp <- read.table(paste0(inputdir, "/", species, ".txt"), sep = " ", header = FALSE, stringsAsFactors = FALSE,fill=T,comment.char = "/")
  sp <- as.data.frame(do.call(rbind,apply(sp,1,function(x){
    ind <- !is.na(x)
    x[ind]
  },simplify = FALSE)))
  sp <- sp[rowSums(sp=="")!=ncol(sp), ]
  sp[,2] <- as.numeric(sp[,2])
  colnames(sp)[2] <- "value"


  # man = on checks
  if(man == "on") {
    # year_start_man cannot be before year_start
    if(year_start_man < year_start) {
      stop("\n'man' = on: \n* Value of argument 'year_start_man' < 'year_start'.")
    }
    # management parameters in species file must be defined
    if (sp[sp$V1 == "ROTATION", "value"]==-9999 ||
        sp[sp$V1 == "THINNING", "value"]==-9999 ||
        sp[sp$V1 == "THINNING_INTENSITY", "value"]==-9999) {
      stop(paste0("\n'man' = on: \n* Missing management parameter(s) in '", species, ".txt'."))
    }
    # year_start_man cannot exceed years of thinning interval from year_start
    if(year_start_man - year_start > sp[sp$V1 == "THINNING", "value"]) {
      stop(paste0("\n'man' = on: \n* Value of 'year_start_man' > thinning interval from 'year_start'. \n* Parameter 'THINNING' in '", species, ".txt' = ", sp[sp$V1 == "THINNING", "value"],"."))
    }
    # If harvest happens within the simulation time frame (accounting for stand age), replanted trees parameters must be defined
    if (year_start + sp[sp$V1 == "ROTATION", "value"] - st[st$Year == year_start, "Age"] < year_end &&
        (
          rep_trees  ==-9999||
          rep_age    ==-9999||
          rep_dbh    ==-9999||
          rep_lai    ==-9999||
          rep_height ==-9999||
          rep_species==-9999
        )) {

      stop(paste0("\n'man' = on and harvesting year ('ROTATION') is reached: \n* Replanting arguments are missing, with no default."))
    }
  }


  # man = var checks
  if(man == "var") {

    if((man == "var" && pres_dens == "off") || (man == "var" && pres_dens == "on" && after_pres == "on")) {

    mn <- read.delim(paste0(inputdir, "/", site, "_management.txt"), sep = ",", header = FALSE, stringsAsFactors = FALSE)
    mn <- as.data.frame(t(mn))
    col.names <- mn[1,]
    mn <- as.data.frame(mn[-1,])
    mn <- as.data.frame(apply(mn, MARGIN = 2, FUN = as.numeric, simplify = FALSE))
    colnames(mn) <- col.names
    rownames(mn) <- NULL
    }
    # Check consistency between stand file and pres_dens argument
    if(pres_dens == "off" && nrow(st) > 1) {
      st1 <- st[st$Year == year_start,]
      st1$Management <- "T"
      write.csv(st1, paste0(inputdir, "/", site, "_stand_pres_dens_off.txt"), row.names = FALSE, quote = FALSE)
      warning(paste0("'man' = var and 'pres_dens' = off: \n* Multiple years in ", site, "_stand.txt. \n* Rewritten as ", site, "_stand_pres_dens_off.txt."))
      rm(st1)
      #stop(paste0("\n'man' = var and 'pres_dens' = off: \n* Multiple years in ", site, "_stand.txt."))
    }
    if(pres_dens == "on" && nrow(st) == 1) {
      warning(paste0("'man' = var and 'pres_dens' = on: \n* Single year in ", site, "_stand.txt."))
    }
    # Check management file when present
    if(pres_dens == "off" || (pres_dens == "on" && after_pres == "on") || (man == "var" && pres_dens == "on" && after_pres == "off" && reg == "on")) {
      # If harvesting is defined in management file and is within simulation years, replanted trees parameters must be defined
      if(any(colnames(mn) == "Harvesting") && any(mn$Harvesting %in% year_start:year_end) && (rep_trees==-9999  || rep_age==-9999 || rep_dbh==-9999 || rep_lai==-9999 || rep_height==-9999|| rep_species==-9999)) {
        stop(paste0("\n'man' = var and harvesting year ('Harvesting') is reached: \n* Replanting arguments are missing, with no default."))
      }
      # If both thinning years and intensity are provided, they must have the same length
      if(any(colnames(mn) == "Thinning") && any(colnames(mn) == "Thinning_int") && length(mn$Thinning[!is.na(mn$Thinning)]) != length(mn$Thinning_int[!is.na(mn$Thinning_int)])) {
        stop("\n'man' = var: \n* Lengths of 'Thinning' and 'Thinning_int' are not the same.")
      }
      # If thinning years are provided but thinning intensities are not, the THINNING_INTENSITY parameter must be defined in the species file
      if(any(colnames(mn) == "Thinning") && all(colnames(mn) != "Thinning_int") && (sp[sp$V1 == "THINNING_INTENSITY", "value"]==-9999 || is.na(sp[sp$V1 == "THINNING_INTENSITY", "value"]))) {
        stop(paste0("\n'man' = var and 'Thinning' defined in '", site, "_management.txt': \n* Missing 'THINNING_INTENSITY' in '", species, ".txt.'"))
      }
      # If thinning years or harvesting years precede last year in stand file, throw error
      if(any(colnames(mn) == "Thinning") && any(mn$Thinning[!is.na(mn$Thinning)] < tail(st$Year,1))) {
        stop("\n'man' = var: \n* Value(s) of parameter 'Thinning' <  last observed stand density.")
      }
      if(any(colnames(mn) == "Harvesting") && any(mn$Harvesting[!is.na(mn$Harvesting)] < tail(st$Year,1))) {
        stop("\n'man' = var: \n* Value(s) of parameter 'Harvesting' <  last observed stand density.")
      }
      # If harvesting and thinning are performed on the same year, throw error
      if(any(colnames(mn) == "Harvesting") && any(colnames(mn) == "Thinning") && any(mn$Harvesting[!is.na(mn$Harvesting)] %in% mn$Thinning[!is.na(mn$Thinning)])) {
        stop("\n'man' = var: \n* Value(s) of parameter 'Harvesting' = 'Thinning'.")
      }
      # If harvesting or thinning years are not included in simulation, throw warning
      if((any(colnames(mn) == "Harvesting") && !any(mn$Harvesting[!is.na(mn$Harvesting)] %in% year_start:year_end)) && (any(colnames(mn) == "Thinning")  && !any(mn$Thinning[!is.na(mn$Thinning)]  %in% year_start:year_end))) {
        warning("'man' = var: \n* Management actions are outside simulated time frame.")
      }
    }
    if(pres_dens == "on" && after_pres == "off" && reg == "off" && nrow(st) == 1) {
      warning("'man' = var and 'pres_dens' = on: \n* Single density in stand file.")
    }
    if(reg == "on") {
      # If regeneration is not defined in management file, throw error
      if(!any(colnames(mn) == "regeneration")) {
        stop(paste0("\n'reg' = on: \n* Missing 'regeneration' in '", site, "_management.txt'"))
      }
      mn$regeneration <- as.numeric(mn$regeneration)
      # If regeneration is defined in management file and is within simulation years, saplings parameters must be defined
      if(any(mn$regeneration %in% year_start:year_end) && (reg_trees==-9999 ||reg_age==-9999 || reg_dbh==-9999 || reg_lai==-9999 || reg_height==-9999)) {
        stop(paste0("\n'reg' = on and regeneration year ('regeneration') is reached: \n* Regeneration (saplings) arguments are missing, with no default."))
      }
      # If regeneration is not included in simulation, throw warning
      if(!all(mn$regeneration[!is.na(mn$regeneration)] %in% year_start:year_end)) {
        warning("'reg' = on: \n* Regeneration starts outside simulated time frame.")
      }
    }
  }

  # ******************************************************************************************************
  # SETTINGS
  # ******************************************************************************************************
  # Create settings file(s)
  options(stringsAsFactors = FALSE)
  settings <- rbind(data.frame("Parameters" = "SITENAME", "Values" = site),
                    data.frame("Parameters" = "VERSION", "Values" =  "f"),
                    data.frame("Parameters" = "SPATIAL", "Values" = "u"),
                    data.frame("Parameters" = "TIME", "Values" = "d"),
                    data.frame("Parameters" = "SPINUP", "Values" = "off"),
                    data.frame("Parameters" = "SPINUP_YEARS", "Values" = 6000),
                    data.frame("Parameters" = "SCREEN_OUTPUT", "Values" = "off"),
                    data.frame("Parameters" = "DEBUG_OUTPUT", "Values" = "off"),
                    data.frame("Parameters" = "DAILY_OUTPUT", "Values" = daily),
                    data.frame("Parameters" = "MONTHLY_OUTPUT", "Values" = monthly),
                    data.frame("Parameters" = "ANNUAL_OUTPUT", "Values" = annual),
                    data.frame("Parameters" = "SOIL_OUTPUT", "Values" = "off"),
                    data.frame("Parameters" = "NETCDF_OUTPUT", "Values" = "off"),
                    data.frame("Parameters" = "YEAR_START", "Values" = year_start),
                    data.frame("Parameters" = "YEAR_END", "Values" = year_end),
                    data.frame("Parameters" = "YEAR_RESTART", "Values" = "off"),
                    data.frame("Parameters" = "PSN_mod", "Values" = psn),
                    data.frame("Parameters" = "CO2_trans", "Values" = co2),
                    data.frame("Parameters" = "YEAR_START_CO2_FIXED", "Values" = -9999),
                    data.frame("Parameters" = "Ndep_fixed", "Values" = "on"),
                    data.frame("Parameters" = "Photo_accl", "Values" = ps_accl),
                    data.frame("Parameters" = "Resp_accl", "Values" = rs_accl),
                    data.frame("Parameters" = "regeneration", "Values" = reg),
                    data.frame("Parameters" = "management", "Values" = man),
                    data.frame("Parameters" = "YEAR_START_MANAGEMENT", "Values" = year_start_man),
                    data.frame("Parameters" = "Progn_Aut_Resp", "Values" = "on"),
                    data.frame("Parameters" = "SIZECELL", "Values" = cell),
                    data.frame("Parameters" = "Y", "Values" = 0.48),
                    data.frame("Parameters" = "CO2CONC", "Values" = co2_conc),
                    data.frame("Parameters" = "CO2_INCR", "Values" = 0.01),
                    data.frame("Parameters" = "INIT_FRAC_MAXASW", "Values" = frac_maxasw),
                    data.frame("Parameters" = "TREE_LAYER_LIMIT", "Values" = 3),
                    data.frame("Parameters" = "SOIL_LAYER", "Values" = 1),
                    data.frame("Parameters" = "MAX_LAYER_COVER", "Values" = 1.2),
                    data.frame("Parameters" = "THINNING_REGIME", "Values" =  "Above"),
                    data.frame("Parameters" = "REPLANTED_SPECIES", "Values" = rep_species),
                    data.frame("Parameters" = "REPLANTED_MANAGEMENT", "Values" = "T"),
                    data.frame("Parameters" = "REPLANTED_TREE", "Values" = rep_trees),
                    data.frame("Parameters" = "REPLANTED_AGE", "Values" = rep_age),
                    data.frame("Parameters" = "REPLANTED_AVDBH", "Values" = rep_dbh),
                    data.frame("Parameters" = "REPLANTED_LAI", "Values" = rep_lai),
                    data.frame("Parameters" = "REPLANTED_HEIGHT", "Values" = rep_height),
                    data.frame("Parameters" = "REPLANTED_WS", "Values" = 0),
                    data.frame("Parameters" = "REPLANTED_WCR", "Values" = 0),
                    data.frame("Parameters" = "REPLANTED_WFR", "Values" = 0),
                    data.frame("Parameters" = "REPLANTED_WL", "Values" = 0),
                    data.frame("Parameters" = "REPLANTED_WBB", "Values" = 0),
                    data.frame("Parameters" = "REGENERATION_SPECIES", "Values" = species),
                    data.frame("Parameters" = "REGENERATION_MANAGEMENT", "Values" = "T"),
                    data.frame("Parameters" = "REGENERATION_N_TREE", "Values" = reg_trees),
                    data.frame("Parameters" = "REGENERATION_AGE", "Values" = reg_age),
                    data.frame("Parameters" = "REGENERATION_AVDBH", "Values" = reg_dbh),
                    data.frame("Parameters" = "REGENERATION_LAI", "Values" = reg_lai),
                    data.frame("Parameters" = "REGENERATION_HEIGHT", "Values" = reg_height),
                    data.frame("Parameters" = "REGENERATION_WS", "Values" = 0),
                    data.frame("Parameters" = "REGENERATION_WCR", "Values" = 0),
                    data.frame("Parameters" = "REGENERATION_WFR", "Values" = 0),
                    data.frame("Parameters" = "REGENERATION_WL", "Values" = 0),
                    data.frame("Parameters" = "REGENERATION_WBB", "Values" = 0),
                    data.frame("Parameters" = "PRUNING", "Values" = pruning),
                    data.frame("Parameters" = "IRRIGATION", "Values" = irrigation))

  # Write settings files
  write.table(settings, file = paste0(inputdir, "/", site, "_settings_co2_", co2, "_man_", man, "_", year_start, "-", year_end, "_", output, ".txt"), col.names = FALSE, row.names = FALSE, quote = FALSE)
  cat("Written settings file!\n")

  # ******************************************************************************************************
  # RUN MODEL
  # ******************************************************************************************************
  # Print model run
  if(man == "off" || man == "on") {
    cat(paste0("\nStart 3D-CMCC-FEM site:", site, " co2:", co2, " man:", man, " start:", year_start, " end:", year_end, "\n"))
  }
  if(man == "var" && pres_dens == "off") {
    cat(paste0("\nStart 3D-CMCC-FEM site:", site, " co2:", co2, " man:", man, " pres_dens:", pres_dens, " reg:", reg, " start:", year_start, " end:", year_end, "\n"))
  }
  if(man == "var" && pres_dens == "on") {
    cat(paste0("\nStart 3D-CMCC-FEM site:", site, " co2:", co2, " man:", man, " pres_dens:", pres_dens, " after_pres:", after_pres, " reg:", reg, " start:", year_start, " end:", year_end, "\n"))
  }

  # Prepare input files based on function arguments and input files
  if(man == "off" || man == "on" || (man == "var" && pres_dens == "on" && after_pres == "off" && reg == "off")) {
    systemCall  <- paste0('\"', modeldir, '/3D_CMCC_Forest_Model\" ',
                          ' -i \"', inputdir, '/" ',
                          ' -p \"', inputdir, '/\"',
                          ' -o \"output/\"',
                          ' -d \"', site, '_stand.txt\"',
                          ' -m \"', site, '_meteo.txt\"',
                          ' -s \"', site, '_soil.txt\"',
                          ' -t \"', site, '_topo.txt\"',
                          ' -c \"', site, '_settings_co2_', "on", '_man_', man, '_', year_start, '-', year_end, '_', output, '.txt\"',
                          ' -k \"', site, '_co2.txt\"',
                          '>\"', outputdir, "/", site, '_co2_', "on", '_man_', man, '_', year_start, '-', year_end, '_', output, '.txt\"'
    )
  }

  if((man == "var" && pres_dens == "on" && after_pres == "on") || (man == "var" && pres_dens == "on" && after_pres == "off" && reg == "on")) {
    systemCall  <- paste0('\"', modeldir, '/3D_CMCC_Forest_Model\" ',
                          ' -i \"', inputdir, '/" ',
                          ' -p \"', inputdir, '/"',
                          ' -o \"output/\"',
                          ' -d \"', site, '_stand.txt\"',
                          ' -m \"', site, '_meteo.txt\"',
                          ' -s \"', site, '_soil.txt\"',
                          ' -t \"', site, '_topo.txt\"',
                          ' -c \"', site, '_settings_co2_', co2, '_man_', man, '_', year_start, '-', year_end, '_', output, '.txt\"',
                          ' -k \"', site, '_co2.txt\"',
                          ' -q \"', site, '_management.txt\"',
                          '>\"', outputdir, "/", site, '_co2_', co2, '_man_', man, '_', year_start, '-', year_end, '_', output, '.txt\"'
    )
  }

  if((man == "var" && pres_dens == "off")) {
    st <- read.table(paste0(inputdir, "/", site, "_stand.txt"), sep = ",", header = T)
    if(nrow(st) == 1) {
      systemCall  <- paste0('\"', modeldir, '/3D_CMCC_Forest_Model\" ',
                            ' -i \"', inputdir, '/" ',
                            ' -p \"', inputdir, '/"',
                            ' -o \"output/\"',
                            ' -d \"', site, '_stand.txt\"',
                            ' -m \"', site, '_meteo.txt\"',
                            ' -s \"', site, '_soil.txt\"',
                            ' -t \"', site, '_topo.txt\"',
                            ' -c \"', site, '_settings_co2_', co2, '_man_', man, '_', year_start, '-', year_end, '_', output, '.txt\"',
                            ' -k \"', site, '_co2.txt\"',
                            ' -q \"', site, '_management.txt\"',
                            '>\"', outputdir, site, '_co2_', co2, '_man_', man, '_', year_start, '-', year_end, '_', output, '.txt\"'
      )
    } else {
      systemCall  <- paste0('\"', modeldir, '/3D_CMCC_Forest_Model\" ',
                            ' -i \"', inputdir, '/" ',
                            ' -p \"', inputdir, '/"',
                            ' -o \"output/\"',
                            ' -d \"', site, '_stand_pres_dens_off.txt\"',
                            ' -m \"', site, '_meteo.txt\"',
                            ' -s \"', site, '_soil.txt\"',
                            ' -t \"', site, '_topo.txt\"',
                            ' -c \"', site, '_settings_co2_', co2, '_man_', man, '_', year_start, '-', year_end, '_', output, '.txt\"',
                            ' -k \"', site, '_co2.txt\"',
                            ' -q \"', site, '_management.txt\"',
                            '>\"', outputdir, site, '_co2_', co2, '_man_', man, '_', year_start, '-', year_end, '_', output, '.txt\"'
      )
    }
    rm(st)
  }

  # Launch execution
  setwd(paste(strsplit(outputdir,"/")[[1]][-length(strsplit(outputdir,"/")[[1]])],collapse = "/"))
  fid <- file('launch.bat','w')
  writeLines(systemCall,fid)
  close(fid)
  rm(fid)
  system('launch.bat')
  file.remove('launch.bat')


# ouput file naming --------------------------------------------------------

  if(move_and_rename){

  #rename debug file
  debug <- list.files(outputdir,recursive = F,full.names = T)
  tmpout <- debug[grepl("output_5.6",basename(debug),fixed=T)]
  debug <- debug[grep(paste0("^",site),basename(debug))]
  newdebug <- file.path(outputdir,paste0(output,"_debug.txt"))
  file.rename(from=debug,
              to=newdebug)
  infile <- list.files(file.path(tmpout,"input_data"),full.names = T,pattern = "txt")
  newindir <- file.path(outputdir,"input_data")
  if(!dir.exists(newindir)){
    dir.create(newindir,showWarnings = F)
    to <- file.path(newindir,basename(infile))
    file.rename(from=infile,
                to=to)
  }
  outfold <- list.dirs(outputdir,recursive = T)

  if(output == "annual") {
     outfold <- outfold[grepl("annual",basename(outfold),fixed=T)]
     outfile <- list.files(outfold,full.names = T,pattern = "txt")
    if(man == "off" || man == "on") {
      file.rename(from = outfile,
                  to = file.path(outputdir, paste0("annual_",site, "_co2_", co2, "_man_", man, "_", year_start, "-", year_end, ".txt")))
    }
    if(man == "var") {
      file.rename(from = outfile,
                  to = file.path(outputdir, paste0("annual_",site, "_co2_", co2, "_man_", man, "_pres_dens_", pres_dens, "_reg_", reg, "_", year_start, "-", year_end, ".txt")))
    }
  }
  if(output == "monthly") {
       outfold <- outfold[grepl("monthly",basename(outfold),fixed=T)]
       outfile <- list.files(outfold,full.names = T,pattern = "txt")
    if(man == "off" || man == "on") {
      file.rename(from = outfile,
                  to = file.path(outputdir, paste0("monthly_",site, "_co2_", co2, "_man_", man, "_", year_start, "-", year_end, ".txt")))
    }
    if(man == "var") {
      file.rename(from = outfile,
                  to = file.path(outputdir, paste0("monthly_",site, "_co2_", co2, "_man_", man, "_pres_dens_", pres_dens, "_reg_", reg, "_", year_start, "-", year_end, ".txt")))
    }
  }
  if(output == "daily") {
    outfold <- outfold[grepl("daily",basename(outfold),fixed=T)]
    outfile <- list.files(outfold,full.names = T,pattern = "txt")
    if(man == "off" || man == "on") {
      file.rename(from =outfile,
                  to = file.path(outputdir, paste0("daily_",site, "_co2_", co2, "_man_", man, "_", year_start, "-", year_end, ".txt")))
    }
    if(man == "var") {
      file.rename(from =outfile,
                  to = file.path(outputdir, paste0("daily_",site, "_co2_", co2, "_man_", man, "_pres_dens_", pres_dens, "_reg_", reg, "_", year_start, "-", year_end, ".txt")))
    }
  }

  unlink(tmpout,recursive = T,force=T)

  }
  cat(paste0("\nSimulation complete!\n"))
  end.time <- Sys.time()
  cat(paste0("\nRunning time: ", round(end.time - start.time, 1), " s\n"))
}
