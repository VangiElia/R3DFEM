#' Run the 3D-CMCC-FEM model (version 5.7)
#'
#' \code{run_3DCMCCFEM} is the wrapper for the 3D-CMCC-FEM model developed by
#' the Italian National Research Council (CNR). The package runs the version
#' 5.7 of the model: the executable is installed once with
#' \code{\link{install_model}} (see \code{\link{model_path}} to use an
#' executable already on disk). For all details on the model please refer to
#' the \href{https://www.forest-modelling-lab.com/the-3d-cmcc-model}{official
#' web page}. For the development version and original code please refer to
#' the \href{https://github.com/Forest-Modelling-Lab/3D-CMCC-FEM}{GitHub
#' repository}
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
#'   Default to "off"
#' @param irrigation Character: Do irrigation be performed? options:
#'   c("on","off"); Default to "off"
#' @param tll Numeric: tree height layer limit, to consider different trees in different layers
#' @param inputdir Character: Path to input files directory
#' @param outputdir Character: Path to output files directory. If it is not
#'   named 'output', a subdirectory 'output' is created.
#' @param check_input Logical: Should the consistency of the arguments and of
#'   the input files be checked (see \code{\link{check_inputs}})? Default to TRUE
#' @param check_meteo Logical: Should the validity of the meteo file be checked
#'   with the function \code{\link{check_meteo_3DFEM}}? Default to FALSE
#' @param move_and_rename Logical: Should the outputs be moved and renamed to be
#'   easily identified? Default to TRUE
#' @param man_type Numeric: 0,1, or 2. 0= %BA removed from each classes during
#'   thinning; 1= Thinning is performed till %BA is removed (should be used in
#'   combination with thinning regime); 2= not used yet.
#' @param thinning_regime Character:"above" or "below".
#' @param rep_trees,rep_age,rep_dbh,rep_lai,rep_height,rep_species,rep_man
#'   Replanting parameters (number of trees, age, average DBH, LAI, height,
#'   species and management). They can be vectors of the same length to
#'   replant more than one species. Default to -9999.
#' @param reg_species Character: species of saplings. Default to -9999, i.e.
#'   the first element of \code{species}.
#' @param annual_man_out Character: Output management in a different output files? c("on","off");
#' @param cold_accl Character: Cold acclimation; options : c("on","off");
#' @param n_reg Character: Nitrogen limitation: c("on","off");
#' @param branch_rem Numeric: percentage of branch removed from the stand.
#'   Default to 100% (all branches removed)
#'
#' @details The function needs at least 6 input .txt files, that must be in
#'   the same directory and should be named according to the name of the site
#'   or the species: "site_stand.txt", "site_soil.txt", "site_topo.txt",
#'   "site_meteo.txt", "site_co2.txt", "species.txt". A "site_management.txt"
#'   file must be provided if required. The site name and species in the input
#'   file name must be the same of the argument \code{site} and
#'   \code{species}. The species files must contain all the parameters of the
#'   version 5.7, including CONV_VOL_FACTOR (see \code{\link{list_species}} for
#'   the parameterizations shipped with the package). The input directory can
#'   be prepared with \code{\link{setup_inputs}}. The settings file is written
#'   in \code{inputdir}. To run many simulations (also in parallel) use
#'   \code{\link{run_batch}}.
#'
#' @return Used for its side effects. It saves in \code{outputdir} the .txt
#'   files with the results of the simulation and returns invisibly their
#'   paths, named by output type with the main output first (e.g.
#'   \code{c(monthly = ..., annual_man = ...)}; \code{NULL} if
#'   \code{move_and_rename = FALSE}). The outputs can be read with
#'   \code{\link{read_output}}. The working directory is not changed. For all
#'   details of the output please refer to the
#'   \href{http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'    Guide}
#' @seealso \code{\link{setup_inputs}}, \code{\link{check_inputs}},
#'   \code{\link{read_output}}, \code{\link{run_batch}}
#' @examples
#' \dontrun{
#' inp <- file.path(tempdir(), "input")
#' dir.create(inp)
#' file.copy(list.files(system.file("extdata", "bilykriz", "rcp0p0", package = "R3DFEM"),
#'                      full.names = TRUE), inp)
#' out <- run_3DCMCCFEM(site = "BilyKriz", species = "Piceaabies",
#'                      year_start = 1997, year_end = 2010, man = "off",
#'                      output = "annual", inputdir = inp,
#'                      outputdir = file.path(tempdir(), "output"))
#' head(read_output(out[1]))
#' }
#' @export

run_3DCMCCFEM <- function(site = NULL,
                          species = NULL,
                          year_start = NULL,
                          year_end = NULL,
                          co2 = "on",
                          man = NULL,
                          man_type=0,
                          thinning_regime="below",
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
                          rep_man="T",
                          reg_species=-9999,
                          reg_trees = -9999,
                          reg_age = -9999,
                          reg_dbh = -9999,
                          reg_lai = -9999,
                          reg_height = -9999,
                          output = NULL,
                          annual_man_out="on",
                          ps_accl = "on",
                          rs_accl = "on",
                          cold_accl="off",
                          n_reg="off",
                          psn = "BGC",
                          co2_conc = 368.865,
                          frac_maxasw = 1,
                          cell = 100,
                          pruning = "off",
                          irrigation = "off",
                          tll=3,
                          branch_rem=100,
                          inputdir = NULL,
                          outputdir = NULL,
                          check_input=TRUE,
                          check_meteo=FALSE,
                          move_and_rename=TRUE
) {

  start.time <- Sys.time()

  # checks ------------------------------------------------------------------

  #mandatory arguments
  stopifnot("Argument 'site' is missing, with no default."=!is.null(site),
            "Argument 'species' is missing, with no default."=!is.null(species),
            "Argument 'year_start' is missing, with no default."=!is.null(year_start),
            "Argument 'year_end' is missing, with no default."=!is.null(year_end),
            "Argument 'man' is missing, with no default."=!is.null(man),
            "Argument 'output' is missing, with no default."=!is.null(output),
            "Argument 'inputdir' is missing, with no default."=!is.null(inputdir),
            "Argument 'outputdir' is missing, with no default."=!is.null(outputdir))

  .check_run_options(co2, man, pres_dens, after_pres, reg, output)

  # replanting of more than one species: all rep_* must have the same length
  n_rep <- length(rep_species)
  if (n_rep > 1) {
    rep_man <- rep_len(rep_man, n_rep)
    stopifnot("rep_trees should be of the same length of rep_species"=length(rep_trees)==n_rep,
              "rep_age should be of the same length of rep_species"=length(rep_age)==n_rep,
              "rep_dbh should be of the same length of rep_species"=length(rep_dbh)==n_rep,
              "rep_lai should be of the same length of rep_species"=length(rep_lai)==n_rep,
              "rep_height should be of the same length of rep_species"=length(rep_height)==n_rep)
  }

  exe <- model_path()
  inputdir <- .normalize_dir(inputdir)
  outputdir <- .prepare_outputdir(outputdir)

  if (check_input) {
    .check_run_inputs(site = site, species = species, year_start = year_start, year_end = year_end,
                      man = man, pres_dens = pres_dens, after_pres = after_pres, reg = reg,
                      year_start_man = year_start_man,
                      rep_par = list(rep_trees, rep_age, rep_dbh, rep_lai, rep_height, rep_species),
                      reg_par = list(reg_trees, reg_age, reg_dbh, reg_lai, reg_height),
                      extra_species = c(rep_species, reg_species), inputdir = inputdir,
                      check_meteo = check_meteo)
  }

  # translate argument ------------------------------------------------------

  psn <- ifelse(psn == "BGC", 0, 1)
  daily <- ifelse(output == "daily", "on", "off")
  monthly <- ifelse(output == "monthly", "on", "off")
  annual <- ifelse(output == "annual", "on", "off")
  if (identical(as.character(reg_species), "-9999")) reg_species <- species[1]

  # ******************************************************************************************************
  # SETTINGS
  # ******************************************************************************************************
  general <- list(
    SITENAME = site,
    VERSION = "f",
    SPATIAL = "u",
    TIME = "d",
    SPINUP = "off",
    SPINUP_YEARS = 6000,
    SCREEN_OUTPUT = "off",
    DEBUG_OUTPUT = "off",
    DAILY_OUTPUT = daily,
    MONTHLY_OUTPUT = monthly,
    ANNUAL_OUTPUT = annual,
    ANNUAL_MAN_OUTPUT = annual_man_out,
    SOIL_OUTPUT = "off",
    NETCDF_OUTPUT = "off",
    YEAR_START = year_start,
    YEAR_END = year_end,
    YEAR_RESTART = "off",
    PSN_mod = psn,
    CO2_trans = co2,
    YEAR_START_CO2_FIXED = -9999,
    Ndep_fixed = "on",
    Photo_accl = ps_accl,
    Resp_accl = rs_accl,
    Cold_accl = cold_accl,
    N_REG = n_reg,
    regeneration = reg,
    management = man,
    management_type = man_type,
    YEAR_START_MANAGEMENT = year_start_man,
    Progn_Aut_Resp = "on",
    SIZECELL = cell,
    Y = 0.48,
    CO2CONC = co2_conc,
    CO2_INCR = 0.01,
    INIT_FRAC_MAXASW = frac_maxasw,
    TREE_LAYER_LIMIT = tll,
    SOIL_LAYER = 1,
    MAX_LAYER_COVER = 1.2,
    THINNING_REGIME = thinning_regime,
    BRANCH_REM = branch_rem)

  replanted <- lapply(seq_len(n_rep), function(i) {
    rp <- list(REPLANTED_SPECIES = rep_species[i],
               REPLANTED_MANAGEMENT = rep_man[i],
               REPLANTED_TREE = rep_trees[i],
               REPLANTED_AGE = rep_age[i],
               REPLANTED_AVDBH = rep_dbh[i],
               REPLANTED_LAI = rep_lai[i],
               REPLANTED_HEIGHT = rep_height[i],
               REPLANTED_WS = 0,
               REPLANTED_WCR = 0,
               REPLANTED_WFR = 0,
               REPLANTED_WL = 0,
               REPLANTED_WBB = 0)
    # with more than one replanted species the parameters are numbered (_0, _1, ...)
    if (n_rep > 1) names(rp) <- paste0(names(rp), "_", i - 1)
    rp
  })

  other <- list(
    REGENERATION_SPECIES = reg_species,
    REGENERATION_MANAGEMENT = "T",
    REGENERATION_N_TREE = reg_trees,
    REGENERATION_AGE = reg_age,
    REGENERATION_AVDBH = reg_dbh,
    REGENERATION_LAI = reg_lai,
    REGENERATION_HEIGHT = reg_height,
    REGENERATION_WS = 0,
    REGENERATION_WCR = 0,
    REGENERATION_WFR = 0,
    REGENERATION_WL = 0,
    REGENERATION_WBB = 0,
    PRUNING = pruning,
    IRRIGATION = irrigation)

  settings <- .settings_table(c(general, unlist(replanted, recursive = FALSE), other))
  settings_file <- .write_settings(settings, inputdir, site, co2, man, year_start, year_end, output)
  stand_file <- .stand_file_for_run(site, inputdir, man, pres_dens, year_start)

  # ******************************************************************************************************
  # RUN MODEL
  # ******************************************************************************************************
  .run_model(exe = exe, inputdir = inputdir, outputdir = outputdir, site = site,
             stand = stand_file, settings = settings_file,
             use_management = .uses_management(man, pres_dens, after_pres, reg),
             tag = .run_tag(site, co2, man, pres_dens, reg, year_start, year_end),
             output = output, move_and_rename = move_and_rename, start.time = start.time)
}
