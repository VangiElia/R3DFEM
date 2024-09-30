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
#'   function. The starting and ending years of simulation are taken from the
#'   meteo file. If the stand age of the virtual stand is not present in
#'   the simulation, a warning is throw and the user need to set the appropriate
#'   replanting parameters (man=on; year_start_man and all rep_* arguments). See
#'   the model specification for the correct use of replanting. This function
#'   can be used to create stand files for forest age classes for which there are
#'   no data.
#'
#' @return Used for its side effects.It save in \code{outdir} a site_stand_age_stand.txt file with
#'   stand charateristics. For all details of the stand file
#'   please refer to the  \href{
#'   http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'    Guide}
#' @seealso \code{run_3DCMCCFEM}
#' @export



virtual_stand_generator <-
  function(site = NULL,
           # Site name; single value (character)
           species = NULL,
           # Species names (same as parameterization file); single value (character)
           stand_age,
           # Set co2 and man for the creation of settings file(s)
           co2 = "on",
           # CO2 concentration changes according to scenario; options: c("on","off","var"); single value (character)
           man = NULL,
           # Management options; options: c("on","off","var"); single value (character)
           pres_dens = NULL,
           # Prescribed densities in stand file if man = "var"; options: c("on","off"); single value (character)
           after_pres = NULL,
           # Management after last year of prescribed density if man = "var"; options: c("on","off"); single value (character)
           reg = "off",
           # Regeneration; options: c("on","off"); single value (character)
           # Set start year of management if man = "on"
           year_start_man = -9999,
           # If man = "on"; Starting year of management; single value (integer)
           # Set replanted trees parameters if harvest is performed
           rep_trees = -9999,
           # Number of replanted trees; single value (integer)
           rep_age = -9999,
           # Age of replanted trees; single value (integer)
           rep_dbh = -9999,
           # Average DBH of replanted trees; single value (numeric)
           rep_lai = -9999,
           # Average LAI of replanted trees; single value (numeric)
           rep_height = -9999,
           # Average height of replanted trees; single value (numeric)
           rep_species = -9999,
           # Species of replanted trees
           # Set regeneration parameters
           reg_trees = -9999,
           # Number of saplings; single value (integer)
           reg_age = -9999,
           # Age of saplings; single value (integer)
           reg_dbh = -9999,
           # Average DBH of saplings; single value (numeric)
           reg_lai = -9999,
           # Average LAI of saplings; single value (numeric)
           reg_height = -9999,
           # Average height of saplings; single value (numeric)
           co2_conc = 368.865,
           # Concentration of CO2 if co2  = "off"; single value (numeric)
           frac_maxasw = 1,
           # Percentage of water content in soil compared to maximum at the beginning of simulation (1 = 100%); single value (numeric)
           cell = 100,
           # Size of the cell; single value (numeric)
           pruning = "off",
           irrigation = "off",
           # Set directories
           inputdir = NULL,
           # Path to input files directory (character)
           outputdir = NULL) {


    meteo_p <- list.files(inputdir,pattern = "meteo",full.names = T)
    co2_p <- list.files(inputdir,pattern = "co2.txt",full.names = T)
    stand <- list.files(inputdir,pattern="stand",full.names = T)
    mt <- read.table(meteo_p, sep = "\t", header = T)
    co <- read.table(co2_p, sep = "\t", header = T)
    st <- read.table(stand,sep = ",",header = T)

    list_age_stand <- st$Age

    if(any(stand_age%in%list_age_stand)){
      message("the stand age ",paste0(stand_age[stand_age%in%list_age_stand],collapse=", ")," are already present in the stand input file and will not be created.")
      stand_age <- stand_age[!stand_age%in%list_age_stand]
    }

    year_start <-  min(mt$Year)
    year_end <- max(mt$Year)
    simulation_length <- year_end-year_start

    age_start <- st$Age[st$Year==year_start]
    final_age <- age_start+simulation_length

    if(any(stand_age>final_age)){
      warning(
        "the stand age ",
        paste0(stand_age[stand_age > final_age], collapse = ", "),
        " are greater than the maximum age reached at the end of simulation (",
        final_age,
        ").\nMake sure to set appropriate replantng parameters to allow extracting a virtual stand younger than the real one.\nCheck parameters: man (should be on), year_start_man, rep_* "
      )
    }

  R3DFEM:::run_3DCMCCFEM(site = site,
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
                        reg_trees =reg_trees,
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
                        move_and_rename = T)



  tmp <- list.files(outputdir,pattern ="annual",full.names = T)
  out <- tmp[!grepl("debug",tmp)]

  out <- read.table(out,header = T,sep=",")
  virtual <- out[out$AGE%in%stand_age,]

  col_name <-
    c(
      "Year",
      "x",
      "y",
      "Age",
      "Species",
      "Management",
      "N",
      "Stool",
      "AvDBH",
      "Height",
      "Wf",
      "Wrc",
      "Wrf",
      "Ws",
      "Wbb",
      "Wres",
      "Lai"
    )
  virtual_stands <-
    data.frame(
      year_start,
      0,
      0,
      virtual$AGE,
      virtual$SPECIES,
      "T",
      virtual$LIVETREE,
      0,
      virtual$DBH,
      virtual$HEIGHT,
      0,
      0,
      0,
      0,
      0,
      0,
      0
    )
  colnames(virtual_stands) <- col_name
  new_outdir <- file.path(outputdir,"virtual_stand/")
  dir.create(new_outdir,showWarnings = F)

  for (i in seq_len(nrow(virtual_stands))) {
    write.table(
      virtual_stands[i,],
      file.path(new_outdir,paste0(site,"_",virtual_stands[i,"Age"] , "_stand.txt")),
      row.names = F,
      col.names = T,
      quote = FALSE,
      sep = ","
    )
  }
  return(invisible())
  }
