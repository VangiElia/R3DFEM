% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/virtual_stand_generator.R
\name{virtual_stand_generator}
\alias{virtual_stand_generator}
\title{Create stand file for virtual forest stand based on the 3D-CMCC_FEM model simulation}
\usage{
virtual_stand_generator(
  site = NULL,
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
  outputdir = NULL
)
}
\arguments{
\item{site}{Character: Site name. Used to construct output name}

\item{species}{Character: Species names (same as parameterization files)}

\item{stand_age}{Numeric vector: age of the output virtual stands}

\item{co2}{Character: CO2 concentration changes according to scenario;
options: c("on","off","var")}

\item{man}{Character: Management options; options: c("on","off","var")}

\item{pres_dens}{Character: Prescribed densities in stand file if man =
"var"; options: c("on","off")}

\item{after_pres}{Character: Management after last year of prescribed density
if man = "var"; options: c("on","off")}

\item{reg}{Character: Regeneration; options: c("on","off")}

\item{year_start_man}{Integer: If man = "on"; Starting year of management;
Default to -9999.}

\item{rep_trees}{Integer: Number of replanted trees; Default to -9999.}

\item{rep_age}{Integer: Age of replanted trees; Default to -9999.}

\item{rep_dbh}{Integer: Average DBH of replanted trees; Default to -9999.}

\item{rep_lai}{Integer: Average LAI of replanted trees; Default to -9999.}

\item{rep_height}{Integer: Average height of replanted trees; Default to
-9999.}

\item{rep_species}{Integer: Species of replanted trees}

\item{reg_trees}{Integer: Number of saplings; Default to -9999.}

\item{reg_age}{Integer: Age of saplings; Default to -9999.}

\item{reg_dbh}{Integer: Average DBH of saplings; Default to -9999.}

\item{reg_lai}{Integer: Average LAI of saplings; Default to -9999.}

\item{reg_height}{Integer: Average height of saplings; Default to -9999.}

\item{co2_conc}{Numeric: Concentration of CO2 if co2  = "off"; Default to
368.865}

\item{frac_maxasw}{Numeric: Percentage of water content in soil compared to
maximum at the beginning of simulation (1 = 100\%); Default to 1}

\item{cell}{Numeric: Size of the cell; Default to 100 (1 ha)}

\item{pruning}{Character: Do pruning be performed? options: c("on","off");
Default to "off}

\item{irrigation}{Character: Do irrigation be performed? options:
c("on","off"); Default to "off}

\item{inputdir}{Character: Path to input files directory}

\item{outputdir}{Character: Path to output files directory}
}
\value{
Used for its side effects.It save in \code{outdir} a site_stand_age_stand.txt file with
stand charateristics. For all details of the stand file
please refer to the  \href{
http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
Guide}
}
\description{
\code{virtual_stand_generator} launches the \code{run_3DCMCCFEM} function to
simulate the development of the forest over time. One or more stand files
corresponding to the age classes specified by the user with the
\code{stand_age} parameter are generated from the simulation output.
}
\details{
As the \code{run_3DCMCCFEM} function,  the
\code{virtual_stand_generator} function need at least 6 input .txt files,
that must be in the same directory and should be named according to the
name of the site or the species: "site_stand.txt", "site_soil.txt",
"site_topo.txt", "site_meteo.txt", "site_co2.txt", "species.txt". A
"site_management.txt" file must be provided if required. Usually meteo and
co2 files are de-trended climate data make with the \code{make_CCS}
function. The starting and ending years of simulation are taken from the
meteo file. If the stand age of the virtual stand is not present in
the simulation, a warning is throw and the user need to set the appropriate
replanting parameters (man=on; year_start_man and all rep_* arguments). See
the model specification for the correct use of replanting. This function
can be used to create stand files for forest age classes for which there are
no data.
}
\seealso{
\code{run_3DCMCCFEM}
}
