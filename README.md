![](https://github.com/VangiElia/R3DFEM/blob/main/readme/LOGO_3DCMCCFEM_color.png)
# R3DFEM: An R package for running the 3D-CMCC-FEM model

<!-- badges: start -->
[![R-CMD-check](https://github.com/VangiElia/R3DFEM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/VangiElia/R3DFEM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

R3DFEM runs the version **5.7** of the [3D-CMCC-FEM](https://www.forest-modelling-lab.com/the-3d-cmcc-model)
forest model from R. It installs the model, prepares and checks the input files, ships the
species parameterizations of the version 5.7, runs single simulations or batches of
simulations (also in parallel) and reads and plots the outputs.
See the [official web page](https://www.forest-modelling-lab.com/the-3d-cmcc-model) for the complete model documentation.

## Installation

```r
# install.packages("remotes")
remotes::install_github("VangiElia/R3DFEM")
library(R3DFEM)

# download the model executable (Windows build of the version 5.7), only once
install_model()
```

The executable is installed in a user directory (`tools::R_user_dir("R3DFEM", "data")`).
The executable available for download runs only on Windows. On Linux and macOS compile the
model from the [source code](https://github.com/Forest-Modelling-Lab/3D-CMCC-FEM), then use
`install_model("path/to/executable")` or `options(R3DFEM.exe = "path/to/executable")`.

The vignettes are [online](https://vangielia.github.io/R3DFEM/articles/). To install them
with the package, knitr and rmarkdown must be installed first (otherwise `R CMD build`
fails right after "checking DESCRIPTION meta-information"):

```r
install.packages(c("knitr", "rmarkdown"))
remotes::install_github("VangiElia/R3DFEM", build_vignettes = TRUE)
```

## Getting started

```r
inp <- system.file("extdata", "bilykriz", "rcp0p0", package = "R3DFEM")
work <- file.path(tempdir(), "bilykriz")
setup_inputs("BilyKriz", work,
             stand = file.path(inp, "BilyKriz_stand.txt"), soil = file.path(inp, "BilyKriz_soil.txt"),
             topo = file.path(inp, "BilyKriz_topo.txt"), meteo = file.path(inp, "BilyKriz_meteo.txt"),
             co2 = file.path(inp, "BilyKriz_co2.txt"))
out <- run_3DCMCCFEM(site = "BilyKriz", species = "Piceaabies", year_start = 1997, year_end = 2050,
                     man = "off", output = "annual", inputdir = work, outputdir = "C:/sim/output")
head(read_output(out[["annual"]]))
plot_output_3DFEM(out[["annual"]], y = c("GPP", "NPP", "DBH"))
```

The vignettes describe the whole workflow and the input files:

- [Getting started](https://vangielia.github.io/R3DFEM/articles/R3DFEM.html) (`vignette("R3DFEM")`): inputs, runs, batches, plots
- [Input files and species parameters](https://vangielia.github.io/R3DFEM/articles/input-files.html) (`vignette("input-files")`)

## Functions

| Task | Functions |
|---|---|
| Install the model | `install_model()`, `model_path()`, `model_available()` |
| Prepare the inputs | `setup_inputs()`, `make_stand()`, `make_meteo()`, `make_co2()`, `make_soil()`, `make_topo()`, `make_management()` |
| Inputs from European datasets | `make_meteo_EOBS()` (E-OBS 30.0 and 29.0), `make_soil_ISRIC()` (SoilGrids) |
| Species parameters (v. 5.7) | `list_species()`, `species_file()`, `copy_species()`, `read_species()`, `set_species_param()` |
| Check the inputs | `check_inputs()`, `check_meteo_3DFEM()` |
| Climate scenarios and virtual stands | `make_CCS()`, `virtual_stand_generator()` |
| Run the model | `run_3DCMCCFEM()`, `run_batch()`, `param_sensitivity()` |
| Outputs | `read_output()`, `plot_output_3DFEM()`, `plot_meteo_3DFEM()`, `plot_soil_3DFEM()` |

The E-OBS netCDF files and the SoilGrids raster are not distributed with the package:
download them and set `options(R3DFEM.eobs_dir = ...)` and `options(R3DFEM.isric_file = ...)`.

The model cannot write files whose full path is longer than 260 characters: use short
output directories.

## Workflow

Below is a schematic representation of the workflow of the package:

![](https://github.com/VangiElia/R3DFEM/blob/main/readme/flowchart_hd300.png)

## I/O
Below, we present a schematic description of the input needed by the functions in the R package: 
For initialization, the 3D-CMCC-FEM requires as input data: 

1. The initial stand conditions: species name (since the model is parameterized at species level), age, mean tree height, diameter at breast height (DBH), and number of trees per size cell. The initial data are aggregated per classes (height classes, cohorts, and species) by a pre-processing activity as follows: (1) the relative values of diameters class is associated for each species, (2) the corresponding value of height class is assigned for each diameter class, and (3) the relative age is assigned for each height class (Collalti et al., 2014; 2024). 
2. Species-specific parameters are primarily based on species-specific eco-physiological and allometric characteristics and can be partially derived from forest inventories and literature (Collalti et al., 2019). The package comes with the parameterizations of the version 5.7 for the most common European tree species (`list_species()`), used in many real case studies across Europe. 
3. Meteorological forcing data: daily maximum (Tmax, °C) and minimum air temperature (Tmin, °C), soil temperature (Tsoil, °C), vapor pressure deficit (hPa), global solar radiation (MJ m−2 day−1) and precipitation amount (mm day−1). 
4. Annual atmospheric CO2 concentration and nitrogen deposition (optional)(Collalti et al., 2018). 
5. Soil and topographic information: soil depth, average sand, clay, silt percentages, and elevation.

All input data need to be written into separate .txt files whose structure is fully described in the [**user manual**](https://www.forest-modelling-lab.com/_files/ugd/8a7700_d31451e9a5e64073b50c07f7f007eb71.pdf). Based on the input files and the argument set, the function wrapping the model (run_3DCMCCFEM) creates a setting file in the input directory used only by the internal C code; the user does not need to interact with the setting file. 

The main output variables of the 3D-CMCC-FEM (either a daily, monthly, or annual scale) are Gross Primary Productivity (GPP), Net Primary Productivity (NPP), and state variables such as evapotranspiration (ET), Leaf Area Index (LAI), and rain interception (to cite some). Results are obtained either at class level (species, diameter, height, or age class level), layer level (as the sum of all tree height classes in the same layer), and grid level (as the sum of all classes in the different layers). The model provides information to support decision-making in forest management planning, such as mean annual volume increment (MAI), current volume increment (CAI), basal area, and DBH. 

For detailed information about 3D-CMCC-FEM and its applications, we strongly encourage you to refer to the literature (Collalti et al., 2014, 2016, 2020, 2024; Marconi et al., 2017; Mahnken et al., 2022; Dalmonech et al., 2022, 2024; Testolin et al., 2023; Vangi et al., 2024a, 2024b; Morichetti et al., 2024), and the main [**web page**](https://www.forest-modelling-lab.com/the-3d-cmcc-model), where the most updated user guide can be found (which include the detailed description of all inputs and outputs, as well as the instruction for launching the model from command line, Eclipse and Bash; Collalti et al., 2022).

## Running simulations

`run_3DCMCCFEM()` wraps the C code of the model: each argument of the model is matched in
the wrapper so that the model can be launched with every possible setting. The function
checks the consistency of the arguments and of the input files (`check_inputs()`), writes
the settings file in the input directory, runs the model and returns the paths of the
outputs, named by type (e.g. `annual`, `annual_man`). Together with the main output
(fluxes and stocks for each time step, species and layer; see the 3D-CMCC-FEM User Guide
par. 4.10), a debug file with the log of the model and a copy of the inputs used are saved
in the output directory, helpful in debugging and sharing.

`run_batch()` runs many simulations (sites, scenarios, management options or species
parameters), each in its own directory, sequentially or in parallel with the
[future](https://future.futureverse.org/) framework.

## Previous versions

R3DFEM 0.2.0 runs only the version 5.7 of the model. `run_3DCMCCFEM_5.7()` and
`make_meteo_EOBS_30.0()` are deprecated aliases of `run_3DCMCCFEM()` and
`make_meteo_EOBS()`. The version 0.1.0, which also runs the version 5.6, can be installed
with `remotes::install_github("VangiElia/R3DFEM@v0.1.0")`.

## Publication

All the pieces of information in this README are taken from the paper [R3DFEM: an R package for running the 3D-CMCC-FEM model](https://link.springer.com/article/10.1007/s12145-025-01956-0).
Have fun and have a nice modeling day!!!
