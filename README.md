![](https://github.com/VangiElia/R3DFEM/blob/main/readme/LOGO_3DCMCCFEM_color.png)
# R3DFEM: An R package for running the 3D-CMCC-FEM model
 Due to storage capacity, some data cannot be uploaded to Github, causing the functions make_meteo_EOBS and make_soil_ISRIC not to work.
 
 The R3DFEM package provides functions for creating and checking inputs for
 simulations, running the 3D-CMCC-FEM model, plotting inputs and outputs, and other
 functionalities. See the package readme for an overview of the package
 functionality.
 See the [official web page](https://www.forest-modelling-lab.com/the-3d-cmcc-model) for the complete model documentation.
 
 ## Getting started

```{r eval=FALSE}
#install.packages("devtools")
devtools::install_github("VangiElia/R3DFEM")
#loading R3DFEM package
library(R3DFEM)
```
## Workflow

Below is a schematic representation of the workflow of the package:

![](https://github.com/VangiElia/R3DFEM/blob/main/readme/flowchart_hd300.png)

## I/O
Below, we present a schematic description of the input needed by the functions in the R package: 
For initialization, the 3D-CMCC-FEM requires as input data: 

1. The initial stand conditions: species name (since the model is parameterized at species level), age, mean tree height, diameter at breast height (DBH), and number of trees per size cell. The initial data are aggregated per classes (height classes, cohorts, and species) by a pre-processing activity as follows: (1) the relative values of diameters class is associated for each species, (2) the corresponding value of height class is assigned for each diameter class, and (3) the relative age is assigned for each height class (Collalti et al., 2014; 2024). 
2. Species-specific parameters are primarily based on species-specific eco-physiological and allometric characteristics and can be partially derived from forest inventories and literature (Collalti et al., 2019). The package comes with a suite of already parameterized files for different and most common European tree species used in many real case studies across Europe. 
3. Meteorological forcing data: daily maximum (Tmax, °C) and minimum air temperature (Tmin, °C), soil temperature (Tsoil, °C), vapor pressure deficit (hPa), global solar radiation (MJ m−2 day−1) and precipitation amount (mm day−1). 
4. Annual atmospheric CO2 concentration and nitrogen deposition (optional)(Collalti et al., 2018). 
5. Soil and topographic information: soil depth, average sand, clay, silt percentages, and elevation.

All input data need to be written into separate .txt files whose structure is fully described in the [**user manual**](https://www.forest-modelling-lab.com/_files/ugd/8a7700_d31451e9a5e64073b50c07f7f007eb71.pdf). Based on the input files and the argument set, the function wrapping the model (run_3DFEM) creates a setting file in the output directory used only by the internal C code; the user does not need to interact with the setting file. 

The main output variables of the 3D-CMCC-FEM (either a daily, monthly, or annual scale) are Gross Primary Productivity (GPP), Net Primary Productivity (NPP), and state variables such as evapotranspiration (ET), Leaf Area Index (LAI), and rain interception (to cite some). Results are obtained either at class level (species, diameter, height, or age class level), layer level (as the sum of all tree height classes in the same layer), and grid level (as the sum of all classes in the different layers). The model provides information to support decision-making in forest management planning, such as mean annual volume increment (MAI), current volume increment (CAI), basal area, and DBH. 

For detailed information about 3D-CMCC-FEM and its applications, we strongly encourage you to refer to the literature (Collalti et al., 2014, 2016, 2020, 2024; Marconi et al., 2017; Mahnken et al., 2022; Dalmonech et al., 2022, 2024; Testolin et al., 2023; Vangi et al., 2024a, 2024b; Morichetti et al., 2024), and the main [**web page**](https://www.forest-modelling-lab.com/the-3d-cmcc-model), where the most updated user guide can be found (which include the detailed description of all inputs and outputs, as well as the instruction for launching the model from command line, Eclipse and Bash; Collalti et al., 2022).

## Running simulations 

The main function of the package is run_3DCMCCFEM, which is a wrapper of the C code compiled in the .exe file provided within the package. The function allows the model to be run from the R environment. Each argument of the C functions is matched in the wrapper so that the model can be launched with every possible setting. First, the function performs several checks needed to ensure the consistency of all arguments specified by the user, then checks the consistency against the model specifications, and finally builds the system call to run the model, translating the R-code to a Bash call. All the inputs needed for the simulation (see the 3D-CMCC-FEM User Guide parag. 4 for a detailed overview of each input file), must be in the same directory, whose path is an input for the function. The output is saved locally following a root path that depends on the simulation setting (i.e., temporal scale of the output, name of the simulated site, whether the simulation has been performed with fixed CO2 or active management, etc.) and is managed internally by the C-code. The user needs to specify the working directory where to save the simulation outputs, and the function creates the tree path accordingly. The output files saved by the function consist of the main output file, which contains fluxes and stock values for each time step (i.e. day, month, year), each species and layer (see the 3D-CMCC-FEM User Guide pararg. 4.10 for the detailed output list), a debug file, where, in case of failed simulation, all the errors of the run and the list of the input file used for the simulation are reported (helpful in debugging and sharing). 

## Pubblication

All the pieces of information in this README are taken from the pre-print paper [R3DFEM: an R package for running the 3D-CMCC-FEM model](https://doi.org/10.1101/2024.10.07.616968). We hope to have a published version ASAP.
Have fun and have a nice modeling day!!!
