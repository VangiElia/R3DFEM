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

Below a schematic representation of the workflow of the package:

![](https://github.com/VangiElia/R3DFEM/blob/main/readme/LOGO_3DCMCCFEM_color.png)

## I/O
Below, we present a schematic description of the input needed by the functions in the R package: 
For initialization, the 3D-CMCC-FEM requires as input data: 

1. The initial stand conditions: species name (since the model is parameterized at species level), age, mean tree height, diameter at breast height (DBH), and number of trees per size cell. The initial data are aggregated per classes (height classes, cohorts, and species) by a pre-processing activity as follows: (1) the relative values of diameters class is associated for each species, (2) the corresponding value of height class is assigned for each diameter class, and (3) the relative age is assigned for each height class (Collalti et al., 2014; 2024). 
2. Species-specific parameters are primarily based on species-specific eco-physiological and allometric characteristics and can be partially derived from forest inventories and literature (Collalti et al., 2019). The package comes with a suite of already parameterized files for different and most common European tree species used in many real case studies across Europe. 
3. Meteorological forcing data: daily maximum (Tmax, °C) and minimum air temperature (Tmin, °C), soil temperature (Tsoil, °C), vapor pressure deficit (hPa), global solar radiation (MJ m−2 day−1) and precipitation amount (mm day−1). 
4. Annual atmospheric CO2 concentration and nitrogen deposition (optional)(Collalti et al., 2018). 
5. Soil and topographic information: soil depth, average sand, clay, silt percentages, and elevation.

All input data need to be written into separate .txt files whose structure is fully described in the [**user manual**](https://www.forest-modelling-lab.com/_files/ugd/8a7700_d31451e9a5e64073b50c07f7f007eb71.pdf). Based on the input files and the argument set, the function wrapping the model (run_3DFEM) creates a setting file in the output directory used only by the internal C code; the user does not need to interact with the setting file. 

The main output of the 3D-CMCC-FEM (either a daily, monthly, or annual scale) are Gross Primary Productivity (GPP), Net Primary Productivity (NPP), and state variables such as evapotranspiration (ET), Leaf Area Index (LAI), and rain interception (to cite some). Results are obtained either at class level (species, diameter, height, or age class level), layer level (as the sum of all tree height classes in the same layer), and grid level (as the sum of all classes in the different layers). The model provides information to support decision-making in forest management planning, such as mean annual volume increment (MAI), current volume increment (CAI), basal area, and DBH. 

For detailed information about 3D-CMCC-FEM and its applications, we strongly encourage you to refer to the literature (Collalti et al., 2014, 2016, 2020, 2024; Marconi et al., 2017; Mahnken et al., 2022; Dalmonech et al., 2022, 2024; Testolin et al., 2023; Vangi et al., 2024a, 2024b; Morichetti et al., 2024), and the main [**web page**](https://www.forest-modelling-lab.com/the-3d-cmcc-model, accessed online on 26/09/2024), where the most updated user guide can be found (which include the detailed description of all inputs and outputs, as well as the instruction for launching the model from command line, Eclipse and Bash; Collalti et al., 2022). Throughout the paper and in the description of the function,s we will often refer to the user guide and the official web page. 
