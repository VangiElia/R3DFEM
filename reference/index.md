# Package index

## Model

- [`install_model()`](https://vangielia.github.io/R3DFEM/reference/install_model.md)
  [`model_path()`](https://vangielia.github.io/R3DFEM/reference/install_model.md)
  [`model_available()`](https://vangielia.github.io/R3DFEM/reference/install_model.md)
  : Install and locate the 3D-CMCC-FEM executable
- [`run_3DCMCCFEM()`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
  : Run the 3D-CMCC-FEM model (version 5.7)
- [`run_batch()`](https://vangielia.github.io/R3DFEM/reference/run_batch.md)
  : Run many simulations of the 3DCMCCFEM model
- [`param_sensitivity()`](https://vangielia.github.io/R3DFEM/reference/param_sensitivity.md)
  : Sensitivity of the model to a species parameter

## Input files

- [`setup_inputs()`](https://vangielia.github.io/R3DFEM/reference/setup_inputs.md)
  : Prepare the input directory of a simulation
- [`make_stand()`](https://vangielia.github.io/R3DFEM/reference/make_stand.md)
  : Create stand input file for 3DCMCCFEM model
- [`make_meteo()`](https://vangielia.github.io/R3DFEM/reference/make_meteo.md)
  : Create meteo input file for 3DCMCCFEM model from daily data
- [`make_co2()`](https://vangielia.github.io/R3DFEM/reference/make_co2.md)
  : Create CO2 input file for 3DCMCCFEM model
- [`make_soil()`](https://vangielia.github.io/R3DFEM/reference/make_soil.md)
  : Create soil input file for 3DCMCCFEM model
- [`make_topo()`](https://vangielia.github.io/R3DFEM/reference/make_topo.md)
  : Create topo input file for 3DCMCCFEM model
- [`make_management()`](https://vangielia.github.io/R3DFEM/reference/make_management.md)
  : Create management input file for 3DCMCCFEM model
- [`make_meteo_EOBS()`](https://vangielia.github.io/R3DFEM/reference/make_meteo_EOBS.md)
  : Create meteo input file for 3DCMCCFEM model from E-OBS
- [`make_soil_ISRIC()`](https://vangielia.github.io/R3DFEM/reference/make_soil_ISRIC.md)
  : Create soil input file for 3DCMCCFEM model
- [`make_CCS()`](https://vangielia.github.io/R3DFEM/reference/make_CCS.md)
  : Create detrended meteo input file for the current-climate scenario
  (CCS).
- [`virtual_stand_generator()`](https://vangielia.github.io/R3DFEM/reference/virtual_stand_generator.md)
  : Create stand file for virtual forest stand based on the 3D-CMCC_FEM
  model simulation

## Species parameters

- [`list_species()`](https://vangielia.github.io/R3DFEM/reference/list_species.md)
  [`species_file()`](https://vangielia.github.io/R3DFEM/reference/list_species.md)
  [`copy_species()`](https://vangielia.github.io/R3DFEM/reference/list_species.md)
  : Species parameterizations of the version 5.7
- [`read_species()`](https://vangielia.github.io/R3DFEM/reference/read_species.md)
  [`set_species_param()`](https://vangielia.github.io/R3DFEM/reference/read_species.md)
  : Read and modify species parameterization files

## Checks

- [`check_inputs()`](https://vangielia.github.io/R3DFEM/reference/check_inputs.md)
  : Check the input files before running the 3DCMCCFEM model
- [`check_meteo_3DFEM()`](https://vangielia.github.io/R3DFEM/reference/check_meteo_3DFEM.md)
  : Check meteo data input as required by the 3DCMCCFEM model

## Outputs and plots

- [`read_output()`](https://vangielia.github.io/R3DFEM/reference/read_output.md)
  : Read an output file of the 3DCMCCFEM model
- [`plot_output_3DFEM()`](https://vangielia.github.io/R3DFEM/reference/plot_output_3DFEM.md)
  : Plot the annual output of the 3DCMCCFEM model
- [`plot_meteo_3DFEM()`](https://vangielia.github.io/R3DFEM/reference/plot_meteo_3DFEM.md)
  : Plot meteo data input as required by the 3DCMCCFEM model
- [`plot_soil_3DFEM()`](https://vangielia.github.io/R3DFEM/reference/plot_soil_3DFEM.md)
  : Plot soil data input as required by the 3DCMCCFEM model

## Other

- [`R3DFEM`](https://vangielia.github.io/R3DFEM/reference/R3DFEM.md) :
  R3DFEM: an R package for running the 3D-CMCC-FEM model (version 5.7)
- [`` `%>%` ``](https://vangielia.github.io/R3DFEM/reference/pipe.md) :
  Pipe operator
- [`run_3DCMCCFEM_5.7()`](https://vangielia.github.io/R3DFEM/reference/R3DFEM-deprecated.md)
  [`make_meteo_EOBS_30.0()`](https://vangielia.github.io/R3DFEM/reference/R3DFEM-deprecated.md)
  : Deprecated functions
