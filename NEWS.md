# R3DFEM 0.2.0

The package now runs only the version 5.7 of the 3D-CMCC-FEM.

## Breaking changes

* `run_3DCMCCFEM()` runs the version 5.7 (it has the arguments of the former
  `run_3DCMCCFEM_5.7()`); the version 5.6 is no longer supported. Use R3DFEM
  0.1.0 for the 5.6.
* The model executable is no longer shipped with the package: install it once
  with `install_model()` (download, or copy of a local file) or set
  `options(R3DFEM.exe = ...)` / the environment variable `R3DFEM_EXE`. This
  also allows binaries compiled on Linux or macOS.
* `make_meteo_EOBS()` handles the E-OBS versions 30.0 (default) and 29.0
  (`version`), reads the netCDF files from `eobs_dir` (option
  `R3DFEM.eobs_dir`) and names the files of several points `id_meteo.txt`
  (`site_id_meteo.txt` with `site`), as `make_soil_ISRIC()`.
* `make_soil_ISRIC()` reads the SoilGrids raster from `isric_file` (option
  `R3DFEM.isric_file`).
* `check_inputs()` has no `version` argument: the species files must always
  contain the parameters of the 5.7 (e.g. `CONV_VOL_FACTOR`).
* The gif functions (`make_gif_3DFEM()` and the internal gif helpers) were
  removed, together with the dependencies rgl, rLiDAR (archived on CRAN), FNN,
  magick, gifski, gt, RColorBrewer and doParallel.
* The run messages are sent with `message()` (they can be silenced with
  `suppressMessages()`).

## New functions

* `install_model()`, `model_path()`, `model_available()`.
* `list_species()`, `species_file()`, `copy_species()`: parameterizations of
  the version 5.7 for 19 European species (including coppice), in
  `extdata/species`.
* `setup_inputs()` prepares the whole input directory of a site in one call.
* `read_output()` reads daily, monthly and annual outputs (one or many files)
  as a data.frame, with a DATE column.
* `run_batch()` runs many simulations (sites, scenarios, options, species
  parameters), each in its own directory, sequentially or in parallel with
  future.
* `param_sensitivity()` (now exported) is based on `run_batch()` and returns
  the data and a ggplot.

## Deprecated

* `run_3DCMCCFEM_5.7()` and `make_meteo_EOBS_30.0()` are aliases of
  `run_3DCMCCFEM()` and `make_meteo_EOBS()`.

## Other changes

* Example data: the BilyKriz, Hyytiala and Soroe inputs use the 5.7
  parameterizations; the 5.6 executables and outputs were removed.
* The species files are read also when saved with a byte order mark.
* `make_co2()` accepts a data.frame with a single year.
* `plot_output_3DFEM()` and `virtual_stand_generator()` use `read_output()`;
  `plot_output_3DFEM()` no longer warns when plotting integer and double
  variables together.
* `run_3DCMCCFEM()` warns when `outputdir` is too long for the model (260
  characters limit of the paths).
* Unit tests (testthat), GitHub Actions (R CMD check, pkgdown site), a
  precomputed "Getting started" vignette and a new vignette on the input
  files.

# R3DFEM 0.1.0

## New functions

* `make_stand()`, `make_co2()`, `make_management()`, `make_soil()` and `make_meteo()`
  create the input files from user data.
* `read_species()` and `set_species_param()` read and modify species
  parameterization files, keeping comments.
* `check_inputs()` runs the consistency checks of the run functions without
  running the model.
* New vignette `vignette("R3DFEM")`, running the model version 5.7.
* `run_3DCMCCFEM_5.7()` and `check_inputs(version = "5.7")` check that the
  species files contain `CONV_VOL_FACTOR`, required by the 5.7 executable.
* New example file `extdata/species_5.7/Piceaabies.txt` (5.7 parameterization of
  Norway spruce); the 5.7 executable is now in `extdata/Debug`, where
  `run_3DCMCCFEM_5.7()` looks for it.

## Bug fixes

* `run_3DCMCCFEM()` and `run_3DCMCCFEM_5.7()`:
  * with `co2 = "off"` or `"var"` the model used the settings file of
    `co2 = "on"` (silently, if one existed from a previous run);
  * stands with more classes or species and `man = "on"` failed;
  * replanting of more species (`rep_species` vector) failed in the checks;
  * `man = "var"`, `pres_dens = "on"`, `after_pres = "off"`, `reg = "on"`
    failed ("object 'mn' not found");
  * the working directory of the user was changed and not restored;
  * `outputdir` was not created when its parent directory did not exist;
  * the 5.7 version ignored `reg_species` with a single replanted species;
  * a clear error with the model log is given when the model produces no output.
  They now return invisibly the paths of the output files.
* `make_meteo_EOBS_30.0()` without `id` wrote the meteo variables in the wrong
  columns; `make_meteo_EOBS()` (v29) failed because the wind speed file is not
  available (wind speed was not used).
* `make_soil_ISRIC()` weighted the soil layers dividing by their thickness
  instead of multiplying; a data.frame of coordinates was not accepted.
* `check_meteo_3DFEM()`: anomalous values were reported only when all rows
  were wrong, the check on the number of months used the days, error messages
  had wrong thresholds; it now returns a data.frame and `save` defaults to
  FALSE as documented.
* `make_CCS()`: failed with a single leap year in `obs_year` and with
  data.table/tibble inputs; years after `obs_year[2]` were duplicated; the
  global random seed was changed.
* `plot_output_3DFEM()` did not work (data.table syntax), nor with `window` or
  `x`, and saved files outside `outdir`.
* `plot_soil_3DFEM()` did not read soil files with `//` comments;
  `save_plot` defaults to FALSE as documented.
* `make_topo()`, `virtual_stand_generator()` and `make_gif_3DFEM()` fixes.

## Removed

* `make_stand_INFC()` and the INFC data (`specie`, `tree_level_species`).
