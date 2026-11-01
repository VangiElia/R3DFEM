# Changelog

## R3DFEM 0.2.0

The package now runs only the version 5.7 of the 3D-CMCC-FEM.

### Breaking changes

- [`run_3DCMCCFEM()`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
  runs the version 5.7 (it has the arguments of the former
  [`run_3DCMCCFEM_5.7()`](https://vangielia.github.io/R3DFEM/reference/R3DFEM-deprecated.md));
  the version 5.6 is no longer supported. Use R3DFEM 0.1.0 for the 5.6.
- The model executable is no longer shipped with the package: install it
  once with
  [`install_model()`](https://vangielia.github.io/R3DFEM/reference/install_model.md)
  (download, or copy of a local file) or set `options(R3DFEM.exe = ...)`
  / the environment variable `R3DFEM_EXE`. This also allows binaries
  compiled on Linux or macOS.
- [`make_meteo_EOBS()`](https://vangielia.github.io/R3DFEM/reference/make_meteo_EOBS.md)
  handles the E-OBS versions 30.0 (default) and 29.0 (`version`), reads
  the netCDF files from `eobs_dir` (option `R3DFEM.eobs_dir`) and names
  the files of several points `id_meteo.txt` (`site_id_meteo.txt` with
  `site`), as
  [`make_soil_ISRIC()`](https://vangielia.github.io/R3DFEM/reference/make_soil_ISRIC.md).
- [`make_soil_ISRIC()`](https://vangielia.github.io/R3DFEM/reference/make_soil_ISRIC.md)
  reads the SoilGrids raster from `isric_file` (option
  `R3DFEM.isric_file`).
- [`check_inputs()`](https://vangielia.github.io/R3DFEM/reference/check_inputs.md)
  has no `version` argument: the species files must always contain the
  parameters of the 5.7 (e.g. `CONV_VOL_FACTOR`).
- The gif functions (`make_gif_3DFEM()` and the internal gif helpers)
  were removed, together with the dependencies rgl, rLiDAR (archived on
  CRAN), FNN, magick, gifski, gt, RColorBrewer and doParallel.
- The run messages are sent with
  [`message()`](https://rdrr.io/r/base/message.html) (they can be
  silenced with
  [`suppressMessages()`](https://rdrr.io/r/base/message.html)).

### New functions

- [`install_model()`](https://vangielia.github.io/R3DFEM/reference/install_model.md),
  [`model_path()`](https://vangielia.github.io/R3DFEM/reference/install_model.md),
  [`model_available()`](https://vangielia.github.io/R3DFEM/reference/install_model.md).
- [`list_species()`](https://vangielia.github.io/R3DFEM/reference/list_species.md),
  [`species_file()`](https://vangielia.github.io/R3DFEM/reference/list_species.md),
  [`copy_species()`](https://vangielia.github.io/R3DFEM/reference/list_species.md):
  parameterizations of the version 5.7 for 19 European species
  (including coppice), in `extdata/species`.
- [`setup_inputs()`](https://vangielia.github.io/R3DFEM/reference/setup_inputs.md)
  prepares the whole input directory of a site in one call.
- [`read_output()`](https://vangielia.github.io/R3DFEM/reference/read_output.md)
  reads daily, monthly and annual outputs (one or many files) as a
  data.frame, with a DATE column.
- [`run_batch()`](https://vangielia.github.io/R3DFEM/reference/run_batch.md)
  runs many simulations (sites, scenarios, options, species parameters),
  each in its own directory, sequentially or in parallel with future.
- [`param_sensitivity()`](https://vangielia.github.io/R3DFEM/reference/param_sensitivity.md)
  (now exported) is based on
  [`run_batch()`](https://vangielia.github.io/R3DFEM/reference/run_batch.md)
  and returns the data and a ggplot.

### Deprecated

- [`run_3DCMCCFEM_5.7()`](https://vangielia.github.io/R3DFEM/reference/R3DFEM-deprecated.md)
  and
  [`make_meteo_EOBS_30.0()`](https://vangielia.github.io/R3DFEM/reference/R3DFEM-deprecated.md)
  are aliases of
  [`run_3DCMCCFEM()`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
  and
  [`make_meteo_EOBS()`](https://vangielia.github.io/R3DFEM/reference/make_meteo_EOBS.md).

### Other changes

- Example data: the BilyKriz, Hyytiala and Soroe inputs use the 5.7
  parameterizations; the 5.6 executables and outputs were removed.
- The species files are read also when saved with a byte order mark.
- [`make_co2()`](https://vangielia.github.io/R3DFEM/reference/make_co2.md)
  accepts a data.frame with a single year.
- [`plot_output_3DFEM()`](https://vangielia.github.io/R3DFEM/reference/plot_output_3DFEM.md)
  and
  [`virtual_stand_generator()`](https://vangielia.github.io/R3DFEM/reference/virtual_stand_generator.md)
  use
  [`read_output()`](https://vangielia.github.io/R3DFEM/reference/read_output.md);
  [`plot_output_3DFEM()`](https://vangielia.github.io/R3DFEM/reference/plot_output_3DFEM.md)
  no longer warns when plotting integer and double variables together.
- [`run_3DCMCCFEM()`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
  warns when `outputdir` is too long for the model (260 characters limit
  of the paths).
- Unit tests (testthat), GitHub Actions (R CMD check, pkgdown site), a
  precomputed “Getting started” vignette and a new vignette on the input
  files.

## R3DFEM 0.1.0

### New functions

- [`make_stand()`](https://vangielia.github.io/R3DFEM/reference/make_stand.md),
  [`make_co2()`](https://vangielia.github.io/R3DFEM/reference/make_co2.md),
  [`make_management()`](https://vangielia.github.io/R3DFEM/reference/make_management.md),
  [`make_soil()`](https://vangielia.github.io/R3DFEM/reference/make_soil.md)
  and
  [`make_meteo()`](https://vangielia.github.io/R3DFEM/reference/make_meteo.md)
  create the input files from user data.
- [`read_species()`](https://vangielia.github.io/R3DFEM/reference/read_species.md)
  and
  [`set_species_param()`](https://vangielia.github.io/R3DFEM/reference/read_species.md)
  read and modify species parameterization files, keeping comments.
- [`check_inputs()`](https://vangielia.github.io/R3DFEM/reference/check_inputs.md)
  runs the consistency checks of the run functions without running the
  model.
- New vignette
  [`vignette("R3DFEM")`](https://vangielia.github.io/R3DFEM/articles/R3DFEM.md),
  running the model version 5.7.
- [`run_3DCMCCFEM_5.7()`](https://vangielia.github.io/R3DFEM/reference/R3DFEM-deprecated.md)
  and `check_inputs(version = "5.7")` check that the species files
  contain `CONV_VOL_FACTOR`, required by the 5.7 executable.
- New example file `extdata/species_5.7/Piceaabies.txt` (5.7
  parameterization of Norway spruce); the 5.7 executable is now in
  `extdata/Debug`, where
  [`run_3DCMCCFEM_5.7()`](https://vangielia.github.io/R3DFEM/reference/R3DFEM-deprecated.md)
  looks for it.

### Bug fixes

- [`run_3DCMCCFEM()`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
  and
  [`run_3DCMCCFEM_5.7()`](https://vangielia.github.io/R3DFEM/reference/R3DFEM-deprecated.md):
  - with `co2 = "off"` or `"var"` the model used the settings file of
    `co2 = "on"` (silently, if one existed from a previous run);
  - stands with more classes or species and `man = "on"` failed;
  - replanting of more species (`rep_species` vector) failed in the
    checks;
  - `man = "var"`, `pres_dens = "on"`, `after_pres = "off"`,
    `reg = "on"` failed (“object ‘mn’ not found”);
  - the working directory of the user was changed and not restored;
  - `outputdir` was not created when its parent directory did not exist;
  - the 5.7 version ignored `reg_species` with a single replanted
    species;
  - a clear error with the model log is given when the model produces no
    output. They now return invisibly the paths of the output files.
- [`make_meteo_EOBS_30.0()`](https://vangielia.github.io/R3DFEM/reference/R3DFEM-deprecated.md)
  without `id` wrote the meteo variables in the wrong columns;
  [`make_meteo_EOBS()`](https://vangielia.github.io/R3DFEM/reference/make_meteo_EOBS.md)
  (v29) failed because the wind speed file is not available (wind speed
  was not used).
- [`make_soil_ISRIC()`](https://vangielia.github.io/R3DFEM/reference/make_soil_ISRIC.md)
  weighted the soil layers dividing by their thickness instead of
  multiplying; a data.frame of coordinates was not accepted.
- [`check_meteo_3DFEM()`](https://vangielia.github.io/R3DFEM/reference/check_meteo_3DFEM.md):
  anomalous values were reported only when all rows were wrong, the
  check on the number of months used the days, error messages had wrong
  thresholds; it now returns a data.frame and `save` defaults to FALSE
  as documented.
- [`make_CCS()`](https://vangielia.github.io/R3DFEM/reference/make_CCS.md):
  failed with a single leap year in `obs_year` and with
  data.table/tibble inputs; years after `obs_year[2]` were duplicated;
  the global random seed was changed.
- [`plot_output_3DFEM()`](https://vangielia.github.io/R3DFEM/reference/plot_output_3DFEM.md)
  did not work (data.table syntax), nor with `window` or `x`, and saved
  files outside `outdir`.
- [`plot_soil_3DFEM()`](https://vangielia.github.io/R3DFEM/reference/plot_soil_3DFEM.md)
  did not read soil files with `//` comments; `save_plot` defaults to
  FALSE as documented.
- [`make_topo()`](https://vangielia.github.io/R3DFEM/reference/make_topo.md),
  [`virtual_stand_generator()`](https://vangielia.github.io/R3DFEM/reference/virtual_stand_generator.md)
  and `make_gif_3DFEM()` fixes.

### Removed

- `make_stand_INFC()` and the INFC data (`specie`,
  `tree_level_species`).
