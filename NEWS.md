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
