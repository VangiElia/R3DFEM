# Check the input files before running the 3DCMCCFEM model

`check_inputs` performs the same consistency checks of
[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
(presence of the input files, parameters of the version 5.7 in the
species files, years in stand, meteo and co2 files, management and
replanting options) without running the model.

## Usage

``` r
check_inputs(
  site,
  species,
  year_start,
  year_end,
  man,
  co2 = "on",
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
  reg_species = -9999,
  reg_trees = -9999,
  reg_age = -9999,
  reg_dbh = -9999,
  reg_lai = -9999,
  reg_height = -9999,
  inputdir,
  check_meteo = FALSE
)
```

## Arguments

- site:

  Character: Site name. Used to construct output name

- species:

  Character: Species names (same as parameterization files)

- year_start:

  Integer: Starting year of simulation

- year_end:

  Integer: Ending year of simulation

- man:

  Character: Management options; options: c("on","off","var")

- co2:

  Character: CO2 concentration changes according to scenario; options:
  c("on","off","var")

- pres_dens:

  Character: Prescribed densities in stand file if man = "var"; options:
  c("on","off")

- after_pres:

  Character: Management after last year of prescribed density if man =
  "var"; options: c("on","off")

- reg:

  Character: Regeneration; options: c("on","off")

- year_start_man:

  Integer: If man = "on"; Starting year of management; Default to -9999.

- rep_trees, rep_age, rep_dbh, rep_lai, rep_height, rep_species:

  Replanting parameters (number of trees, age, average DBH, LAI, height
  and species). Default to -9999.

- reg_species:

  Character: species of saplings. Default to -9999, i.e. the first
  element of `species`.

- reg_trees:

  Integer: Number of saplings; Default to -9999.

- reg_age:

  Integer: Age of saplings; Default to -9999.

- reg_dbh:

  Integer: Average DBH of saplings; Default to -9999.

- reg_lai:

  Integer: Average LAI of saplings; Default to -9999.

- reg_height:

  Integer: Average height of saplings; Default to -9999.

- inputdir:

  Character: Path to input files directory

- check_meteo:

  Logical: Should the validity of the meteo file be checked with the
  function
  [`check_meteo_3DFEM`](https://vangielia.github.io/R3DFEM/reference/check_meteo_3DFEM.md)?
  Default to FALSE

## Value

Invisibly TRUE if all checks are passed, otherwise an error is thrown.
Warnings are issued for possible inconsistencies.

## See also

[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md),
[`setup_inputs`](https://vangielia.github.io/R3DFEM/reference/setup_inputs.md)

## Examples

``` r
inp <- system.file("extdata", "bilykriz", "rcp0p0", package = "R3DFEM")
check_inputs("BilyKriz", "Piceaabies", 1997, 2010, man = "off", inputdir = inp)
#> All checks passed
```
