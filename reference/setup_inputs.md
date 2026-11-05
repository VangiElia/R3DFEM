# Prepare the input directory of a simulation

`setup_inputs` creates in one call all the input files needed by
[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
for a site, named as required by the model, and copies the species
parameterizations of the version 5.7 shipped with the package (see
[`list_species`](https://vangielia.github.io/R3DFEM/reference/list_species.md)).

## Usage

``` r
setup_inputs(
  site,
  inputdir,
  stand,
  soil,
  topo,
  meteo,
  co2,
  management = NULL,
  species = NULL
)
```

## Arguments

- site:

  Character: name of the site, used to name the files.

- inputdir:

  Character: input directory (created if it does not exist).

- stand, soil, topo, meteo, co2, management:

  Each input can be:

  - the path of an existing file, copied in `inputdir` with the name
    required by the model (e.g. site_stand.txt);

  - a list of arguments of the corresponding helper
    ([`make_stand`](https://vangielia.github.io/R3DFEM/reference/make_stand.md),
    [`make_soil`](https://vangielia.github.io/R3DFEM/reference/make_soil.md),
    [`make_topo`](https://vangielia.github.io/R3DFEM/reference/make_topo.md),
    [`make_meteo`](https://vangielia.github.io/R3DFEM/reference/make_meteo.md),
    [`make_co2`](https://vangielia.github.io/R3DFEM/reference/make_co2.md),
    [`make_management`](https://vangielia.github.io/R3DFEM/reference/make_management.md)),
    without `site` and `outdir`;

  - for `meteo`, a data.frame of daily data passed to
    [`make_meteo`](https://vangielia.github.io/R3DFEM/reference/make_meteo.md);
    for `stand` and `soil`, a data.frame with the columns of the file;
    for `co2`, a data.frame with year and CO2_ppm; for `topo`, the
    elevation of the site (m).

  `management` is optional (needed with `man = "var"`).

- species:

  Character: species parameterizations to copy from the package. Default
  to the species of the stand file. Files already in `inputdir` (e.g.
  site-specific parameterizations) are kept.

## Value

Invisibly, the named paths of the input files.

## See also

[`check_inputs`](https://vangielia.github.io/R3DFEM/reference/check_inputs.md),
[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)

## Examples

``` r
inp <- file.path(tempdir(), "Collelongo")
setup_inputs("Collelongo", inp,
             stand = list(year = 2000, age = 100, species = "Fagussylvatica",
                          n_trees = 800, avdbh = 30, height = 22),
             soil = list(lat = 41.85, lon = 13.59, clay = 20, silt = 40, sand = 40),
             topo = 1560,
             meteo = system.file("extdata", "soro", "Soroe_meteo.txt", package = "R3DFEM"),
             co2 = list(years = 1997:2099, co2 = 380))
#> Outdir does not exist. It will be created
#> file /tmp/Rtmptz7PmD/Collelongo/Collelongo_stand.txt saved
#> file /tmp/Rtmptz7PmD/Collelongo/Collelongo_soil.txt saved
#> file /tmp/Rtmptz7PmD/Collelongo/Collelongo_topo.txt saved
#> file /tmp/Rtmptz7PmD/Collelongo/Collelongo_co2.txt saved
list.files(inp)
#> [1] "Collelongo_co2.txt"   "Collelongo_meteo.txt" "Collelongo_soil.txt" 
#> [4] "Collelongo_stand.txt" "Collelongo_topo.txt"  "Fagussylvatica.txt"  
```
