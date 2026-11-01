# Create meteo input file for 3DCMCCFEM model from E-OBS

`make_meteo_EOBS` uses the [E-OBS
database](https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles/)
to create the meteo input file according to the 3DCMCCFEM model
specifications. The time period covered is 1950-01-01 - 2024-06-30 for
the version 30.0 and 1950-01-01 - 2023-12-31 for the version 29.0.

## Usage

``` r
make_meteo_EOBS(
  plot,
  crs = NULL,
  buffer = 100,
  id = NULL,
  daterange = NULL,
  convert_rad = TRUE,
  parallel = FALSE,
  site = NULL,
  outdir = NULL,
  return_df = FALSE,
  version = c("30.0", "29.0"),
  eobs_dir = getOption("R3DFEM.eobs_dir", system.file("extdata", package = "R3DFEM"))
)
```

## Arguments

- plot:

  Sf object or something that can be converted to an sf point object
  (i.e., a data.frame of coordinates). If plot is a data.frame the
  argument crs must be specified.

- crs:

  Numeric: a valid EPSG code

- buffer:

  Numeric: buffer around plot to extract meteo data from

- id:

  Character: name of the column in `plot` that identifies different
  points

- daterange:

  Character vector of length 2: starting and ending date from which to
  extract climate data. Must be in the form yyyy-mm-dd. Default to the
  whole period of the E-OBS version.

- convert_rad:

  Logical: Should the solar radiation be converted from W m-2 (E-OBS
  unit) to MJ m-2 day-1 (model unit)? Default to TRUE.

- parallel:

  Logical: Should the extraction be performed in parallel? Default to
  FALSE

- site:

  Character: the name of the plot site. Used to construct the output
  name

- outdir:

  Character: path to the output directory.

- return_df:

  Logical: should the climate data be returned by the function as a
  data.frame? Default to FALSE

- version:

  Character: version of E-OBS, "30.0" or "29.0". Default to "30.0"

- eobs_dir:

  Character: directory with the E-OBS netCDF files (ensemble mean, 0.1
  deg, e.g. tg_ens_mean_0.1deg_reg_v30.0e.nc) of the variables hu, qq,
  rr, tg, tn and tx. Default to the option `R3DFEM.eobs_dir` or, if not
  set, to the extdata directory of the installed package.

## Value

Used for its side effects. It saves in `outdir` a file site_meteo.txt
The output file have the colums:

- *Year*: Reference year for the meteo data

- *Month*: number of month in the year

- *n_day*: number of day in the month

- *Rg_f*: Solar radiation on the ground (in MJ m-2 day-1)

- *Ta_f*: Mean temperature (in °C)

- *Tmax*: Maximum temperature (in °C)

- *Tmin*: Minimum temperature (in °C)

- *RH_f*: relative humidity (in %)

- *Ts_f*: Daily soil temperature (in °C)

- *Precip*: Daily precipitation (in mm day-1)

- *SWC*:Soil Water Content (in mm m-2)\*

- *LAI*:Leaf Area Index (in m2 m-2) \*

- *ET*: Evapotranspiration (in mm m-2 day-1\*

- *WS_f*: Wind speed (in m s-1)\*

## Details

The E-OBS files are not distributed with the package: download them from
the Copernicus web site and set, e.g.,
`options(R3DFEM.eobs_dir = "D:/EOBS")`. If parallel=TRUE, the user can
set a future plan, otherwise the function will use a 7-workers
`future::plan("multisession")` with
[`foreach`](https://rdrr.io/pkg/foreach/man/foreach.html) and the
doFuture package. With `id`, one file per point is written, named
id_meteo.txt (site_id_meteo.txt if `site` is given), as in
[`make_soil_ISRIC`](https://vangielia.github.io/R3DFEM/reference/make_soil_ISRIC.md).
For more information on meteo file needed by the model please refer to
the [User's
Guide](http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf)

## See also

[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md),
[`make_meteo`](https://vangielia.github.io/R3DFEM/reference/make_meteo.md)
