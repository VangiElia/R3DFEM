# Create soil input file for 3DCMCCFEM model

`make_soil_ISRIC` use the
[soilGrids](https://www.isric.org/explore/soilgrids) from the ISRIC
database to create the soil input file according to the 3DCMCCFEM model
specifications.

## Usage

``` r
make_soil_ISRIC(
  plot,
  crs = NULL,
  buffer = 100,
  id = NULL,
  site = NULL,
  outdir = NULL,
  return_df = FALSE,
  isric_file = getOption("R3DFEM.isric_file", system.file("extdata", "ISRIC.tif", package
    = "R3DFEM"))
)
```

## Arguments

- plot:

  Sf object or something that can be converted to an sf point object
  (i.e., a data.frame of coordinates). If plot is a data.frame the first
  two colums must specify X and Y coordinates. if plot ia a data.frame
  the argument crs must be specified.

- crs:

  Numeric: a valid EPSG code

- buffer:

  Numeric: buffer around plot to extract meteo data from

- id:

  Character: name of the column in `plot` that identifies different
  points

- site:

  Character: the name of the plot site. Used to construct the output
  name

- outdir:

  Character: path to the output directory.

- return_df:

  Logical: should the climate data be returned by the function as a
  data.frame? Default to FALSE

- isric_file:

  Character: path of the SoilGrids raster (ISRIC.tif, not distributed
  with the package). Default to the option `R3DFEM.isric_file` or, if
  not set, to the extdata directory of the installed package.

## Value

Used for its side effects. It saves in `outdir` a file site_soil.txt.
The most important columns in this file are:

- *X*: x cell position

- *Y*: y cell position

- *LANDUSE*: "F" by default: Forest land use.

- *LAT*: latitude of point

- *LON*: longitude of point

- *CLAY_PERC*: soil clay (in %)

- *SILT_PERC*: soil silt (in %)

- *SAND_PERC*: soil sand (in %)

- *SOIL_DEPTH*: soil depth (in cm), 100 by default

- *FR*:0.9 by default. Fertility rating (dim) (only LUE version)

- *FN0*:0.5 by default. Value of fertility modifier when FR=0 (dim)
  (only LUE version)

- *M0*:0.2 by default. Value of ‘m’ when FR=0 (dim) (only LUE version)

## Details

This function compute the mean percentage of sand, silt and clay in the
first meter of soil, weighted by the thickness of the SoilGrids layers
(0-5, 5-15, 15-30, 30-60, 60-100 cm). The file is written with
[`make_soil`](https://vangielia.github.io/R3DFEM/reference/make_soil.md).
For more information on soil file needed by the model please refer to
the [User's
Guide](http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf)

## See also

[`make_soil`](https://vangielia.github.io/R3DFEM/reference/make_soil.md),
[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
