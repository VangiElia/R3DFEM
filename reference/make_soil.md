# Create soil input file for 3DCMCCFEM model

`make_soil` writes the site_soil.txt file from user-provided values
(e.g. field measurements). For European sites the texture can be taken
from SoilGrids with
[`make_soil_ISRIC`](https://vangielia.github.io/R3DFEM/reference/make_soil_ISRIC.md).

## Usage

``` r
make_soil(
  site,
  lat,
  lon,
  clay,
  silt,
  sand,
  soil_depth = 100,
  fr = 0.9,
  fn0 = 0.5,
  fnn = 0.5,
  m0 = 0.2,
  litterc = -9999,
  littern = -9999,
  soilc = -9999,
  soiln = -9999,
  deadwoodc = -9999,
  outdir
)
```

## Arguments

- site:

  Character: Name of the site. Used to construct output name

- lat, lon:

  Numeric: latitude and longitude of the site (WGS84)

- clay, silt, sand:

  Numeric: percentage of clay, silt and sand. Their sum must be 100
  (+-1)

- soil_depth:

  Numeric: soil depth (cm). Default to 100

- fr:

  Numeric: fertility rating (only LUE version). Default to 0.9

- fn0:

  Numeric: value of fertility modifier when FR=0. Default to 0.5

- fnn:

  Numeric: Default to 0.5

- m0:

  Numeric: value of 'm' when FR=0. Default to 0.2

- litterc, littern, soilc, soiln, deadwoodc:

  Numeric: initial carbon and nitrogen pools. Default to -9999 (computed
  by the model)

- outdir:

  Character: path to the output directory.

## Value

Invisibly, the soil data.frame. The file site_soil.txt is saved in
`outdir`.

## See also

[`make_soil_ISRIC`](https://vangielia.github.io/R3DFEM/reference/make_soil_ISRIC.md),
[`plot_soil_3DFEM`](https://vangielia.github.io/R3DFEM/reference/plot_soil_3DFEM.md)
