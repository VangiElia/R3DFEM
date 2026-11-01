# Create meteo input file for 3DCMCCFEM model from daily data

`make_meteo` converts a data.frame of daily weather data (e.g. from a
weather station or a climate model) into the site_meteo.txt file
required by the model. For European sites the data can be taken from
E-OBS with
[`make_meteo_EOBS`](https://vangielia.github.io/R3DFEM/reference/make_meteo_EOBS.md).

## Usage

``` r
make_meteo(data, site, rad_unit = c("MJ", "W"), check = TRUE, outdir)
```

## Arguments

- data:

  data.frame with daily data. It must contain a column `date` (Date or
  "yyyy-mm-dd") or the columns `Year`, `Month` and `n_days`, and the
  columns `Rg_f` (global radiation), `Tmax`, `Tmin` (°C), `Precip` (mm
  day-1) and `RH_f` (\\ computed as (Tmax+Tmin)/2 if missing), `Ts_f`,
  `SWC`, `LAI`, `ET`, `WS_f` (set to -9999 if missing).

- site:

  Character: Name of the site. Used to construct output name

- rad_unit:

  Character: unit of `Rg_f`: "MJ" (MJ m-2 day-1, model unit) or "W" (W
  m-2, daily mean). Default to "MJ"

- check:

  Logical: check the result with
  [`check_meteo_3DFEM`](https://vangielia.github.io/R3DFEM/reference/check_meteo_3DFEM.md)
  (without correcting it)? Default to TRUE

- outdir:

  Character: path to the output directory.

## Value

Invisibly, the meteo data.frame. The file site_meteo.txt is saved in
`outdir`.

## See also

[`make_meteo_EOBS`](https://vangielia.github.io/R3DFEM/reference/make_meteo_EOBS.md),
[`check_meteo_3DFEM`](https://vangielia.github.io/R3DFEM/reference/check_meteo_3DFEM.md),
[`make_CCS`](https://vangielia.github.io/R3DFEM/reference/make_CCS.md)
