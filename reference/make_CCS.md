# Create detrended meteo input file for the current-climate scenario (CCS).

`make_CCS` use an historical meteo input file to generate a new,
detrended meteo input file for the future (called current-climate
scenario).

## Usage

``` r
make_CCS(
  meteo = NULL,
  co2 = NULL,
  obs_year = NULL,
  end_year = 2099,
  outdir = NULL,
  keep_name = TRUE
)
```

## Arguments

- meteo:

  Character or data.frame: Historical meteo data or path to a meteo.txt
  file. Must have columns "Year", "Month", "n_days", "Rg_f", "Ta_f",
  "Tmax", "Tmin", "RH_f", "Ts_f", "Precip", "SWC", "LAI", "ET", "WS_f"
  (see details). Can be created for European sites with the function
  [`make_meteo_EOBS`](https://vangielia.github.io/R3DFEM/reference/make_meteo_EOBS.md),
  using E-OBS data

- co2:

  Character or data.frame: Historical CO2 data or path to co2.txt file.
  Must have columns "year" and "CO2_ppm"

- obs_year:

  Numeric vector of length 2: first and last year of historical meteo
  and CO2 data to be used to generate the future detrended weather. Must
  be present in both meteo and co2 data.frame

- end_year:

  Numeric: Last year of simulated meteo and co2 data. Default to 2099

- outdir:

  Character: path to the output directory. If does not exist it will be
  created

- keep_name:

  Logical: if TRUE the name of the detrended file will be the same of
  the the original one, otherwise the prefix "detrended\_" will be add

## Value

Used for its side effects. It saves in `outdir` a meteo.txt and co2.txt
file with the same columns of the input data and returns invisibly the
table of resampled years.

## Details

The input meteo data.frame must have the columns:

- *Year*: Reference year for the meteo data

- *Month*: number of month in the year

- *n_day*: number of day in the month

- *Rg_f*: Solar radiation on the ground (in WJ/m-2)

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

The input co2 data.frame must have the colums:

- *year*: Reference year for the co2 data

- *CO2_ppm*: CO2 concentration (in ppm)

Only the years up to `obs_year[2]` of the input data are kept, then the
years from `obs_year[2] + 1` to `end_year` are filled by randomly
resampling (with a fixed seed, leap years with leap years) the years in
`obs_year`. All the year of simulation must be included in the meteo and
co2 input files. For more info on the stand input data needed see the
official [User's
Guide](http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf)

## See also

[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
