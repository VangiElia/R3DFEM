# Create CO2 input file for 3DCMCCFEM model

`make_co2` writes the site_co2.txt file with the yearly atmospheric CO2
concentration. The values can be a constant, a vector with one value per
year, or a data.frame of known values that are linearly interpolated
over the requested years (and kept constant outside the known range).

## Usage

``` r
make_co2(site, years, co2, outdir)
```

## Arguments

- site:

  Character: Name of the site. Used to construct output name

- years:

  Integer vector: years to be written (e.g. 1950:2100). Must include all
  the years of the simulation.

- co2:

  Numeric of length 1 (constant concentration), numeric of the same
  length of `years`, or a data.frame with columns year and CO2_ppm (e.g.
  a scenario), or a path to an existing co2.txt file.

- outdir:

  Character: path to the output directory.

## Value

Invisibly, the co2 data.frame. The file site_co2.txt is saved in
`outdir`.

## See also

[`make_CCS`](https://vangielia.github.io/R3DFEM/reference/make_CCS.md),
[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)

## Examples

``` r
if (FALSE) { # \dontrun{
make_co2("BilyKriz", years = 1997:2100,
         co2 = data.frame(year = c(1997, 2020, 2100), CO2_ppm = c(363, 412, 600)),
         outdir = tempdir())
} # }
```
