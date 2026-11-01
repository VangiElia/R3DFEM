# Create management input file for 3DCMCCFEM model

`make_management` writes the site_management.txt file used by the model
when `man = "var"`: years of thinning (and their intensity), harvesting
and regeneration.

## Usage

``` r
make_management(
  site,
  thinning = NULL,
  thinning_int = NULL,
  harvesting = NULL,
  regeneration = NULL,
  outdir
)
```

## Arguments

- site:

  Character: Name of the site. Used to construct output name

- thinning:

  Integer vector: years of thinning. Default to NULL

- thinning_int:

  Numeric vector: intensity of each thinning (\\ area removed). If NULL
  the parameter THINNING_INTENSITY of the species file is used. Default
  to NULL

- harvesting:

  Integer vector: years of harvesting. Default to NULL

- regeneration:

  Integer vector: years of regeneration. Default to NULL

- outdir:

  Character: path to the output directory.

## Value

Invisibly, a named list with the management actions. The file
site_management.txt is saved in `outdir`.

## See also

[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md),
[`check_inputs`](https://vangielia.github.io/R3DFEM/reference/check_inputs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
make_management("BilyKriz", thinning = c(2030, 2060, 2090),
                thinning_int = c(30, 20, 10), harvesting = 2102,
                regeneration = 2103, outdir = tempdir())
} # }
```
