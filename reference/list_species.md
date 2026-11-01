# Species parameterizations of the version 5.7

The package ships the parameterization files of the version 5.7 of the
model for the main European forest species. `list_species` lists them,
`species_file` returns their paths and `copy_species` copies them in an
input directory.

## Usage

``` r
list_species()

species_file(species)

copy_species(species, outdir, overwrite = FALSE)
```

## Arguments

- species:

  Character vector: species names (name of the file without .txt, e.g.
  "Piceaabies"). See `list_species()`.

- outdir:

  Character: directory where the files are copied (e.g. the `inputdir`
  of
  [`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)).

- overwrite:

  Logical: overwrite files already in `outdir`? Default to FALSE, i.e. a
  site-specific parameterization already in `outdir` is kept.

## Value

`list_species`: a character vector of species names. `species_file`:
named paths of the files. `copy_species`: invisibly, the paths of the
files in `outdir`.

## See also

[`read_species`](https://vangielia.github.io/R3DFEM/reference/read_species.md),
[`set_species_param`](https://vangielia.github.io/R3DFEM/reference/read_species.md),
[`setup_inputs`](https://vangielia.github.io/R3DFEM/reference/setup_inputs.md)

## Examples

``` r
list_species()
#>  [1] "Abiesalba"              "Betulapubescens"        "Castaneasativa"        
#>  [4] "Castaneasativa_coppice" "Fagussylvatica"         "Fagussylvatica_coppice"
#>  [7] "Larixdecidua"           "Piceaabies"             "Pinushalepensis"       
#> [10] "Pinuslaricio"           "Pinuspinaster"          "Pinuspinea"            
#> [13] "Pinussylvestris"        "Quercuscerris"          "Quercuscerris_coppice" 
#> [16] "Quercusilex"            "Quercusilex_coppice"    "Quercuspubescens"      
#> [19] "Quercusrobur"          
head(read_species(species_file("Fagussylvatica")))
#>     parameter value
#> 1   LIGHT_TOL 1.000
#> 2   PHENOLOGY 0.100
#> 3       ALPHA 0.057
#> 4 EPSILONgCMJ 0.690
#> 5 GAMMA_LIGHT 0.000
#> 6           K 0.710
```
