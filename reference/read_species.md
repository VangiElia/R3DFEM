# Read and modify species parameterization files

`read_species` reads a species parameterization file (e.g.
Piceaabies.txt) as a data.frame. `set_species_param` changes the value
of one or more parameters, keeping comments and the order of the file,
and optionally writes the result in a new file (e.g. to create a new
parameterization or for sensitivity analysis).

## Usage

``` r
read_species(file)

set_species_param(file, ..., outfile = file)
```

## Arguments

- file:

  Character: path to the species file

- ...:

  Named values of the parameters to be changed, e.g.
  `LIGHT_TOL = 3, SLA_AVG0 = 8`

- outfile:

  Character: path of the file to be written. Default to `file` (the file
  is overwritten)

## Value

`read_species`: a data.frame with columns parameter and value.
`set_species_param`: invisibly, the previous values of the changed
parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
sp <- read_species("Piceaabies.txt")
set_species_param("Piceaabies.txt", LIGHT_TOL = 3, outfile = "Piceaabies_lt3.txt")
} # }
```
