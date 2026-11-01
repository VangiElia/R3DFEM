# Create stand input file for 3DCMCCFEM model

`make_stand` writes the site_stand.txt file from user-provided values,
following the 3D-CMCC-FEM model specifications. Each element of the
vectors is a row of the stand file, i.e., a class (species, age, DBH,
height) in a given year.

## Usage

``` r
make_stand(
  site,
  year,
  age,
  species,
  n_trees,
  avdbh,
  height,
  management = "T",
  x = 0,
  y = 0,
  stool = 0,
  wf = 0,
  wrc = 0,
  wrf = 0,
  ws = 0,
  wbb = 0,
  wres = 0,
  lai = -9999,
  species_dir = NULL,
  outdir
)
```

## Arguments

- site:

  Character: Name of the site. Used to construct output name

- year:

  Integer vector: reference year(s) of the stand data. The first year
  must be the `year_start` of the simulation. More years can be used
  with `man = "var"` and `pres_dens = "on"`.

- age:

  Numeric vector: age of the class (years)

- species:

  Character vector: species of the class. Must be the name of the
  parameterization file (without .txt), e.g. "Piceaabies"

- n_trees:

  Numeric vector: number of trees in the cell

- avdbh:

  Numeric vector: average DBH of the class (cm)

- height:

  Numeric vector: average height of the class (m)

- management:

  Character vector: "T" (timber) or "C" (coppice). Default to "T"

- x, y:

  Numeric: cell position. Default to 0

- stool:

  Numeric vector: number of stools (coppice). Default to 0

- wf, wrc, wrf, ws, wbb, wres:

  Numeric vectors: initial biomass of foliage, coarse roots, fine roots,
  stem, branch and bark, reserves. Default to 0 (computed by the model)

- lai:

  Numeric vector: LAI of the class. Default to -9999 (computed by the
  model)

- species_dir:

  Character: optional path to the directory with the species
  parameterization files. If given, the existence of the files is
  checked

- outdir:

  Character: path to the output directory.

## Value

Invisibly, the stand data.frame. The file site_stand.txt is saved in
`outdir`.

## See also

[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md),
[`virtual_stand_generator`](https://vangielia.github.io/R3DFEM/reference/virtual_stand_generator.md)

## Examples

``` r
if (FALSE) { # \dontrun{
make_stand("BilyKriz", year = 1997, age = 16, species = "Piceaabies",
           n_trees = 2408, avdbh = 7.11, height = 5.63, outdir = tempdir())
} # }
```
