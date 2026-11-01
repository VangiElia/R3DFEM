# Create topo input file for 3DCMCCFEM model

`make_topo` create the topo.txt file following the 3D-CMCC-FEM model
specification

## Usage

``` r
make_topo(site = NULL, elev, outdir)
```

## Arguments

- site:

  Character: Name of the site. Used to construct output name

- elev:

  Numeric vector: elevation of the site

- outdir:

  Character: path to the output directory.

## Value

Used for its side effects.It save in `outdir` as many topo input files
as the length of `elev` provided.

## Details

The function can accept a vector of elevation and return as many files
as the length of the `elev` vector.The output topo.txt file has the
columns:

- *X*: x cell position

- *Y*: y cell position

- *ELEV*: Elevation of the stand

For more info on the topo input data needed see the official [User's
Guide](http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf)

## See also

[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
