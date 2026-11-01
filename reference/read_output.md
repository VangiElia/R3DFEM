# Read an output file of the 3DCMCCFEM model

`read_output` reads a daily, monthly or annual output of
[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
as a data.frame, dropping the model settings written by the model at the
end of the file.

## Usage

``` r
read_output(file, date = TRUE)
```

## Arguments

- file:

  Character: path to an output file, e.g. one of the paths returned by
  [`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md).
  Several paths can be given (e.g. the outputs of
  [`run_batch`](https://vangielia.github.io/R3DFEM/reference/run_batch.md)):
  the outputs are bound by row and a column `file` identifies them.

- date:

  Logical: add a column DATE (Date class) for daily and monthly outputs
  (first day of the month)? Default to TRUE

## Value

A data.frame. The species column is always named SPECIES (the model
writes SPECIE in the annual output). Column names are kept as written by
the model (e.g. "BP/NPP").

## See also

[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md),
[`plot_output_3DFEM`](https://vangielia.github.io/R3DFEM/reference/plot_output_3DFEM.md)
