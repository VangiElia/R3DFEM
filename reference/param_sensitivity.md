# Sensitivity of the model to a species parameter

`param_sensitivity` runs the model with different values of one
parameter of the species file (with
[`run_batch`](https://vangielia.github.io/R3DFEM/reference/run_batch.md))
and plots the annual values of the selected output variables.

## Usage

``` r
param_sensitivity(
  param,
  value,
  variable,
  inputdir,
  outputdir,
  ...,
  parallel = FALSE
)
```

## Arguments

- param:

  Character: name of the parameter of the species file (e.g.
  "LIGHT_TOL").

- value:

  Numeric vector: values of the parameter.

- variable:

  Character vector: variables of the annual output to plot (e.g.
  c("GPP", "NPP")).

- inputdir:

  Character: input directory used by the runs without an `inputdir`
  column.

- outputdir:

  Character: directory of the runs. Each run is saved in
  outputdir/id/input and outputdir/id/output. Keep it short: the model
  cannot write paths longer than 260 characters.

- ...:

  Other arguments of
  [`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
  (at least site, species, year_start, year_end, man). The output is
  always annual.

- parallel:

  Logical: run the simulations in parallel with the future framework
  (package future.apply)? The future plan set by the user is used, e.g.
  `future::plan("multisession", workers = 4)`. Default to FALSE

## Value

A list with `data` (data.frame with YEAR, the value of the parameter,
variable and value) and `plot` (a ggplot object).

## See also

[`run_batch`](https://vangielia.github.io/R3DFEM/reference/run_batch.md),
[`set_species_param`](https://vangielia.github.io/R3DFEM/reference/read_species.md)

## Examples

``` r
if (FALSE) { # \dontrun{
inp <- system.file("extdata", "bilykriz", "rcp0p0", package = "R3DFEM")
s <- param_sensitivity("LIGHT_TOL", 1:4, c("GPP", "NPP", "HEIGHT"),
                       inputdir = inp, outputdir = "C:/sim/sens",
                       site = "BilyKriz", species = "Piceaabies",
                       year_start = 1997, year_end = 2050, man = "off")
s$plot
} # }
```
