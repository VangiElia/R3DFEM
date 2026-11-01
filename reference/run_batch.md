# Run many simulations of the 3DCMCCFEM model

`run_batch` runs a set of simulations (e.g. different sites, scenarios,
management options or species parameters), each one in its own
directory, sequentially or in parallel. The input files are copied for
each run, so the simulations do not interfere with each other.

## Usage

``` r
run_batch(
  runs,
  inputdir = NULL,
  outputdir,
  ...,
  parallel = FALSE,
  stop_on_error = FALSE
)
```

## Arguments

- runs:

  data.frame with one row per simulation. Columns named as the arguments
  of
  [`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
  (e.g. `site`, `year_end`, `man`, `co2`) set that argument for the run.
  A column `inputdir` gives a different input directory to each run
  (e.g. different sites). A column `id` names the runs (default run_001,
  run_002, ...). Any other column named as a parameter of the species
  file (e.g. `LIGHT_TOL`) changes that parameter, for all the species of
  the run, in the copy of the species files.

- inputdir:

  Character: input directory used by the runs without an `inputdir`
  column.

- outputdir:

  Character: directory of the runs. Each run is saved in
  outputdir/id/input and outputdir/id/output. Keep it short: the model
  cannot write paths longer than 260 characters.

- ...:

  Arguments of
  [`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
  common to all runs (e.g. `species`, `year_start`, `output`).

- parallel:

  Logical: run the simulations in parallel with the future framework
  (package future.apply)? The future plan set by the user is used, e.g.
  `future::plan("multisession", workers = 4)`. Default to FALSE

- stop_on_error:

  Logical: stop at the first failed simulation? Default to FALSE: errors
  are reported in the result.

## Value

A data.frame with the columns of `runs` and the columns `id`, `status`
("ok" or "error"), `message` (error message) and `output` (path of the
main output file, to be read with
[`read_output`](https://vangielia.github.io/R3DFEM/reference/read_output.md)).

## See also

[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md),
[`param_sensitivity`](https://vangielia.github.io/R3DFEM/reference/param_sensitivity.md)

## Examples

``` r
if (FALSE) { # \dontrun{
inp <- system.file("extdata", "bilykriz", "rcp0p0", package = "R3DFEM")
res <- run_batch(data.frame(co2 = c("on", "off")), inputdir = inp,
                 outputdir = "C:/sim/co2", site = "BilyKriz",
                 species = "Piceaabies", year_start = 1997, year_end = 2050,
                 man = "off", output = "annual")
out <- read_output(res$output)
} # }
```
