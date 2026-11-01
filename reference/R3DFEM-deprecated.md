# Deprecated functions

These functions are kept for compatibility with R3DFEM 0.1 and will be
removed in a future version.

- `run_3DCMCCFEM_5.7`: use
  [`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md),
  that now runs the version 5.7 of the model.

- `make_meteo_EOBS_30.0`: use
  [`make_meteo_EOBS`](https://vangielia.github.io/R3DFEM/reference/make_meteo_EOBS.md)
  with `version = "30.0"` (the default).

## Usage

``` r
run_3DCMCCFEM_5.7(...)

make_meteo_EOBS_30.0(...)
```

## Arguments

- ...:

  Arguments passed to the new function.

## Value

The value of the new function.
