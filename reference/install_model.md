# Install and locate the 3D-CMCC-FEM executable

The model executable is not shipped with the package. `install_model`
downloads it (or copies a local file) in a user directory, once;
`model_path` returns the executable that will be used by
[`run_3DCMCCFEM`](https://vangielia.github.io/R3DFEM/reference/run_3DCMCCFEM.md)
and `model_available` tells whether one is available.

## Usage

``` r
install_model(source = .model_url(), dir = .model_dir(), overwrite = FALSE)

model_path()

model_available()
```

## Arguments

- source:

  Character: URL or path of a local file of the executable. Default to
  the Windows build of the version 5.7 published in the GitHub releases
  of R3DFEM (error on other systems). On Linux and macOS the model must
  be compiled from the [source
  code](https://github.com/Forest-Modelling-Lab/3D-CMCC-FEM) and the
  path of the binary given here (or with the option below).

- dir:

  Character: directory where the executable is installed. Default to
  `tools::R_user_dir("R3DFEM", "data")` (option `R3DFEM.model_dir`).

- overwrite:

  Logical: overwrite an executable already installed? Default to FALSE

## Value

`install_model`: invisibly, the path of the installed executable.
`model_path`: the path of the executable (error if not found).
`model_available`: TRUE or FALSE.

## Details

The executable used by the package is searched, in order, in:

1.  the option `R3DFEM.exe`, e.g.
    `options(R3DFEM.exe = "C:/models/3D_CMCC_Forest_Model_5.7.exe")`;

2.  the environment variable `R3DFEM_EXE`;

3.  the executable installed by `install_model` in `dir`.

## Examples

``` r
model_available()
#> [1] FALSE
if (FALSE) { # \dontrun{
install_model()
# or use an executable already on disk
install_model("C:/models/3D_CMCC_Forest_Model_5.7.exe")
model_path()
} # }
```
