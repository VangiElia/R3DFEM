# The "Getting started" vignette runs the model, that is not available when the
# package is built (e.g. on GitHub Actions or CRAN). It is precomputed: run this
# script from the vignettes directory, with the model installed (install_model()),
# after editing R3DFEM.Rmd.orig.
knitr::knit("R3DFEM.Rmd.orig", output = "R3DFEM.Rmd")
