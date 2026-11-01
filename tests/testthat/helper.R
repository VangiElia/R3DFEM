# directory with the BilyKriz example inputs
bilykriz <- function() system.file("extdata", "bilykriz", "rcp0p0", package = "R3DFEM")

# copy of the example inputs in a temporary directory
local_inputs <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(list.files(bilykriz(), full.names = TRUE), dir)
  dir
}

# the simulations need the model executable (see install_model()) and a
# short path (the model cannot write paths longer than 260 characters)
skip_if_no_model <- function() {
  skip_on_cran()
  skip_if_not(model_available(), "3D-CMCC-FEM executable not installed")
}

short_tempdir <- function(env = parent.frame()) {
  base <- Sys.getenv("R3DFEM_TEST_DIR", tempdir())
  dir <- file.path(base, paste0("t", as.integer(stats::runif(1, 1e5, 1e6))))
  dir.create(dir, recursive = TRUE)
  withr::defer(unlink(dir, recursive = TRUE), envir = env)
  dir
}
