test_that("run_3DCMCCFEM runs the model and returns the outputs", {
  skip_if_no_model()
  inp <- local_inputs()
  out <- short_tempdir()
  wd <- getwd()
  o <- suppressMessages(run_3DCMCCFEM(site = "BilyKriz", species = "Piceaabies", year_start = 1997,
                                      year_end = 2000, man = "off", output = "monthly",
                                      inputdir = inp, outputdir = file.path(out, "output")))
  expect_identical(getwd(), wd)
  expect_equal(names(o)[1], "monthly")
  expect_true(all(file.exists(o)))
  m <- read_output(o[["monthly"]])
  expect_equal(nrow(m), 4 * 12)
  expect_true(all(m$GPP >= 0))
})

test_that("run_3DCMCCFEM with management and replanting", {
  skip_if_no_model()
  inp <- local_inputs()
  make_management("BilyKriz", thinning = c(2005, 2015), harvesting = 2025, outdir = inp)
  # the stand file has more years: only year_start is used with pres_dens = "off"
  expect_warning(
    o <- suppressMessages(run_3DCMCCFEM(site = "BilyKriz", species = "Piceaabies", year_start = 1997,
                                        year_end = 2035, man = "var", pres_dens = "off", output = "annual",
                                        rep_trees = 2000, rep_age = 3, rep_dbh = 1, rep_lai = 0.5,
                                        rep_height = 1.3, rep_species = "Piceaabies",
                                        inputdir = inp, outputdir = file.path(short_tempdir(), "output"))),
    "Multiple years")
  a <- read_output(o[["annual"]])
  expect_true(any(diff(a$AGE) < 0))
})

test_that("run_batch runs several simulations and reports errors", {
  skip_if_no_model()
  out <- short_tempdir()
  res <- suppressMessages(suppressWarnings(
    run_batch(data.frame(co2 = c("on", "off"), year_end = c(2000, 2500)), inputdir = bilykriz(),
              outputdir = out, site = "BilyKriz", species = "Piceaabies", year_start = 1997,
              man = "off", output = "annual")))
  expect_equal(res$status, c("ok", "error"))
  expect_match(res$message[2], "time span")
  expect_equal(nrow(read_output(res$output[1])), 4)
  # the example inputs are not modified
  expect_false(any(grepl("settings", list.files(bilykriz()))))
})

test_that("run_batch changes species parameters", {
  skip_if_no_model()
  res <- suppressMessages(run_batch(data.frame(LIGHT_TOL = c(1, 4)), inputdir = bilykriz(),
                                    outputdir = short_tempdir(), site = "BilyKriz",
                                    species = "Piceaabies", year_start = 1997, year_end = 2005,
                                    man = "off", output = "annual"))
  expect_equal(res$status, c("ok", "ok"))
  used <- read_species(file.path(dirname(dirname(res$output[2])), "input", "Piceaabies.txt"))
  expect_equal(used$value[used$parameter == "LIGHT_TOL"], 4)
  g <- vapply(res$output, function(f) sum(read_output(f)$GPP), 0)
  expect_false(isTRUE(all.equal(g[[1]], g[[2]])))
})
