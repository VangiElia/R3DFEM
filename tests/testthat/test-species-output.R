test_that("all the species files of the package are complete for the version 5.7", {
  sp <- list_species()
  expect_true(length(sp) >= 19)
  params <- lapply(species_file(sp), function(f) read_species(f)$parameter)
  for (s in sp) {
    expect_true("CONV_VOL_FACTOR" %in% params[[s]], info = s)
    expect_setequal(params[[s]], params[["Piceaabies"]])
    expect_false(anyNA(read_species(species_file(s))$value), info = s)
  }
})

test_that("species_file and copy_species", {
  expect_error(species_file("Unknown"), "No parameterization")
  dir <- withr::local_tempdir()
  copy_species(c("Abiesalba", "Larixdecidua"), dir)
  expect_setequal(list.files(dir), c("Abiesalba.txt", "Larixdecidua.txt"))
  # a site-specific file is kept
  writeLines("LIGHT_TOL 9", file.path(dir, "Abiesalba.txt"))
  expect_message(copy_species("Abiesalba", dir), "Kept")
  expect_equal(readLines(file.path(dir, "Abiesalba.txt")), "LIGHT_TOL 9")
})

test_that("read_output reads annual and monthly outputs", {
  a <- read_output(test_path("fixtures", "annual.txt"))
  expect_equal(nrow(a), 14)
  expect_true(all(c("YEAR", "SPECIES", "GPP", "DBH", "BP/NPP") %in% colnames(a)))
  expect_false("SPECIE" %in% colnames(a))
  m <- read_output(test_path("fixtures", "monthly.txt"))
  expect_equal(nrow(m), 14 * 12)
  expect_s3_class(m$DATE, "Date")
  expect_equal(m$DATE[1], as.Date("1997-01-01"))
  both <- read_output(test_path("fixtures", c("annual.txt", "monthly.txt")))
  expect_equal(nrow(both), 14 + 14 * 12)
  expect_true("file" %in% colnames(both))
  expect_error(read_output("not_a_file.txt"), "not found")
})

test_that("check_inputs finds inconsistencies", {
  inp <- local_inputs()
  expect_true(check_inputs("BilyKriz", "Piceaabies", 1997, 2010, man = "off", inputdir = inp))
  expect_error(check_inputs("BilyKriz", "Piceaabies", 1990, 2010, man = "off", inputdir = inp), "year_start")
  expect_error(check_inputs("BilyKriz", "Piceaabies", 1997, 2500, man = "off", inputdir = inp), "time span")
  expect_error(check_inputs("Nowhere", "Piceaabies", 1997, 2010, man = "off", inputdir = inp), "Missing input")
  expect_error(check_inputs("BilyKriz", "Piceaabies", 1997, 2010, man = "var", inputdir = inp), "pres_dens")
  # 5.6 species file without CONV_VOL_FACTOR
  sp <- readLines(file.path(inp, "Piceaabies.txt"))
  writeLines(sp[!grepl("^CONV_VOL_FACTOR", sp)], file.path(inp, "Piceaabies.txt"))
  expect_error(check_inputs("BilyKriz", "Piceaabies", 1997, 2010, man = "off", inputdir = inp), "CONV_VOL_FACTOR")
})

test_that("settings table and run options", {
  s <- R3DFEM:::.settings_table(list(A = "on", B = 1e-4, C = 2010))
  expect_equal(s$Values, c("on", "0.0001", "2010"))
  expect_error(R3DFEM:::.check_run_options("on", "maybe", NULL, NULL, "off", "annual"), "man")
  expect_error(R3DFEM:::.check_run_options("on", "off", NULL, NULL, "on", "annual"), "reg")
  expect_true(R3DFEM:::.uses_management("var", "off", NULL, "off"))
  expect_false(R3DFEM:::.uses_management("var", "on", "off", "off"))
})

test_that("model executable is found through the options", {
  withr::local_options(R3DFEM.exe = NULL, R3DFEM.model_dir = withr::local_tempdir())
  withr::local_envvar(R3DFEM_EXE = "")
  expect_false(model_available())
  expect_error(model_path(), "install_model")
  fake <- withr::local_tempfile(fileext = ".exe")
  writeBin(as.raw(1:10), fake)
  install_model(fake)
  expect_true(model_available())
  expect_message(install_model(fake), "already installed")
  withr::local_options(R3DFEM.exe = fake)
  expect_equal(model_path(), normalizePath(fake, winslash = "/"))
})
