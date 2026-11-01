test_that("make_stand writes a valid stand file", {
  dir <- withr::local_tempdir()
  st <- make_stand("S", year = c(2000, 2000), age = c(20, 40), species = "Piceaabies",
                   n_trees = c(500, 300), avdbh = c(15, 30), height = c(12, 25), outdir = dir)
  expect_equal(nrow(st), 2)
  f <- read.csv(file.path(dir, "S_stand.txt"))
  expect_named(f, c("Year", "x", "y", "Age", "Species", "Management", "N", "Stool", "AvDBH", "Height",
                    "Wf", "Wrc", "Wrf", "Ws", "Wbb", "Wres", "Lai"))
  expect_error(make_stand("S", 2000, 20, "Picea abies", 500, 15, 12, outdir = dir), "spaces")
  expect_error(make_stand("S", 2000, 20, "Piceaabies", -1, 15, 12, outdir = dir), "n_trees")
  expect_error(make_stand("S", 2000, c(20, 30, 40), "Piceaabies", c(1, 2), 15, 12, outdir = dir), "length")
})

test_that("make_co2 interpolates and recycles", {
  dir <- withr::local_tempdir()
  co <- make_co2("S", 2000:2010, data.frame(year = c(2000, 2010), CO2_ppm = c(370, 390)), outdir = dir)
  expect_equal(co$CO2_ppm[6], 380)
  expect_equal(make_co2("S", 2000:2002, 400, outdir = dir)$CO2_ppm, rep(400, 3))
  expect_error(make_co2("S", 2000:2002, -1, outdir = dir), "positive")
  expect_warning(make_co2("S", 1990:2000, data.frame(year = 2000, CO2_ppm = 370), outdir = dir), "outside")
})

test_that("make_management writes one row per action", {
  dir <- withr::local_tempdir()
  make_management("S", thinning = c(2030, 2050), thinning_int = c(30, 20), harvesting = 2080, outdir = dir)
  expect_equal(readLines(file.path(dir, "S_management.txt")),
               c("Thinning,2030,2050", "Thinning_int,30,20", "Harvesting,2080"))
  expect_error(make_management("S", thinning = 2030, harvesting = 2030, outdir = dir), "same year")
  expect_error(make_management("S", thinning = 2030, thinning_int = c(1, 2), outdir = dir), "same length")
})

test_that("make_soil checks the texture", {
  dir <- withr::local_tempdir()
  s <- make_soil("S", lat = 45, lon = 11, clay = 20, silt = 30, sand = 50, outdir = dir)
  expect_equal(s$SOIL_DEPTH, 100)
  expect_error(make_soil("S", lat = 45, lon = 11, clay = 20, silt = 30, sand = 60, outdir = dir), "100")
})

test_that("make_meteo converts daily data", {
  dir <- withr::local_tempdir()
  d <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
  df <- data.frame(date = d, Rg_f = 150, Tmax = 15, Tmin = 5, Precip = 1, RH_f = 70)
  m <- make_meteo(df, site = "S", rad_unit = "W", outdir = dir)
  expect_equal(nrow(m), 365)
  expect_equal(m$Ta_f[1], 10)
  expect_equal(m$Rg_f[1], 150 * 0.0864)
  expect_equal(colnames(read.delim(file.path(dir, "S_meteo.txt")))[1:3], c("Year", "Month", "n_days"))
  expect_error(make_meteo(df[-10, ], site = "S", outdir = dir), "missing day")
})

test_that("set_species_param changes values and keeps comments", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "Fagussylvatica.txt")
  file.copy(species_file("Fagussylvatica"), f)
  old <- set_species_param(f, LIGHT_TOL = 3, SLA_AVG0 = 25.5)
  expect_equal(unname(old["LIGHT_TOL"]), 1)
  sp <- read_species(f)
  expect_equal(sp$value[sp$parameter == "LIGHT_TOL"], 3)
  expect_equal(sp$value[sp$parameter == "SLA_AVG0"], 25.5)
  expect_equal(length(readLines(f)), length(readLines(species_file("Fagussylvatica"))))
  expect_true(any(grepl("//", readLines(f)[grepl("^LIGHT_TOL", readLines(f))])))
  expect_error(set_species_param(f, NOT_A_PARAM = 1), "not found")
})

test_that("setup_inputs creates a complete input directory", {
  dir <- file.path(withr::local_tempdir(), "in")
  bk <- bilykriz()
  f <- setup_inputs("Test", dir,
                    stand = list(year = 1997, age = 16, species = "Piceaabies", n_trees = 2408,
                                 avdbh = 7.11, height = 5.63),
                    soil = file.path(bk, "BilyKriz_soil.txt"),
                    topo = 875,
                    meteo = file.path(bk, "BilyKriz_meteo.txt"),
                    co2 = file.path(bk, "BilyKriz_co2.txt"),
                    management = list(thinning = 2010))
  expect_setequal(basename(f), c("Test_stand.txt", "Test_soil.txt", "Test_topo.txt", "Test_meteo.txt",
                                 "Test_co2.txt", "Test_management.txt", "Piceaabies.txt"))
  expect_true(all(file.exists(f)))
  expect_true(check_inputs("Test", "Piceaabies", 1997, 2010, man = "off", inputdir = dir))
  expect_error(setup_inputs("T2", dir, stand = list(year = 1997, age = 1, species = "Unknown", n_trees = 1,
                                                    avdbh = 1, height = 1),
                            soil = f[["soil"]], topo = 1, meteo = f[["meteo"]], co2 = f[["co2"]]),
               "No parameterization")
})
