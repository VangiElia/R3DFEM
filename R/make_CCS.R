#' Create detrended meteo input file for the current-climate scenario (CCS).
#'
#' \code{make_CCS} use an historical meteo input file to generate a new,
#' detrended meteo input file for the future (called current-climate scenario).
#'
#' @param meteo Character or data.frame: Historical meteo data or path to a
#'   meteo.txt file. Must have columns "Year", "Month", "n_days", "Rg_f",
#'   "Ta_f", "Tmax", "Tmin", "RH_f", "Ts_f", "Precip", "SWC", "LAI", "ET",
#'   "WS_f" (see details). Can be created for European sites with the function
#'   \code{\link{make_meteo_EOBS}}, using E-OBS data
#'
#' @param co2 Character or data.frame: Historical CO2 data or path to co2.txt
#'   file. Must have columns "year" and "CO2_ppm"
#' @param obs_year Numeric vector of length 2: first and last year of historical
#'   meteo and CO2 data to be used to generate the future detrended weather.
#'   Must be present in both meteo and co2 data.frame
#' @param end_year Numeric: Last year of simulated meteo and co2 data. Default
#'   to 2099
#' @param outdir Character: path to the output directory. If does not exist it
#'   will be created
#' @param keep_name Logical: if TRUE the name of the detrended file will be the
#'   same of the the original one, otherwise the prefix "detrended_" will be add
#'
#' @details The input meteo data.frame must have the columns:
#'   \itemize{
#'   \item \emph{Year}: Reference year for the meteo data
#'   \item \emph{Month}: number of month in the year
#'   \item \emph{n_day}: number of day in the month
#'   \item \emph{Rg_f}: Solar radiation on the ground (in  WJ/m-2)
#'   \item \emph{Ta_f}: Mean temperature (in °C)
#'   \item \emph{Tmax}: Maximum temperature (in °C)
#'   \item \emph{Tmin}: Minimum temperature (in °C)
#'   \item \emph{RH_f}: relative humidity (in %)
#'   \item \emph{Ts_f}: Daily soil temperature (in °C)
#'   \item \emph{Precip}: Daily precipitation (in mm day-1)
#'   \item \emph{SWC}:Soil Water Content (in mm m-2)*
#'   \item \emph{LAI}:Leaf Area Index (in m2 m-2) *
#'   \item \emph{ET}: Evapotranspiration (in mm m-2 day-1*
#'   \item \emph{WS_f}: Wind speed (in m s-1)*
#'   }
#'
#'   The input co2 data.frame must have the colums:
#'   \itemize{
#'   \item \emph{Year}: Reference year for the co2 data
#'   \item \emph{CO2_ppm}: CO2 concentration (in ppm)
#'   }
#'
#'   All the year of simulation must be included in the meteo and co2 input
#'   files. For more info on the stand input data needed see the official \href{
#'   http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'   Guide}
#'
#' @return Used for its side effects.It save in \code{outdir} a meteo.txt and
#'   co2.txt file with the same columns of the input data
#' @seealso \code{\link{run_3DFEM}}
#' @export

make_CCS <-
  function(meteo = NULL,
           co2 = NULL,
           obs_year = NULL,
           end_year = 2099,
           outdir = NULL,
           keep_name=TRUE) {

    # args check --------------------------------------------------------------

    check_dir(outdir)
    stopifnot(
      "obs_year must be a numeric vector of length 2" = check_len_num(obs_year,len =2),
      "end_year must be a number" = check_num(end_year)
    )

    if (!is.null(meteo)) {
      if (class(meteo) == "character") {
        if(keep_name){
          outname <- file.path(outdir,basename(meteo))
        }else{
          outname <- file.path(outdir,paste0("detrended_",basename(meteo)))
        }
        meteo <- read.table(meteo, header = T, sep = "\t")
      } else if (class(meteo) %in% c("data.frame", "data.table", "tbl_df", "tbl")) {
        meteo <- meteo
        outname <-  file.path(outdir, "detrended_meteo.txt")
      } else{
        stop("meteo is of the wrong class")
      }
    }
    if (!is.null(meteo)) {
      range_year <- range(meteo$Year)
      stopifnot(
        "First year of observation must be >= of first year in meteo data" = obs_year[1] >=
          range_year[1],
        "Last year of observation must be <= of last year in meteo data" =
          obs_year[2] <= range_year[2]
      )
    }

    if (!is.null(co2)) {
      if (class(co2) == "character") {
        if(keep_name){
          outname_co2 <- file.path(outdir,basename(co2))
        }else{
          outname_co2 <- file.path(outdir,paste0("detrended_",basename(co2)))
        }
        co2 <- read.table(co2, header = T)
      } else if (class(co2) %in% c("data.frame", "data.table", "tbl_df", "tbl")) {
        co2 <- co2
        outname_co2 <-  file.path(outdir, "detrended_co2.txt")
      } else{
        stop("co2 is of the wrong class")
      }
    }
    if (!is.null(co2)) {
      range_year <- range(co2$year)
      stopifnot(
        "First year of observation must be >= of first year in CO2 data" = obs_year[1] >=
          range_year[1],
        "Last year of observation must be <= of last year in CO2 data" =
          obs_year[2] <= range_year[2]
      )
    }
    # - -----------------------------------------------------------------------

    set.seed(4)

    # definisce anni per resampling
    lista_year = obs_year[1]:obs_year[2]
    lista_year = data.frame('year' = lista_year)
    lista_year$isleap = lubridate::leap_year(lista_year$year)

    # sample anni random dagli anni osservati da far corrispondere agli anni da obs_year[2] a end_year
    #considerando gli anni bisestili

    anni_random <- c()

    for (cy in seq(obs_year[2]+1,end_year)) {
      anni_random = c(anni_random,sample(lista_year$year[lista_year$isleap == lubridate::leap_year(cy)])[1])
    }

    anni_random = data.frame('anni_or' = anni_random,
                             'anni_new' = (obs_year[2] + 1):end_year)

    # internal data loading ---------------------------------------------------

    #METEO
    if (!is.null(meteo)) {
      meteo_or <- meteo

      meteo_random <-
        do.call(rbind, lapply(seq_along(anni_random$anni_or), function(i) {
          tmp <- meteo_or[meteo_or$Year == anni_random$anni_or[i], ]
          tmp$Year <- anni_random$anni_new[i]
          return(tmp)
        }))

      meteo_random <- rbind(meteo,meteo_random)

      write.table(
        meteo_random,
        file = outname,
        quote = F,
        sep = '\t',
        row.names = F
      )
      message(outname," saved!")

    }
    #CO2
    if (!is.null(co2)) {
      co2_or <- co2
      co2_random <-
        do.call(rbind, lapply(seq_along(anni_random$anni_or), function(i) {
          tmp <- co2_or[co2_or$year == anni_random$anni_or[i], ]
          tmp$year <- anni_random$anni_new[i]
          return(tmp)
        }))

      co2_random <- rbind(co2,co2_random)

      write.table(
        co2_random,
        file = outname_co2,
        quote = F,
        sep = '\t',
        row.names = F
      )
      message(outname_co2," saved!")

    }

}

#make_CCS(meteo="E:/meteo.txt",co2="E:/co2.txt",obs_year=c(2010,2022),end_year = 2099,outdir="E:/xxx")
