#' Create meteo input file for 3DCMCCFEM model from E-OBS
#'
#' \code{make_meteo_EOBS} uses the
#' \href{https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles/}{E-OBS
#' database} to create the meteo input file according to the 3DCMCCFEM model
#' specifications. The time period covered is 1950-01-01 - 2024-06-30 for the
#' version 30.0 and 1950-01-01 - 2023-12-31 for the version 29.0.
#'
#' @param plot Sf object or something that can be converted to an sf point object
#'   (i.e., a data.frame of coordinates). If plot is a data.frame the argument
#'   crs must be specified.
#' @param crs Numeric: a valid EPSG code
#' @param buffer Numeric: buffer around plot to extract meteo data from
#' @param id Character: name of the column in \code{plot} that identifies different
#'   points
#' @param daterange Character vector of length 2: starting and ending date from
#'   which to extract climate data. Must be in the form yyyy-mm-dd. Default to
#'   the whole period of the E-OBS version.
#' @param convert_rad Logical: Should the solar radiation be converted from
#'   W m-2 (E-OBS unit) to MJ m-2 day-1 (model unit)? Default to TRUE.
#' @param parallel Logical: Should the extraction be performed in parallel? Default to FALSE
#' @param site Character: the name of the plot site. Used to construct the output name
#' @param outdir Character: path to the output directory.
#' @param return_df Logical: should the climate data be returned by the function
#'   as a data.frame? Default to FALSE
#' @param version Character: version of E-OBS, "30.0" or "29.0". Default to
#'   "30.0"
#' @param eobs_dir Character: directory with the E-OBS netCDF files (ensemble
#'   mean, 0.1 deg, e.g. tg_ens_mean_0.1deg_reg_v30.0e.nc) of the variables
#'   hu, qq, rr, tg, tn and tx. Default to the option \code{R3DFEM.eobs_dir}
#'   or, if not set, to the extdata directory of the installed package.
#'
#' @details The E-OBS files are not distributed with the package: download
#'   them from the Copernicus web site and set, e.g.,
#'   \code{options(R3DFEM.eobs_dir = "D:/EOBS")}. If parallel=TRUE, the user
#'   can set a future plan, otherwise the function will use a 7-workers
#'   \code{future::plan("multisession")} with \code{\link[foreach]{foreach}}
#'   and the doFuture package. With \code{id}, one file per point is written,
#'   named id_meteo.txt (site_id_meteo.txt if \code{site} is given), as in
#'   \code{\link{make_soil_ISRIC}}. For more information on meteo file needed
#'   by the model please refer to the
#'   \href{http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'    Guide}
#'
#' @return Used for its side effects. It saves in \code{outdir} a file
#'   site_meteo.txt The output file have the colums:
#'   \itemize{
#'   \item \emph{Year}: Reference year for the meteo data
#'   \item \emph{Month}: number of month in the year
#'   \item \emph{n_day}: number of day in the month
#'   \item \emph{Rg_f}: Solar radiation on the ground (in MJ m-2 day-1)
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
#' @seealso \code{\link{run_3DCMCCFEM}}, \code{\link{make_meteo}}
#' @export

make_meteo_EOBS <-
  function(plot,
           crs = NULL,
           buffer = 100,
           id = NULL,
           daterange = NULL,
           convert_rad = TRUE,
           parallel = FALSE,
           site = NULL,
           outdir = NULL,
           return_df = FALSE,
           version = c("30.0", "29.0"),
           eobs_dir = getOption("R3DFEM.eobs_dir", system.file("extdata", package = "R3DFEM"))) {

    version <- match.arg(version)
    last_date <- c("30.0" = "2024-06-30", "29.0" = "2023-12-31")[[version]]
    if (is.null(daterange)) daterange <- c("1950-01-01", last_date)
    if (is.null(site) && !is.null(id)) site <- ""
    .make_meteo_EOBS(plot = plot, crs = crs, buffer = buffer, id = id, daterange = daterange,
                     convert_rad = convert_rad, parallel = parallel, site = site, outdir = outdir,
                     return_df = return_df, version = version, last_date = last_date,
                     eobs_dir = eobs_dir,
                     file_name = function(site, id) {
                       paste0(paste(c(if (nzchar(site)) site, id), collapse = "_"), "_meteo.txt")
                     })
  }
