#' Create soil input file for 3DCMCCFEM model
#'
#' \code{make_soil_ISRIC} use the
#' \href{https://www.isric.org/explore/soilgrids}{soilGrids} from the ISRIC
#' database to create the soil input file according to the 3DCMCCFEM model
#' specifications.
#'
#' @param plot Sf object or something that can be converted to an sf point object
#'   (i.e., a data.frame of coordinates). If plot is a data.frame the first two
#'   colums must specify X and Y coordinates. if plot ia a data.frame the
#'   argument crs must be specified.
#' @param crs Numeric: a valid EPSG code
#' @param buffer Numeric: buffer around plot to extract meteo data from
#' @param id Character: name of the column in \code{plot} that identifies different
#'   points
#' @param site Character: the name of the plot site. Used to construct the output name
#' @param outdir Character: path to the output directory.
#' @param return_df Logical: should the climate data be returned by the function
#'   as a data.frame? Default to FALSE
#' @param isric_file Character: path of the SoilGrids raster (ISRIC.tif, not
#'   distributed with the package). Default to the option
#'   \code{R3DFEM.isric_file} or, if not set, to the extdata directory of the
#'   installed package.
#'
#' @details This function compute the mean percentage of sand, silt and
#'   clay in the first meter of soil, weighted by the thickness of the
#'   SoilGrids layers (0-5, 5-15, 15-30, 30-60, 60-100 cm). The file is
#'   written with \code{\link{make_soil}}. For more information on soil file needed
#'   by the model please refer to the \href{http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'    Guide}
#'
#' @return Used for its side effects. It saves in \code{outdir} a file
#'   site_soil.txt. The most important columns in this file are:
#'   \itemize{
#'   \item \emph{X}: x cell position
#'   \item \emph{Y}: y cell position
#'   \item \emph{LANDUSE}: "F" by default: Forest land use.
#'   \item \emph{LAT}: latitude of point
#'   \item \emph{LON}: longitude of point
#'   \item \emph{CLAY_PERC}: soil clay (in %)
#'   \item \emph{SILT_PERC}: soil silt (in %)
#'   \item \emph{SAND_PERC}: soil sand (in %)
#'   \item \emph{SOIL_DEPTH}: soil depth (in cm), 100 by default
#'   \item \emph{FR}:0.9 by default. Fertility rating (dim) (only LUE version)
#'   \item \emph{FN0}:0.5 by default. Value of fertility modifier when FR=0 (dim) (only LUE version)
#'   \item \emph{M0}:0.2 by default. Value of ‘m’ when FR=0 (dim) (only LUE version)
#'   }
#' @seealso \code{\link{make_soil}}, \code{\link{run_3DCMCCFEM}}
#' @export

make_soil_ISRIC <- function(plot,crs=NULL,buffer=100,id=NULL,site=NULL,outdir=NULL,return_df=FALSE,
                            isric_file=getOption("R3DFEM.isric_file", system.file("extdata", "ISRIC.tif", package = "R3DFEM"))){

  check_dir(outdir)
  stopifnot(
    "buffer must be a number" =  check_num(buffer)
  )

  if(is.null(site)&&!is.null(id)){
    warning("Argument 'site' is missing with no default. It will be dropped in the file name and id will be used instead.")
  }

  if(is.null(site)&&is.null(id)){
    warning("Argument 'site' is missing with no default and id is NULL. A temporary site name will be used.")
    site <- "tmp"
  }

  if(!inherits(plot,"sf")){
    if(is.null(crs)){
      stop("If argument 'plot' is not an sf object, the crs of coordinates must be given")
    }
    if(is.na(sf::st_crs(crs))){
      stop("Argument 'crs' should be something suitable as input to sf::st_crs")
    }
    plot <- sf::st_as_sf(as.data.frame(plot),coords = c(1,2),crs=crs)
  }

  if(nrow(plot)!=1 && !check_char(id)){
    stop("Argument 'id' must be a character vector of length 1")
  }

  plot <- sf::st_transform(plot,4326)
  latlon <- as.data.frame(sf::st_coordinates(plot))
  plot <- sf::st_buffer(plot,buffer)
  if (!nzchar(isric_file) || !file.exists(isric_file)) {
    stop("SoilGrids raster not found. Set options(R3DFEM.isric_file = ...) or the argument isric_file")
  }
  rst <- terra::rast(isric_file)

  soil <- exactextractr::exact_extract(rst, plot, "mean", append_cols = id, progress = FALSE)
  gc()

  # thickness (cm) of the SoilGrids layers 0-5, 5-15, 15-30, 30-60, 60-100 cm
  thickness <- c(5,10,15,30,40)
  wmean <- function(var) {
    cols <- grep(paste0("^mean\\.", var, "_"), colnames(soil), value = TRUE)
    as.vector(as.matrix(soil[, cols]) %*% thickness / sum(thickness))
  }
  clay <- wmean("clay")
  sand <- wmean("sand")
  silt <- wmean("silt")
  tot <- clay + sand + silt

  idd <- if (!is.null(id)) soil[[id]] else NULL
  out <- lapply(seq_len(nrow(soil)), function(i) {
    name <- if (is.null(id)) site else if (is.null(site)) idd[i] else paste0(site, "_", idd[i])
    make_soil(site = name, lat = latlon[i, 2], lon = latlon[i, 1],
              clay = clay[i] / tot[i] * 100, silt = silt[i] / tot[i] * 100, sand = sand[i] / tot[i] * 100,
              soil_depth = 100, outdir = outdir)
  })
  soil <- do.call(rbind, out)
  if (!is.null(id)) soil$id <- idd

  if(return_df){
    return(soil)
  }
  invisible(soil)
}

