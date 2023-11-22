#' Create soil input file for 3DCMCCFEM model
#'
#' \code{make_soil_ISRIC} use the
#' \href{https://www.isric.org/explore/soilgrids}{soilGrids} from the ISRIC
#' database to create the soil input file according to the 3DCMCCFEM model
#' specifications.
#'
#' @param plot Sf object or something that can be converted to an sf point object
#'   (i.e., a data.frame of coordinates). If plot is a data.frame the argument
#'   crs must be specified.
#' @param crs Numeric: a valid EPSG code
#' @param buffer Numeric: buffer around plot to extract meteo data from
#' @param id Character: name of the column in \code{plot} that identifies different
#'   points
#' @param site Character: the name of the plot site. Used to construct the output name
#' @param outdir Character: path to the output directory.
#' @param return_df Logical: should the climate data be returned by the function
#'   as a data.frame? Default to FALSE
#'
#' @details This function computes the weighted mean percentage of sand, silt, and
#'   clay in the first meter of soil. For more information on soil file needed
#'   by the model, please refer to the \href{
#'   http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'    Guide}
#'
#' @return Used for its side effects.It save in \code{outdir} a file
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
#'   \item \emph{SAND_PERC}: soil sand (in %)
#'   \item \emph{SOIL_DEPTH}: soil depth (in m)
#'   \item \emph{FR}:0.9 by default. Fertility rating (dim) (only LUE version)
#'   \item \emph{FN0}:0.5 by default. Value of fertility modifier when FR=0 (dim) (only LUE version)
#'   \item \emph{M0}:0.2 by default. Value of ‘m’ when FR=0 (dim) (only LUE version)
#'   }
#' @seealso \code{\link{run_3DFEM}}
#' @export



make_soil_ISRIC <- function(plot,crs=NULL,buffer=100,id=NULL,site=NULL,outdir=NULL,return_df=FALSE){

  check_dir(outdir)
  stopifnot(
    "buffer must be a number" =  check_num(buffer)
  )

  if(is.null(site)&!is.null(id)){
    warning("Argument 'site' is missing with no default. It will be dropped in the file name and id will be used instead.")
  }

  if(is.null(site)&is.null(id)){
    warning("Argument 'site' is missing with no default and id is NULL. A temporary site name will be used.")
  }

  if(!any(class(plot)%in%c("sf"))){
    if(is.null(crs)){
      stop("If argument 'plot' is not an sf object, the crs of coordinates must be given")
    }
    if(class(sf::st_crs(crs))!="crs"){
      stop("Argument 'crs' should be something suitable as input to sf::st_crs")
    }
    plot <- sf::st_as_sf(as.data.frame(plot),coords = c(1,2),crs=crs)
  }

  if(nrow(plot)!=1){
    if(!check_char(id)){
      stop("Argument 'id' must be a character vector of length 1")
    }
  }

  #eventually for parallelization
  #unregister <- function() {
  #  env <- foreach:::.foreachGlobals
  #  rm(list=ls(name=env), pos=env)
  #}

  plot <- sf::st_transform(plot,4326)
  latlon <- as.data.frame(sf::st_coordinates(plot))
  plot <- sf::st_buffer(plot,buffer)
  rst <- system.file("extdata/ISRIC.tif",package = "R3DFEM")
  rst <- terra::rast(rst)

  if(!is.null(id)) {
    soil <- exactextractr::exact_extract(rst, plot, "mean",append_cols=id)
  } else{
    soil <- exactextractr::exact_extract(rst, plot, "mean")
  }
  gc()

  deep <- c(rep(c(5,10,15,30,40),3))
  if(!is.null(id)){
    idd <- soil[[id]]
    soil[[id]] <- NULL
  }

  soil_weighted <-  sweep(soil,2,deep,FUN="/")
  tot <- rowSums(soil_weighted)
  soil_weighted <-  soil_weighted/tot*100
  clay <- rowSums(soil_weighted[,1:5])
  sand <- rowSums(soil_weighted[,6:10])
  silt <- rowSums(soil_weighted[,11:15])

  soil <-
    data.frame(
      X = 0,
      Y = 0,
      LANDUSE = "F",
      LAT = latlon[,1],
      LON = latlon[,2],
      CLAY_PERC = clay,
      SILT_PERC = silt,
      SAND_PERC = sand,
      SOIL_DEPTH = 100,
      FR=0.9,
      FN0=0.5,
      FNN=0.5,
      M0=0.2,
      LITTERC=-9999,
      LITTERN=-9999,
      SOILC=-9999,
      SOILN=-9999,
      DEADWOODC=-9999
    )

  if(!is.null(id)){
    soil$id <- idd
    for (i in 1:length(idd)) {
      out <- soil[soil$id==idd[i],]
      out$id <- NULL
      if(is.null(site)){
        write.table(out,paste0(outdir,"/",idd[i],"_soil.txt"),row.names=F,col.names=T,quote=FALSE,sep=",")
        message("file ", paste0(outdir,"/",idd[i],"_soil.txt"), " saved")

      }else{
      write.table(out,paste0(outdir,"/",site,"_",idd[i],"_soil.txt"),row.names=F,col.names=T,quote=FALSE,sep=",")
      message("file ", paste0(outdir,"/",site,"_",idd[i],"_soil.txt"), " saved")
      }
    }
  }else{
    if(is.null(site)){
      site <- "tmp"
      write.table(soil,paste0(outdir,"/",site,"_soil.txt"),row.names=F,col.names=T,quote=FALSE,sep=",")
      message("file ", paste0(outdir,"/",site,"_soil.txt"), " saved")
    }else{
    write.table(soil,paste0(outdir,"/",site,"_soil.txt"),row.names=F,col.names=T,quote=FALSE,sep=",")
    message("file ", paste0(outdir,"/",site,"_soil.txt"), " saved")
    }
  }
  if(return_df){
    return(soil)
  }
}
