#' Create meteo input file for 3DCMCCFEM model
#'
#' \code{make_meteo_EOBS} use the
#' \href{https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles/}{E-OBS
#' database} to create the meteo input file according to the 3DCMCCFEM model
#' specifications. The time period covered by E-OBS is 1950-01-01 - 2022-06-30.
#'
#' @param plot Sf object or something that can be converted to an sf point object
#'   (i.e., a data.frame of coordinates). If plot is a data.frame the argument
#'   crs must be specified.
#' @param crs Numeric: a valid EPSG code
#' @param buffer Numeric: buffer around plot to extract meteo data from
#' @param id Character: name of the column in \code{plot} that identifies different
#'   points
#' @param daterange Character vector of length 2: starting and ending date from
#'   which to extract climate data. Must be in the form yyyy-mm-dd
#' @param convert_rad Logical: Should the solar radiation be converted from
#'   radiant to Wh/m-2? Default to TRUE.
#' @param parallel Logical: Should the extraction be performed in parallel? Default to FALSE
#' @param site Character: the name of the plot site. Used to construct the output name
#' @param outdir Character: path to the output directory.
#' @param return_df Logical: should the climate data be returned by the function
#'   as a data.frame? Default to FALSE
#'
#' @details This function uses version 26.0 of the E-OBS climate data to
#'   Generate the input file for the 3D-CMCC-FEM model. If parallel=TRUE, user
#'   can register his own backhand. Otherwise, the function will register a
#'   7-workers implicit cluster based on \code{\link[foreach]{foreach}} and
#'   \code{\link[doParallel]{registerDoParallel}}. For more information on meteo
#'   file needed by the model, please refer to the \href{
#'   http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'    Guide}
#'
#' @return Used for its side effects.It save in \code{outdir} a file
#'   site_meteo.txt The output file has the following columns:
#'   \itemize{
#'   \item \emph{Year}: Reference year for the meteo data
#'   \item \emph{Month}: number of month in the year
#'   \item \emph{n_day}: number of day in the month
#'   \item \emph{Rg_f}: Solar radiation on the ground (in  Wh/m-2)
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
#' @seealso \code{\link{run_3DFEM}}
#' @export

make_meteo_EOBS <- function(plot,crs=NULL,buffer=100,id=NULL,daterange=c("1980-01-01","2022-06-30"),convert_rad=TRUE,parallel=FALSE,site=NULL,outdir=NULL,return_df=FALSE){

  terra::gdalCache(46567)

  check_dir(outdir)
  if(is.null(site)){
    warning("Argument 'site' is missing with no default. A temporary name will be given.")
    site <- "tmp"
  }

  stopifnot("daterange must be a character vector of length 2"=check_len_char(daterange,2))
  ref_date <- as.Date(c("1980-01-01","2022-06-30"))
  tmp <- as.Date(daterange)
  if(tmp[1]<ref_date[1])stop("starting date must be after 1980-01-01")
  if(tmp[2]>ref_date[2])stop("ending date must be before 2022-06-30")

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

  unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }

  plot <- sf::st_transform(plot,4326)
  plot <- sf::st_buffer(plot,buffer)
  rst <-
    system.file(
      c(
        "extdata/fg_ens_mean_0.1deg_reg_v26.0e.nc",
        "extdata/hu_ens_mean_0.1deg_reg_v26.0e.nc",
        "extdata/qq_ens_mean_0.1deg_reg_v26.0e.nc",
        "extdata/rr_ens_mean_0.1deg_reg_v26.0e.nc",
        "extdata/tg_ens_mean_0.1deg_reg_v26.0e.nc",
        "extdata/tn_ens_mean_0.1deg_reg_v26.0e.nc",
        "extdata/tx_ens_mean_0.1deg_reg_v26.0e.nc"
      ),
      package = "R3DFEM"
    )

  rst <- sort(rst)

  if (parallel) {
    # honor registration made by user, and only create and register
    # our own cluster object once
    if (! foreach::getDoParRegistered()) {
      doParallel::registerDoParallel(7)
      message('Registered doParallel with ',
              7, ' workers')
    } else {
      message('Using ', foreach::getDoParName(), ' with ',
              foreach::getDoParWorkers(), ' workers')
    }
    `%d%` <- foreach::`%dopar%`
  } else {
    message('Executing extraction sequentially')
    `%d%` <- foreach::`%do%`
  }

  meteo <- foreach::foreach(i=rst,.packages=c("terra","exactextractr","reshape2"),.combine=cbind)%d%{
    eobs <- terra::rast(i)
    time_eobs <- terra::time(eobs)
    start <- which(time_eobs==daterange[1])
    end <- which(time_eobs==daterange[2])
    eobs <- eobs[[start:end]]
    if(!is.null(id)){
      ex <- exactextractr::exact_extract(eobs,plot,"mean",append_cols =id)
      colnames(ex)[-1] <- as.character(time_eobs[start:end])
      ex <- reshape2::melt(ex,id.vars=id)
      colnames(ex)[2:3] <- c("date",substr(basename(i),1,2))
    }else{
      ex <- exactextractr::exact_extract(eobs,plot,"mean")
      colnames(ex) <- as.character(time_eobs[start:end])
      ex <- reshape2::melt(ex)
      colnames(ex) <- c("date",substr(basename(i),1,2))
    }

    return(ex)
  }

  if(parallel){unregister()
    doParallel::stopImplicitCluster()}


  meteo <- meteo[,!duplicated(colnames(meteo))]
  date <-  stringr::str_split(meteo$date,"-",simplify = T)
  meteo$Year <- date[,1]
  meteo$Month <- date[,2]
  meteo$n_days <- date[,3]
  meteo$Ts_f <- -9999
  meteo$SWC <- -9999
  meteo$LAI <- -9999
  meteo$ET <- -9999
  meteo$date <- NULL
  if(!is.null(id)){
    meteo <- meteo[,c(1,9:11,4,6,8,7,3,12,5,13,14,15,2)]
    colnames(meteo)[5:15] <- c("Rg_f","Ta_f","Tmax","Tmin","RH_f","Ts_f","Precip","SWC","LAI","ET","WS_f")
  } else{
    meteo <- meteo[,c(9:11,4,6,8,7,3,12,5,13,14,15,2)-1]
    colnames(meteo)[4:14] <- c("Rg_f","Ta_f","Tmax","Tmin","RH_f","Ts_f","Precip","SWC","LAI","ET","WS_f")
  }

  if(convert_rad){
    meteo$Rg_f <- meteo$Rg_f*0.0864
  }

  if(!is.null(id)){
    idd <- unique(meteo[[id]])
    for (i in 1:length(idd)) {
      out <- meteo[meteo[[id]]==idd[i],]
      out[[id]] <- NULL
      write.table(out,paste0(outdir,"/",site,"_",idd[i],"_meteo.txt"),row.names=F,col.names=T,quote=FALSE,sep="\t")
    }
  }else{
    write.table(meteo,paste0(outdir,"/",site,"_meteo.txt"),row.names=F,col.names=T,quote=FALSE,sep="\t")
  }

  message("file ", paste0(outdir,"/",site,"_meteo.txt"), " saved")
  if(return_df){
    return(meteo)
  }
}
