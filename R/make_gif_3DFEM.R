#' Create a gif from the output of 3DCMCCFEM model
#'
#' \code{make_gif_3DFEM} use the output file generated by the function
#' \code{run_3DFEM} to create a gif of the simulation. In particular each frame
#' will correspond to one simulation year. For this reason this function works only with the annual output of the model.
#'
#' @param path Character: path to simulation output file. Must be annual.
#' @param years Numeric vector: years of simulations from which to create the
#'   gif
#' @param distribution Character: How tree should be distributed in the stand?.
#'   Options are c("regular","pseudorandom","random"). The pseudorandom option
#'   is the slowest but ensure a minimum distance among trees in the random distribution
#' @param outdir Character: path to the output directory.
#' @param site Character: site name for constructing the output name.
#' @param return_gif Logical: Should a file gif be created? Default to TRUE.
#' @param delate_frame Logical: Should the single images frames be deleted after
#'   the creation of the gif file? default to FALSE
#'
#' @details The function relys on \code{rgl::snapshot(...,top=TRUE)} so during
#'   the run it is a good idea to not alter the screen, to avoid
#'   \code{rgl::snapshot} capturing a wrong screenshot. The function became very
#'   slow when the number of tree to plot exeeds 300.
#'
#' @return Used for its side effects.It save in \code{outdir} as many images as
#'   the length of the simulation (if \code{delete_frame=FALSE}) and a gif file (if
#'   \code{return_gif=TRUE})
#' @seealso \code{\link[=run_3DFEM]{run_3DFEM}}
#' @export

make_gif_3DFEM <- function(path,years=NULL,distribution=c("regular","pseudorandom","random"),outdir,return_gif=TRUE,delete_frame=TRUE) {


  distribution <- match.arg(distribution,c("regular","pseudorandom","random"))

  requireNamespace("magick", quietly = TRUE)
  requireNamespace("rLiDAR", quietly = TRUE)
  dir.create(outdir,showWarnings = F)
  stopifnot("Path must be the character path of an annual output of the 3D-CMCC-FEM model, as returned by run_3DFEM()"=is.character(path))
  stand <- read.table(path, header = T, sep = ",")

  if(!is.null(years)){
    stand <- stand[stand$YEAR%in%years,]
  }

  rgl::par3d(windowRect=c(0,0,900,900))

  initial_state <-list()

  for (l in unique(stand$LAYER)) {

    l_stand <- stand[stand$LAYER==l,]

    max_n <- max(l_stand$LIVETREE)
    message("Layer ",l,": ",nrow(l_stand), " years of simulation \nMax number of trees: ",max_n)
    species <- c(Fagussylvatica="ellipsoid",
                 Larixdecidua="cone",
                 Piceaabies="cone",
                 Pinussylvestris="cylinder",
                 Quercusrobur="ellipsoid")

    if(!l_stand$SPECIE[1]%in%names(species)){
      crw_typ <- "ellipsoid"
    }else{
      crw_typ <- unname(species[names(species)==l_stand$SPECIE[1]])
    }

    set.seed(4)
    l_stand$HCB <- l_stand$HEIGHT-l_stand$CROWN_HEIGHT


    year <- l_stand$YEAR[1]
    Ntrees <- l_stand$LIVETREE[1]
    meanHCB <- l_stand$HCB[1] # mean of the height at canopy base
    sdHCB <-    0.01*meanHCB # l_standard deviation of the height at canopy base
    HCB <- abs(rnorm(Ntrees, mean = meanHCB, sd = sdHCB))# height at canopy base
    dbh <- FNN::knn.reg(y=l_stand$DBH,test=as.data.frame(HCB),train=l_stand$HCB, k=2)$pred/15
    CL <- FNN::knn.reg(y=l_stand$CROWN_HEIGHT,test=as.data.frame(HCB),train=l_stand$HCB, k=2)$pred*2
    CW <- FNN::knn.reg(y=l_stand$CROWN_DIAMETER,test=as.data.frame(HCB),train=l_stand$HCB, k=2)$pred/1.2
    age <- l_stand$AGE[1]
    randomness <- age/Ntrees*100

    if(distribution=="random"){
      grid <- data.frame(
        X =   as.numeric(sample(1:100,max(l_stand$LIVETREE),replace = T)),
        Y = as.numeric(sample(1:100,max(l_stand$LIVETREE),replace = T))
      )

    }else if(distribution=="pseudorandom"){
      X <- seq(1,100,length.out=ceiling((max_n)))
      Y <- seq(1,100,length.out=ceiling((max_n)))
      grid <- expand.grid(x=X,y=Y)

      density <- 10000/max_n

      sampled_points <- sf::st_sample(
        x = sf::st_as_sf(grid,coords=c("y","x")),
        type = "SSI",
        r = density, # threshold distance (in metres)
        size = Ntrees # number of points
      )

      grid <- as.data.frame(sf::st_coordinates(sampled_points[[1]])[,1:2])
    }else{
      X <- seq(1,100,length.out=ceiling(sqrt(max_n)))
      Y <- seq(1,100,length.out=ceiling(sqrt(max_n)))
      grid <- expand.grid(X=X,Y=Y)
      grid <- grid[sample(1:nrow(grid),Ntrees),]
    }

    initial_state_l <-
      data.frame(
        HCB = HCB,
        CL = CL,
        CW = CW,
        dbh = dbh,
        x =   grid$X,
        y = grid$Y
      )

    initial_state[[l+1]] <- initial_state_l

    for (i in 1:Ntrees) {
      rLiDAR::LiDARForestStand(
        crownshape = crw_typ,
        CL = initial_state_l$CL[i],
        CW = initial_state_l$CW[i],
        HCB = initial_state_l$HCB[i],
        X = initial_state_l$x[i],
        Y = initial_state_l$y[i],
        dbh = initial_state_l$dbh[i] ,
        crowncolor = "forestgreen",
        stemcolor = "chocolate4",
        resolution = "low",
        mesh = F
      )
    }
  }
  # Add other plot parameters
  rgl::axes3d(c("x-", "x-", "y-", "z"), col = "black")
  rgl::title3d(main = year,col="black")
  rgl::snapshot3d(
    file.path(outdir, paste0(l_stand$YEAR[1], ".png")),top=T,
    webshot=F,width = 1000, height = 1000
  )
  rgl::close3d()
  message("Saving initial state: Year ",year)

  rgl::par3d(windowRect=c(0,0,900,900))



  for (j in 2:nrow(l_stand)) {

    for (l in unique(stand$LAYER)) {

      l_stand <- stand[stand$LAYER==l,]

      year <- l_stand$YEAR[j]
      Ntrees <- l_stand$LIVETREE[j]

      initial_state_l <- initial_state[[l+1]][sample(1:nrow(initial_state[[l+1]]),Ntrees), ]

      i_HCB <-
        abs(rnorm(
          Ntrees,
          mean = l_stand$HEIGHT[j] - l_stand$HEIGHT[j - 1],
          sd = (l_stand$HEIGHT[j] - l_stand$HEIGHT[j - 1])
        )) # height at canopy base
      i_CL <-
        abs(rnorm(
          Ntrees,
          mean = l_stand$CROWN_HEIGHT[j] - l_stand$CROWN_HEIGHT[j - 1],
          sd = (l_stand$CROWN_HEIGHT[j] - l_stand$CROWN_HEIGHT[j - 1])
        )) # height at canopy base
      i_CW <-
        abs(rnorm(
          Ntrees,
          mean = l_stand$CROWN_DIAMETER[j] - l_stand$CROWN_DIAMETER[j - 1],
          sd = (l_stand$CROWN_DIAMETER[j] - l_stand$CROWN_DIAMETER[j - 1])
        )) # height at canopy base
      i_dbh <-
        abs(rnorm(
          Ntrees,
          mean = l_stand$DBH[j] - l_stand$DBH[j - 1],
          sd = (l_stand$DBH[j] - l_stand$DBH[j - 1])
        )) # height at canopy base


      initial_state_l$HCB <- initial_state_l$HCB + i_HCB
      initial_state_l$CL <- initial_state_l$CL + i_CL
      initial_state_l$CW <- initial_state_l$CW + i_CW
      initial_state_l$dbh <- initial_state_l$dbh + i_dbh / 15

      for (i in 1:Ntrees) {
        rLiDAR::LiDARForestStand(
          crownshape = crw_typ,
          CL = initial_state_l$CL[i],
          CW = initial_state_l$CW[i],
          HCB = initial_state_l$HCB[i],
          X = initial_state_l$x[i],
          Y = initial_state_l$y[i],
          dbh = initial_state_l$dbh[i] ,
          crowncolor = "forestgreen",
          stemcolor = "chocolate4",
          resolution = "low",
          mesh = F
        )
      }
    }
    # Add other plot parameters
    rgl::axes3d(c("x-", "x-", "y-", "z"), col="black")
    rgl::title3d(main = year,col="black")
    rgl::snapshot3d(
      file.path(outdir, paste0(l_stand$YEAR[j], ".png")),top=T,
      webshot=F,width = 1000, height = 1000
    )
    rgl::close3d()
    message("Saving image: Year ",year)
  }

  # make gif ----------------------------------------------------------------
  path_frame <- list.files(outdir, full.name = T,pattern="png")
  img <- magick::image_read(path_frame)

  if(return_gif){
    my.animation <- magick::image_animate(img, dispose = "previous",loop=1)
    magick::image_write(my.animation, file.path(outdir,paste0(tools::file_path_sans_ext(basename(path)),".gif")))
  }

  if(delete_frame){
    file.remove(path_frame)
  }
}

#path <- "E:\\3DCMCCFEM_output\\nuove_simulazioni_CCS\\output\\BilyKriz\\rcp8p5\\120\\ESM5\\output\\BilyKriz_co2_on_man_off_1997-2099_2023-09-28_14-10-38\\annual\\BilyKriz_co2_on_man_off_1997-2099.txt"
#outdir <- "E:/3DCMCCFEM_output/gif/"
#make_gif_3DFEM(path,outdir)
