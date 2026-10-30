
make_daily_allocation_gif <- function(site,daily_output,outdir,delate_frame=T,parallel=FALSE){
  
  o <- Sys.time()
  
  if(!dir.exists(outdir)){
    message("Outdir does not exist. It will be created")
    dir.create(outdir)}
  
  df <- read.table(daily_output,header=T,sep=",") 
  time <- unique(df$YEAR)
  var <- c("DELTA_RESERVE_C","DELTA_STEM_C","DELTA_LEAF_C","DELTA_FROOT_C","DELTA_CROOT_C","DELTA_BRANCH_C","DELTA_FRUIT_C")
  cost <- c("NPP","LAI_PROJ")
  date <- lubridate::make_date(as.character(df$YEAR),as.character(df$MONTH),as.character(df$DAY))
  df$doy <- lubridate::yday(date)
  df <- df[,c("YEAR","doy",var,cost)]
  df <- df[!df$doy%in%c(1,365,366),]
  #df$NPP <- df$NPP/100
  #df[,var] <- apply(df[,var],2,function(x)x*100)
  
  ylim.prim <- range(df$NPP)   
  ylim.sec <- range(df$LAI_PROJ) 
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b*ylim.sec[1]
  
  n <- reshape2::melt(df,id.vars=c("YEAR","doy"))
  n$value[n$variable%in%var] <- n$value[n$variable%in%var]*100
  lim <- range(n$value[n$variable!="LAI_PROJ"])
  

  if (parallel) {
    # honor registration made by user, and only create and register
    # our own cluster object once
    if (! foreach::getDoParRegistered()) {
      ncore <- max(1, min(length(time), parallel::detectCores() - 1))
      cl <- parallel::makeCluster(ncore)
      doParallel::registerDoParallel(cl)
      on.exit({parallel::stopCluster(cl); foreach::registerDoSEQ()}, add = TRUE)
      message('Registered doParallel with ',
              ncore, ' workers')
    } else {
      message('Using ', foreach::getDoParName(), ' with ',
              foreach::getDoParWorkers(), ' workers')
    }
    `%d%` <- foreach::`%dopar%`
  } else {
    message('Executing sequentially')
    `%d%` <- foreach::`%do%`
  }
  
  
  foreach::foreach(i=time,.packages=c("ggplot2"))%d%{
    
    db <- df[df$YEAR==i,]
    db$YEAR <- NULL
    m <- reshape2::melt(db,id.vars="doy")
    m$value[m$variable%in%var] <- m$value[m$variable%in%var]*100
    text_size=40
    p <- ggplot2::ggplot(m,ggplot2::aes(col=variable,x=doy,y=value,lty =variable,linewidth=variable))+
      ggplot2::geom_line()+
      ggplot2::scale_linetype_manual(
        values = c(
          DELTA_RESERVE_C = "solid",
          DELTA_STEM_C="solid",
          DELTA_LEAF_C="solid",
          DELTA_FROOT_C="solid",
          DELTA_CROOT_C="solid",
          DELTA_BRANCH_C="solid",
          DELTA_FRUIT_C="solid",
          NPP="dashed",
          LAI_PROJ="dashed"
        )
      ) +
      ggplot2::scale_color_manual(values = c(
        c(
          "#ED1915",
          "#EB9423",
          "#08FA0C",
          "#F2E208",
          "#9203FF",
          "#B5FF08",
          "#0AB6FF",
          "#00F204",
          "#3700FF"
        )
      )) +
      ggplot2::scale_alpha_manual(values = seq(0.5, 1, length = 9)) +
      ggplot2::scale_linewidth_manual(values = c(rep(.7, 8), 1.2)) +
      ggplot2::labs(x = "DOY", title = paste0(site, " (", i, ")"),caption = "Hist") +
      ggplot2::theme_classic(base_family = "sans") +
      ggplot2::theme(title = ggplot2::element_text(size = text_size-10),
            plot.title = ggplot2::element_text(hjust = 0.5),
            axis.title = ggplot2::element_text(size = text_size-6),
            axis.title.y.right = ggplot2::element_text(angle = 90),
            axis.text = ggplot2::element_text(size = text_size-12),
            panel.spacing = ggplot2::unit(2, "lines"),
            legend.title = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = text_size-18),
            legend.position = "bottom",
            legend.key.size = ggplot2::unit(2,"line"),
            legend.key.height=ggplot2::unit(1,"line"),
            legend.key = ggplot2::element_rect(linewidth=1,fill="transparent",color="transparent",linetype=0),
            strip.text = ggplot2::element_text(size = text_size-5),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            strip.background = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = 'transparent'),
            panel.border = ggplot2::element_rect(linetype = 'solid', fill = 'transparent', color='black',linewidth=1.2))+
      ggplot2::guides(
        linetype = ggplot2::guide_legend(override.aes = list(size = 5), ncol = 3),
        colour = ggplot2::guide_legend(override.aes = list(size = 5), ncol = 3),
        alpha = ggplot2::guide_legend(override.aes = list(size = 5), ncol = 3),
        linewidth = ggplot2::guide_legend(override.aes = list(size = 5), ncol =3)
      ) +
      ggplot2::ylab(bquote(paste("Daily allocation"," in gC ",m^-2,day^-1)))+
      ggplot2::scale_x_continuous(breaks = seq(0,365,30))+
      ggplot2::scale_y_continuous(limits=lim, sec.axis = ggplot2::sec_axis( ~ (. - a) / b, name = bquote(paste(
        "LAI", " in ", m ^ 2, m ^ -2
      ))))
    
    
    ggplot2::ggsave(file.path(outdir,paste0(site,"_",i,".png")),
           plot = p, scale=2,width = 26,height = 13, units = "cm",
           dpi = 300) 

  }
  
  path_frame <- file.path(outdir, paste0(site, "_", time, ".png"))
  gifski::gifski(png_files = path_frame,
                 gif_file = file.path(outdir,paste0(site,".gif")),
                 width = 5196, height = 4251,
                 delay = 0.3, loop = F, progress = TRUE)
  
  
  dt <- difftime(Sys.time(),o)
  message("Elapsed time:",round(dt,2)," ", attr(dt,"units"))
  
  if(delate_frame)file.remove(path_frame)
  return(NULL)
}

#daily_output <- "D:\\forest modelling lab\\Theoretical basis\\output_daily\\daily_111_co2_on_man_off_2016-2099.txt"
#make_daily_allocation_gif("Monte Amiata",daily_output,outdir = "D:/forest modelling lab/Theoretical basis/gif/")

