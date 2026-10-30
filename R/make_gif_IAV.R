

make_IAV_gif <- function(site,monthly_output,var,outdir,delate_frame=T,parallel=FALSE){

  o <- Sys.time()

  if(!dir.exists(outdir)){
    message("Outdir does not exist. It will be created")
    dir.create(outdir)}
db <- read.table(monthly_output,header=T,sep=",")
db$MONTH <- factor(month.abb[db$MONTH],levels=month.abb)
db$YEAR <- as.factor(db$YEAR)
db$wue <- (db$GPP/1000)/db$CTRANSP
db$wue[db$wue==Inf] <- 0
db$wue[is.nan(db$wue)] <- 0
db$wue[is.na(db$wue)] <- 0

text_size <- 40
time <- length(unique(db$YEAR))
palette <-grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))
col <- palette(time)
year <- unique(db$YEAR)
current_col=gt::adjust_luminance(col,1)

gpp <- db %>% dplyr::group_by(YEAR) %>% dplyr::summarise(gpp=mean(.data[[var]]))
gpp <- gpp[order(gpp$gpp),]
gpp$col <- col[time:1]
gpp$cur_col <- current_col[time:1]


alpha <- seq(0.9,0.1,length=time)

column <- c("YEAR","MONTH",var)
range_var <- range(db[[var]])

if (parallel) {
  # honor registration made by user, and only create and register
  # our own cluster object once
  if (! foreach::getDoParRegistered()) {
    ncore <- max(1, min(time, parallel::detectCores() - 1))
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

foreach::foreach(i=1:(time),.packages=c("ggplot2"))%d%{

 df <- db[db$YEAR%in%year[1:i],column]
 ny <- length(unique(df$YEAR))
 df$alpha <- rep(alpha[i:1],each=12)

 p <- ggplot2::ggplot(df)+
   ggplot2::geom_line(ggplot2::aes(x=MONTH,y=.data[[var]],group=YEAR,colour=YEAR,alpha=alpha),linetype=1,linewidth=0.9)+
  ggplot2::geom_line(data=df[df$YEAR==year[i],],ggplot2::aes(x=MONTH,y=.data[[var]],group=YEAR,alpha=alpha),linetype=1,linewidth=1.5,colour=gpp$cur_col[i])+
  ggplot2::theme_minimal(base_family = "sans")+
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(colour = "#22FF00"),
    panel.grid.minor =  ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank() ,
    plot.background = ggplot2::element_rect(fill = "black"),
    panel.background = ggplot2::element_rect(fill = 'black'),
    plot.title = ggplot2::element_text(hjust = 0.5,size = text_size - 15,color="white"),
    plot.subtitle = ggplot2::element_text(hjust = 0.5,size = text_size - 20,colour = "white"),
    axis.title = ggplot2::element_text(size = text_size - 10, colour = "white"),
    axis.text = ggplot2::element_text(size = text_size - 15),
    axis.text.y =   ggplot2::element_text(colour = "#22FF00"),
    axis.text.x =   ggplot2::element_text(colour = "#F3FF12",size=text_size-20),
    legend.position = "none",
    plot.caption = ggplot2::element_text(color = "white", face = "italic")
  )+
   ggplot2::labs(title=paste0(site),subtitle="3D-CMCC-FEM v.5.6")+
   ggplot2::ylim(range_var[1],range_var[2])+
   ggplot2::scale_colour_manual(values = gpp$col[1:i])+
   ggplot2::annotate("text",x=11,y=range_var[2],label=year[i],col=gpp$col[i],size=text_size-28)


 if (var == "GPP" | var == "RA" | var == "NPP") {
   p  <-  p +
     ggplot2::ylab(bquote(paste(.(var)," in gC ",m^-2,month^-1)))
 }
 if (var == "C_HWP" | var == "STANDING_WOOD" | var == "DELTA_WOOD" | var == "Total_C_Woody_Stock" ) {
   p  <-  p +
     ggplot2::ylab(bquote(paste(.(var)," in tC ",ha^-1, sep="")))

 }

 ggplot2::ggsave(
   file.path(outdir, paste0(site,"_",year[i],"_",var, ".png")),
   plot = p,
   scale = 2,
   width = 11,
   height = 9,
   units = "cm",
   dpi = 200
 )

}

path_frame <- file.path(outdir, paste0(site,"_",year,"_",var, ".png"))
gifski::gifski(png_files = path_frame,
               gif_file = file.path(outdir,paste0(site,"_",var,".gif")),
               width = 5196, height = 4251,
               delay = 0.1, loop = F, progress = TRUE)

dt <- difftime(Sys.time(),o)
 message("Elapsed time:",round(dt,2)," ", attr(dt,"units"))

 if(delate_frame)file.remove(path_frame)
return(NULL)
}


