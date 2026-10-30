# Sensitivity of the model outputs to a single species parameter (internal).
# '...' are passed to run_3DCMCCFEM() and must include 'species'.
param_sensitivity <- function(param=NULL,variable=NULL,value,inputdir,outputdir,remove_output=T,...){

  stopifnot("Param must be a character vector of length 1."=check_char(param))
  check_dir(outputdir)

  args <- list(...)
  if (is.null(args$species)) stop("Argument 'species' must be passed in '...'")
  parametrization <- file.path(inputdir, paste0(args$species[1], ".txt"))
  if (!file.exists(parametrization)) stop("Missing file ", parametrization)

  possible.args <- read_species(parametrization)$parameter
  param <- match.arg(param,possible.args,several.ok = F)

  # the original parameterization is restored also if a run fails
  backup <- tempfile(fileext = ".txt")
  file.copy(parametrization, backup)
  on.exit(file.copy(backup, parametrization, overwrite = TRUE), add = TRUE)

  newoutputdir <- file.path(outputdir,param,value,"output")

  res <- lapply(seq_along(value), function(i) {
    do.call(set_species_param, c(list(file = parametrization), stats::setNames(list(value[i]), param)))
    out_files <- run_3DCMCCFEM(...,inputdir = inputdir,outputdir = newoutputdir[i],output = "annual")
    df <- data.table::fread(out_files[grepl("^annual_[^m]", basename(out_files))][1], sep = ",", header = T, fill = T)
    df <- df[stats::complete.cases(df), ]
    df[[param]] <- value[i]
    df
  })

  allin <- data.table::rbindlist(res)
  allin[[param]] <- as.factor(as.numeric(allin[[param]]))
  possible.var <- colnames(allin)
  variable <- match.arg(variable,possible.var,several.ok = T)

  cols <- c("YEAR",variable,param)
  allin <- as.data.frame(allin)
  allin <- allin[,cols]
  m <- reshape2::melt(allin,id.vars=c("YEAR",param),variable.name = "var")
  text_size <- 40
  site_plot <- ggplot2::ggplot(m) +
    ggplot2::geom_line(ggplot2::aes(x=YEAR,y=value,col=.data[[param]],group=.data[[param]]),linewidth=1.1)+
    ggplot2::facet_wrap(~var,scales = "free_y")+
    ggplot2::theme_classic(base_family = "sans") +
    ggplot2::theme(title = ggplot2::element_text(size = text_size -2),
                   axis.title = ggplot2::element_text(size = text_size -6),
                   axis.text = ggplot2::element_text(size = text_size -12),
                   axis.text.x = ggplot2::element_text(angle = 90),
                   panel.spacing = ggplot2::unit(2, "lines"),
                   legend.title = ggplot2::element_text(size = text_size -12),
                   legend.text = ggplot2::element_text(size = text_size - 18),
                   legend.position = "right",
                   legend.key.size = ggplot2::unit(2,"line"),
                   legend.key.height = ggplot2::unit(1,"line"),
                   legend.key = ggplot2::element_rect(linewidth = 1,fill = "transparent", color = "transparent",linetype = 0),
                   strip.text = ggplot2::element_text(size = text_size - 5),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), strip.background = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "transparent"),
                   panel.border = ggplot2::element_rect(linetype = "solid",fill = "transparent", color = "black", linewidth = 1.2))
  print(site_plot)
  newname <- file.path(outputdir,paste0(param,".png"))
  ggplot2::ggsave(newname, plot = site_plot, scale = 2,
                  width = 26, height = 13, units = "cm", dpi = 300)

  if(remove_output){
    unlink(file.path(outputdir,param), recursive = TRUE)
  }
  return(allin)
}


#inputdir <- "D:/input"
#outputdir <- "D:/output/"
#x <- param_sensitivity(param="LIGHT_TOL",variable = c("NPP","MaxLAI","HEIGHT"),value=1:4,inputdir = inputdir,outputdir = outputdir,site = 'Hogwarts',
#                  species = 'Whompingwillow',
#                  year_start = 1950,
#                  year_end = 2030,
#                  man = 'off')
#
