setwd("E:/R3DFEM")
meteo_or = read.table(
  "E:\\R3DFEM\\data-raw\\Bonis_meteo_hist_cosmo.txt",
  header = T,
  sep = '\t'
)
co2_or = read.table(
  "E:\\R3DFEM\\data-raw\\CO2_hist.txt",
  sep = '\t',
  header = T
)


# infc 2005 ---------------------------------------------------------------

tl_infc05 <- read.csv("E:\\R3DFEM\\data-raw\\INFC/t2_05_apv.csv",sep=";")

shp_infc05 <- sf::read_sf("E:\\R3DFEM\\data-raw\\INFC/INFC_2005_tot.shp")
shp_infc05 <- sf::st_transform(shp_infc05,crs=4326)
tmp <- sf::read_sf("E:/1ForestNavigator_data/INFC2005/infc_2005_4326_clean.shp")
shp_infc05<- merge.data.frame(sf::st_drop_geometry(shp_infc05[,1:3]),tmp,by.y = "IDPUNTO",by.x = "idpunto")
shp_infc05 <- shp_infc05[!sf::st_is_empty(shp_infc05$geometry),]
shp_infc05$ETA_MEDIA_ <- round(shp_infc05$ETA_MEDIA_,0)
colnames(shp_infc05) <- tolower(colnames(shp_infc05))

# infc 2015 ---------------------------------------------------------------

tl_infc15 <- read.csv("E:\\R3DFEM\\data-raw\\INFC/t2_15_apv.csv",sep=";")
tl_infc15$capv <- NULL
shp_infc15 <- sf::read_sf("E:\\R3DFEM\\data-raw\\INFC/INFC_2015_tot.shp")
shp_infc15 <- sf::st_transform(shp_infc15,crs=4326)
#uniform the 2 dataset
shp_infc15 <- shp_infc15[,c(1,4,5,6,7,10,11,12,23)]
shp_infc15 <- shp_infc15[!sf::st_is_empty(shp_infc15$geometry),]
shp_infc15 <- merge.data.frame(sf::st_drop_geometry(shp_infc05[,c(1,4:7)]),shp_infc15,by.y = "IDPUNTO",by.x = "idpunto")
shp_infc15$ETA <- ifelse(shp_infc15$ETA==0,shp_infc15$eta_media_+1,shp_infc15$ETA)
shp_infc15$eta_media_ <- NULL
new.name <- colnames(shp_infc05)
shp_infc15 <- shp_infc15[,c(1,7,8,6,2:4,5,9:13)]
colnames(shp_infc15) <- new.name


# extract dtm -------------------------------------------------------------

dtm <- terra::rast("E:/volume_italia_2005/variabili/DTM_warped.tif")

ex05 <- exactextractr::exact_extract(dtm,sf::st_buffer(sf::st_as_sf(shp_infc05),13),"mean",append_cols =colnames(shp_infc05))
ex15 <- exactextractr::exact_extract(dtm,sf::st_buffer(sf::st_as_sf(shp_infc15),13),"mean",append_cols =colnames(shp_infc15))

colnames(ex05)[13] <- "dtm"
colnames(ex15)[13] <- "dtm"
shp_infc05 <- ex05
shp_infc15 <- ex15
# load internal data ------------------------------------------------------


usethis::use_data(meteo_or,co2_or,tl_infc05,tl_infc15,shp_infc05,shp_infc15,internal=TRUE,overwrite = TRUE,compress="xz")

# load external data ------------------------------------------------------

specie <- read.csv("E:/R3DFEM/data-raw/specie.csv",header = T,sep=";")
tree_level_species <- read.csv("E:/R3DFEM/data-raw/aggregation.csv",header = T,sep=";")
usethis::use_data(specie,internal=FALSE,overwrite = TRUE,compress="xz")
usethis::use_data(tree_level_species,internal=FALSE,overwrite = TRUE,compress="xz")
