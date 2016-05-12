##########################################################################################
################## Module interaction entre les sources de donnees                 #######
##########################################################################################

# Points abordes :
# - faire un graphique avec moyenne, quantile et outliers, par categories
# - croiser raster et point
# - croiser vecteur et raster
# - reclassifier un raster


#############################################################a############################# 
# Derniere mise e jour 30/08/2015
# remi.dannunzio@fao.org
##########################################################################################

##########################################################################################
################## Options de base, paquets
##########################################################################################
options(stringsAsFactors=FALSE)

setwd("/media/xubuntu/data_ofgt/")
setwd("c:/Users/dannunzio/Documents/countries/cote_ivoire/trainings_cote_ivoire/training_geospatial_mai2016/")


library(rgdal) # pour lire les format GDAL/OGR
library(foreign) # pour lire les fichiers DBF
library(raster) # pour lire les fichiers raster, obtenir les projections
library(rgeos) # pour faire des operations sur les geometries
#library(tmap) # pour faire des cartes esthetiques
library(dplyr) # pour manipuler les data.frame (triage par exemple)
#library(maps)

### Lire les fichiers: fonction "raster", "readOGR" et "read.csv"
r_90 <- raster("data/raster/BNETD_Carto/1990/p197r055_5dt19860116_classify.img")
r_00 <- raster("data/raster/BNETD_Carto/2000/p197r055_7t20020102_classify.img")
r_15 <- raster("data/raster/BNETD_Carto/2015/p197r055_lc8_20151231_classify.img")

poly   <- readOGR(dsn="data/vector/coteivoire_gaul_adm1_geo.shp",layer="coteivoire_gaul_adm1_geo")
points <- readOGR(dsn="data/vector/mes_points.kml",layer="mes_points")

table  <- read.csv("data/table/classes.csv")

cci <- raster("data/raster/cci_rci.tif")

### Faire une reclassification thematique (sur une portion plus petite)
r_90_fnf <- reclassify(r_90,table[,c(1,2)])
r_00_fnf <- reclassify(r_00,table[,c(1,3)])
r_15_fnf <- reclassify(r_15,table[,c(1,4)])


writeRaster(r_90_fnf,"data/raster/r_90_fnf.tif")
writeRaster(r_00_fnf,"data/raster/r_00_fnf.tif")
writeRaster(r_15_fnf,"data/raster/r_15_fnf.tif")

e<-extent(119020,237430,765152,854981)

brick <- brick(
  crop(r_90_fnf,e),
  crop(r_00_fnf,e),
  crop(r_15_fnf,e))


plot(brick)
plotRGB(brick,stretch="hist")
writeRaster(brick,"data/raster/brick.tif")

code_3date <- 
  crop(r_90_fnf,e) + 
  10*crop(r_00_fnf,e) +
  100*crop(r_15_fnf,e)

writeRaster(code_3date,"data/raster/code_3.tif")

### Extraire la valeur du raster pour chaque point 
my_extract <-data.frame(extract(brick,spTransform(points,proj4string(brick))@coords))
names(my_extract)
2^32

### Rajouter l'information
points$L1990 <- my_extract$layer.1
points$L2000 <- my_extract$layer.2
points$L2015 <- my_extract$layer.3

### Afficher les points qui tombent dans l'image
plot(points[!is.na(points@data$L1990),])

points(points[!is.na(points@data$L1990) & points@data$L1990==1,],col="blue")


### Creer les statistiques zonales d'un raster sur un polygone
bas_ss <- poly[poly$ADM1_NAME=="Bas Sassandra",]


table(data.frame(
  extract(cci,bas_ss)
  ))

### Decouper un raster aux limites d'un polygone 
my_crop <- mask(crop(cci,bas_ss),bas_ss)
plot(my_crop)


### Creer une fonction zonale
cci_zonal <- function(x){
    table(
      data.frame(
        extract(cci,poly[poly$ADM1_NAME == x,]
        )
      )
    )
}

### Appliquer la fonction zonale avec le raster aggrege pour un departement
cci_zonal("Bas Sassandra")

### Creer la liste des departements
list <- levels(as.factor(poly$ADM1_NAME))

### Appliquer la fonction a tous les departements
sapply(list,cci_zonal)



