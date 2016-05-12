##########################################################################################
##################        Module de gestion des formats vecteurs                   #######
##########################################################################################

# Points abordes :
# - lire, modifier, ecrire un fichier DBF
# - lire, modifier, exporter un fichier vecteur (shape)
# - extraire des modalites (points, lignes, polygones)
# - transformer un fichier vecteur en fichier raster

########################################################################################### 
# Derniere mise a jour 25/04/2016
# remi.dannunzio@fao.org
##########################################################################################

##########################################################################################
################## Options de base, paquets
##########################################################################################
options(stringsAsFactors=FALSE)

#setwd("/media/xubuntu/data_ofgt/")
setwd("C:/Users/dannunzio/Documents/countries/cote_ivoire/trainings_cote_ivoire/training_geospatial_mai2016/")

getwd()

library(rgdal) # pour lire les format GDAL/OGR
library(foreign) # pour lire les fichiers DBF
library(raster) # pour lire les fichiers raster, obtenir les projections
library(rgeos) # pour faire des operations sur les geometries
#library(tmap) # pour faire des cartes esthetiques
library(dplyr) # a rajouter!
#library(maps)

### Lire un fichier DBF: fonction "read.dbf"
df     <- read.dbf("data/vector/coteivoire_gaul_adm1_geo.dbf")

### Selectionner seulement les colonnes qui nous interessent
df     <- df[,c("ADM1_NAME","ADM1_CODE")]
names(df)<-c("dept","code")

df$type <- "rural"
df[df$code == 16838 | 
     df$code == 16839,]$type <- "urbain"

### Exporter comme fichier DBF --> permet toutes manipulations sur les DBF
write.dbf(df,"data/vector/coteivoire_gaul_adm1_geo_modifie.dbf")


### Lire un fichier geospatial/vecteur: fonction "readOGR"
poly_admin <- readOGR(dsn="data/vector/coteivoire_gaul_adm1_geo.shp",
                      layer="coteivoire_gaul_adm1_geo")


### Obtenir les caracteristiques d'un fichier vecteur
summary(poly_admin)


### Re-Projeter un fichier vecteur
poly_utm <- spTransform(poly_admin, 
                        CRS("+init=epsg:32630"))

### Extraire le systeme de projection et l'etendue comme des variables
my_crs <- projection(poly_utm)
my_ext <- extent(poly_utm)

extent(poly_admin)
extent(poly_utm)


### Creer un raster vide de la meme etendue et projection que le vecteur, avec une resolution de 1km
temp   <- raster(poly_utm,resolution=1000,
                 ext=my_ext,crs=my_crs)


### Emplir le raster avec 
#les valeurs du vecteur initial 
#pour le champs "ADM1_NAME"

raster <- rasterize(
  x=poly_utm,
  y=temp,       
  field="ADM1_CODE",   
  background=0,
  fun='first',
  update=TRUE)



### Afficher le raster et le vecteur par dessus
plot(raster)
plot(poly_utm,add=TRUE)


### Creer un extrait du fichier vecteur 
mon_dbf <- poly_admin@data
summary(mon_dbf)
levels(as.factor(mon_dbf$ADM1_NAME))

mon_poly <- 
  poly_utm[poly_utm$ADM1_NAME=="Bas Sassandra",]

mon_poly


### Ajouter le departement en vert
plot(mon_poly,add=T,col="blue")


### Creer un deuxieme departement
comoe <-poly_utm[poly_utm$ADM1_NAME=="Comoe",]


### Fusionner les deux polygones
union <- gUnion(comoe,mon_poly)
plot(union,add=T,col="red")


### Transformer une table en fichier spatial-vecteur
mes_points    <- read.csv("data/table/points_aleatoires.csv")
summary(mes_points)


# ######### Transformer en data frame spatial
sp_df<-SpatialPointsDataFrame(
  coords = mes_points[,c(1,2)],
  data   = data.frame(mes_points[,c(4,3)]),
  proj4string=CRS("+proj=longlat +datum=WGS84")
)


# ######### Exporter en KML
writeOGR(obj=sp_df,dsn="data/vector/mes_points.kml",
         layer="mes_points",
         driver = "KML",
         overwrite=T)


# ######### Reprojeter un fichier vecteur (ici des points)
proj4string(poly_utm)
pts_utm <- spTransform(sp_df,proj4string(poly_utm))


### Selectionner des elements dans un vecteur par leur localisation 
pts_poly <- pts_utm[mon_poly,]
points(pts_poly)
summary(pts_poly)

### Calculer le nombre d'element d'un vecteur selon les geometries d'un autre vecteur
tmp <- aggregate(x = pts_utm["id"],
               by = union,
               FUN = length)

poly_utm@data$IFN_pts <- tmp$id


### Joindre des attributs spatiaux 
pts_utm$DPT <- aggregate(
              x = poly_utm["ADM1_NAME"],
              by=pts_utm,
              FUN=first)$ADM1_NAME

head(pts_utm)
table(pts_utm$DPT)

