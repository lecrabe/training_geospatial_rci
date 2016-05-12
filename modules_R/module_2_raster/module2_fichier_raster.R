##########################################################################################
################## Module manipulation de raster     ,   extraction d'information  #######
##########################################################################################

# Points abordes :
# - lire un fichier raster
# - lire fichier CSV, TXT
# - creer une grille de point
# - extraire des informations
# - selectionner un sous-jeu de donnees
# - exporter les resultats en format vecteur

#############################################################a############################# 
# Derniere mise e jour 25/04/2016
# remi.dannunzio@fao.org
##########################################################################################

##########################################################################################
################## Options de base, paquets
##########################################################################################
options(stringsAsFactors=FALSE)

setwd("/media/xubuntu/data_ofgt/")
setwd("C:/Users/dannunzio/Documents/countries/cote_ivoire/trainings_cote_ivoire/training_geospatial_mai2016/")

library(rgdal)
library(raster)


##########################################################################################
### Lire un fichier raster: fonction "raster"
rast <- raster("data/raster/cci_rci.tif")
str(rast)

###  Afficher un fichier raster: fonction "plot"
plot(rast)

### Afficher des informations sur le raster
extent(rast)
proj4string(rast)
res(rast)
### Fonction "as.factor" et "levels" fonctionne aussi pour un raster
### La fonction () permet d'afficher le resultat d'une attribution
unique(rast)
(legend <- levels(as.factor(rast)))

# ######### Creer un extrait spatial: fonction "extent" et "crop"
e    <- extent(-6,-4,7,8)
crop <- crop(rast,e)
plot(crop)
?crop

# Calculer la frequence de chacune des classes du raster
freq(rast)

# ######### Tirer des points aleatoires sur l'etendue du raster: fonction "sampleRandom"
# ######### L'option "xy=TRUE" signifie que les coordonnees sont maintenues
tmp <- sampleRandom(rast,1000,xy=TRUE)
class(tmp)

# ######### Convertir en data frame
my_sample <- data.frame(tmp)
class(my_sample)

# ######### Changer les noms des colonnes
names(my_sample) <- c("x_coord","y_coord","value")
str(my_sample)
head(my_sample)

# ######### Extraire les colonnes latitude et longitude seulement
x<-my_sample$x_coord
y<-my_sample$y_coord

# ######### Afficher les points: fonction "plot"
plot(x,y)

# ######### Effacer le graphique
dev.off()

# ######### Creer un cadre vide de la taille du raster de depart
plot(my_sample$x_coord,my_sample$y_coord,
     type="n",xlab="longitude",ylab="latitude")

# ######### Afficher le raster: fonction "rasterImage"
rasterImage(as.raster(rast),xmin(rast),ymin(rast),xmax(rast),ymax(rast))

# ######### Afficher le raster: fonction "plot" en specifiant chaque couleur de classe
ext_legend <- read.delim("data/legends/legend_cci_R.txt",sep="\t")

classes <- ext_legend$id
colours <- ext_legend$color
plot(rast,col=colours,breaks=c(0,classes))
plot(rast)

# ######### Rajouter des points sur un graphique existant: fonction "points"
points(my_sample$x_coord,my_sample$y_coord)


# ######### Creer une colonne avec des identificateurs uniques: fonction "row"
my_sample$id <- row(my_sample)[,1]
head(my_sample)


# ######### Creer le vecteur logique qui indique si la valeur des points correspond a Terre Ferme: operateur logique "!="
list_logic <- my_sample$value != 210
head(list_logic)

# ######### Creer un sous-jeu de donnees (points e l'interieur du pays)
in_country <- my_sample[list_logic,]

points(in_country$x_coord,in_country$y_coord,col="grey")

# ######### Voir la distribution des points par valeurs
table(in_country$value)

# ######### Selectionner aleatoirement 100 points parmi la classe "Tree_cover_broadleaved_deciduous_open_(15-40%)": fonction "sample"
# ######### NB: la liste logique qui indique si la valeur correspond a la Terre Ferme est integree directement
ext_legend

pts_FP <-   my_sample[
  sample(
    my_sample[
      my_sample$value==62,]$id,100
    )
  ,]

# ######### Ajouter les points sur le graphique
points(pts_FP$x_coord,pts_FP$y_coord,col="darkgreen",pch=19)

################################################
################################################
# Cette procedure a permis de selectionner des points dans une classe donnee
# Mais qu'en est-il des classes plus rares ?
################################################


# ######### Extraire des points avec une valeur particulire: fonction "rasterToPoints"
start<-Sys.time()
rast_PP<-rasterToPoints(rast,
          fun=function(rast){rast==12})
Sys.time()-start

# ######### Transformer en data frame et ajouter une colonne identifiant unique
df_pts_PP<-as.data.frame(rast_PP)
names(df_pts_PP) <- c("x_coord","y_coord","value")
df_pts_PP$id<-row(df_pts_PP)[,1]

# ######### Selectionner aleatoirement 50 points parmi ces pixels individualises
pts_PP<-df_pts_PP[sample(df_pts_PP$id,50),]

# ######### Afficher les points nouveaux en rouge
points(pts_PP$x_coord,pts_PP$y_coord,col="red",pch=19)


# ######### Combiner les deux jeux de donnees: fonction "rbind"
mes_points <- rbind(pts_FP,pts_PP)


# ######### Verifier les valeurs des points finaux
table(mes_points$value)


write.csv(mes_points,"data/table/points_aleatoires.csv",row.names=F)


# ######### Exporter un raster
writeRaster(crop,"data/raster/my_crop.tif",overwrite=T)


# ######### Reechantillonner
rast_agg <- aggregate(rast,fact=3,fun=mode)
(res(rast_agg)*111320)[1]
??mod
