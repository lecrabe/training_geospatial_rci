##########################################################################################
################## Module gestion de table, creation d'une matrice de confusion    #######
##########################################################################################

# Points abordes :
# - creer un data frame
# - manipuler un data frame
# - ecrire fichier CSV, TXT

#############################################################a############################# 
# Derniere mise e jour 26/08/2015
# remi.dannunzio@fao.org
##########################################################################################

##########################################################################################
################## Options de base, paquets
##########################################################################################
options(stringsAsFactors=FALSE)

setwd("/media/xubuntu/data_ofgt/")


### Creer un vecteur: fonctions "<-" et ":" 
vecteur <- 0:20


### Repeter des elements: fonction "rep"
rep("bonjour",3)


### Creer un vecteur: fonction "c()"
code_1990 <- c(0,1,0,1,rep(0,17))

code_2000 <- rep(0,21)
code_2015 <- code_2000

code_2000[10]<-1
code_2015[c(7,8,10,17,19)]<-1


### Creer une table: fonction "data.frame()"
df <- data.frame(vecteur)

### Associer des vecteur en colonne: fonction "cbind()"
df <- cbind(vecteur,code_1990)
df <- cbind(df,code_2000)
df <- cbind(df,code_2015)

### Afficher la structure  d'un objet: fonction "str"
str(df)

### Changer le type d'un objet: fonction "as"
df <- as.data.frame(df)
str(df)



### Afficher le haut d'une table: fonction "head"
head(df)
head(df,2)


### Afficher le nom des colonnes d'une table: fonction "names"
names(df) 


### Changer le nom d'une colonne d'une table
names(df)[1] <- "classe_origine"


### Concatener des elements : fonction "paste"
paste("je commence "," R",sep="avec")
paste("classe",vecteur,sep="")


### Ajouter une colonne dans une table
df$colonne_nouvelle <- paste("classe",vecteur,sep="")


### Extraire une colonne d'une table: fonction "$"
df2 <- df$classe_origine
df2


### Tester une condition
df2 == vecteur


### Suppimer un objet: fonction "rm"
rm(df2)


### Afficher la longueur d'un vecteur/table: fonction "length"
length(df$classe_origine)


### Afficher la classe d'un vecteur/table: fonction "class"
class(df$colonne_nouvelle)
class(df$classe_origine)


### Extraire un element / une ligne / une colonne: fonction "[,]" 
df[5,]
df[df$classe_origine > 10,]
df[,"classe_origine"]
df[df$classe_origine == 13,]


### Determiner les valeurs uniques: fonction "unique"
unique(df$code_1990)


### Afficher les niveaux d'une variable: fonction "levels" 
levels(df$code_1990)


### Changer le type d'une variable: fonction "as.XXXXX"
### NB: plusieurs fonctions imbriquees, l'indentation est automatique

(legend <- levels(as.factor(df$code_1990)
                  )
 )

### Afficher un comptage des elements par colonne: fonction "table"
table(df$code_1990)
table(df$code_1990,df$code_2000)


### Creer un sous-dataset 
out <- df[,1:4]


### Exporter resultats en CSV
write.csv(file="data/table/classes.csv",out,row.names=F)

