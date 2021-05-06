###############################################################################
## Christine Plumejeaud-Perreau, U.M.R 7266 LIENSS
## Script pour visualisation et analyse des résultats de PAMELI sur Aytré, 25/02/2021
## Source:  U.M.R. LIENSS 7266
###############################################################################

#setwd("C:/Travail/CNRS_mycore/PAMELI/Scripts/")
setwd("~/R/TP_CMI")
getwd()

meslibrairiesR <- "C:/Tools/R4"
# Ajouter meslibrairiesR dans ces chemins : 
.libPaths(c( .libPaths(), meslibrairiesR) )
#install.packages(c("OpenStreetMap", "regex"), meslibrairiesR)
#install.packages("rgrs", meslibrairiesR)
## Charger les libraries
library(tidyverse)
library(lubridate)

#Pour les correlations
library(corrplot)
library(texreg)
library(Hmisc)

## Carto
library(sf)
library(cartography)
library(sp) #pour bbox
library(tmap)

library(knitr)
library(rmarkdown)
library(markdown)

#install.packages(c("tmap"), dependencies=TRUE)


## Sauver / reprendre son travail sous R
# https://mtes-mct.github.io/parcours-r/m1/sauvegarder-son-travail.html 
dir.create("./outputs")

save(list = ls(), file = "outputs/env_entier.RData") 
# sauvegarde de tout l'environnement sur le répertoire choisi 

rm(list = ls()) # suppression de notre environnement dans R 

load("outputs/env_entier.RData") 
# chargement de l'environnement stocké sur l'ordinateur 


################################################################################
## Lire les données
data <- read.csv("aytre_25fev2021_ysiexo2_gps.csv", header=TRUE, sep=";", dec=",", stringsAsFactors=FALSE, encoding = "UTF-8")

summary(data)

#point_id devrait être un entier, mais il est lu comme une chaîne de caractères 
# "75 177" au lieu de 75177
res <- vector("integer", length(data$point_id))
for (k in 1:length(data$point_id)){
  res[k] <- as.integer(gsub("[^0-9]", "", data$point_id[k])) 
}
data$point_id <-res

data <- data %>%
  mutate_at(vars(sensor_name, property_name, unit_code, unit_name), list(as.factor))  %>%
  mutate_at(vars(property_value, pt_longitude, pt_latitude), list(as.numeric)) %>%
  mutate_at(vars(is_reference), list(as.logical)) 


## Process time and spatial dimension
data <- data %>% 
  mutate(date = ymd(data$date)) %>% 
  mutate(local_time = as_datetime(data$local_time))%>% 
  mutate(geom_point = st_as_sfc(data$geom_point)) 

# convert data to a spatial dataframe
#R uses Simple Features (sf) , read https://r-spatial.github.io/sf/articles/sf1.html
datasf <- st_as_sf(data)
datasf <- st_set_crs(datasf, 4326)

# data[1,]$local_time
"2020-09-17 16:55:12"

# data[1,]$date
"2020-09-17" ymd

summary(datasf)


################################################################################
## Une carte toute simple avec cartography
## Emprise de la sortie, trait de cote, et trajectoire du drone
################################################################################

## 1. Lecture du fond de carte
getwd() #Vérifier votre répertoire de travail


library("sf")
library("cartography")

## Cadre
footprint<-st_read(dsn="shp/emprise_Aytre_17092020_4326.shp", stringsAsFactors = FALSE)
## cote
cote<-st_read(dsn="shp/cote_GSHHS_f_L1_4326.shp", stringsAsFactors = FALSE)
cote_precise<-st_read(dsn="shp/cote_precise_line_4326.shp", stringsAsFactors = FALSE)
## Trajectoire du drone (simplifiée)
drone_path<-st_read(dsn="shp/curve_Aytre_25022021_4326.shp", stringsAsFactors = FALSE)
## 20 points d'échantillonage théorique
#options(encoding="UTF-8")
sampling<-st_read(dsn="geojson/sampling_4326.geojson", stringsAsFactors = FALSE)

colnames(sampling) #"id"     "level"      "source"     "parent_id"  "sibling_id" "area" "geometry"

## 2. Dessin de la carte

## Refaire le graphique avec des marges réduites
## Les couches et styles cartographiques s'empilent les uns sur les autres dans l'ordre d'appel
## comme dans un SIG classique
# reduire la largeur des marges avec par(mar=())
dev.off()


# c(bas, gauche, haut, droite)
# OMA : augmenter la largeur de la MARGE EXTERNE (oma) sur les bords gauche et droit de 1
# MAR : reduire la largeur de la MARGE INTERNE (mar), sauf en haut ( + 1.5)
par(mar = c(0.5,0,1.5,0), oma = c(0,7,0,7))

# La carte basique avec les polygones
plot(st_geometry(footprint), col="lightblue", lwd=1)
plot(st_geometry(cote), col="#93C572", add=T)
plot(st_geometry(cote_precise), col="#006400", add=T)

plot(st_geometry(drone_path), col="black", add=T, lwd=2)
plot(st_geometry(sampling), col="red", add=T)
labelLayer(x = sampling, txt="id", halo= TRUE,overlap=FALSE)

# Rajouter le cartouche, la légende, l'échelle, etc.
#Options : https://rdrr.io/cran/cartography/man/layoutLayer.html
layoutLayer(title = "Espace d'étude sur la baie d'Aytré, 25-02-2021",
            sources = "Source : UMR 7266 LIENSS",
            scale = FALSE, tabtitle = TRUE,
            frame = FALSE,theme = "blue.pal",
            north = TRUE)




############################ ZOOM #############################################

## Zoomer sur la zone à échantillonner (Aytré)

##calculer des bornes en coordonnées géographiques (longitude, latitude)
zzoom <- bbox(SpatialPoints(cbind(sampling$coord_x, sampling$coord_y)))
# min       max
# coords.x1 -1.139645 -1.124722
# coords.x2 46.110383 46.126291

## Prendre 10% de marge en hauteur et 15 % en largeur
deltax <- (zzoom[1,2]-zzoom[1,1])*0.15
deltay <- (zzoom[2,2]-zzoom[2,1])*0.1

## Calcul des points extrêmes : st_point(x, y) avec x = longitude, y=latitude
sudouest <- st_point(c(zzoom[1,1]-deltax,zzoom[2,1]-deltay)) 
sudest <- st_point(c(zzoom[1,2]+deltax,zzoom[2,1]-deltay))
nordouest <- st_point(c(zzoom[1,1]-deltax,zzoom[2,2]+deltay))
nordest <- st_point(c(zzoom[1,2]+deltax,zzoom[2,2]+deltay))

#Fabrique du polygone rectangle d'emprise
p1 <- rbind(sudouest, nordouest, nordest, sudest, sudouest)
pol <-st_polygon(list(p1))
zzoom_sf <- st_sfc(pol, crs=4326)

#Découpe des autres géometries pour ne retenir que l'intersection avec ce rectangle
footprintz <- st_intersection(x = footprint, y = zzoom_sf)
cotez <- st_intersection(x = cote, y = zzoom_sf)
coteprecisez <- st_intersection(x = cote_precise, y = zzoom_sf)
drone_pathz <- st_intersection(x = drone_path, y = zzoom_sf)

## Limiter les données à celles de la zone d'Aytré
datasfz <- st_intersection(x = datasf, y = zzoom_sf)

dev.off()

# reduire la largeur des marges avec par(mar=())
# c(bas, gauche, haut, droite)
# OMA : augmenter la largeur de la MARGE EXTERNE (oma) sur les bords gauche et droit de 1
# MAR : reduire la largeur de la MARGE INTERNE (mar), sauf en haut ( + 1.5)
par(mar = c(0.5,0,1.5,0), oma = c(0,1,0,1))

# La carte basique avec les polygones
plot(st_geometry(footprintz), col="lightblue", lwd=1)
#plot(st_geometry(zzoom_sf), col="lightblue", lwd=1)
plot(st_geometry(cotez), col="#93C572", add=T) ##vert
plot(st_geometry(coteprecisez), col="#006400", add=T) ##vert
plot(st_geometry(drone_pathz), col="black", add=T, lwd=2)
plot(st_geometry(sampling), col="red", add=T)
labelLayer(x = sampling, txt="id", halo= TRUE,overlap=FALSE)

# Rajouter le cartouche, la légende, l'échelle, etc.
#Options : https://rdrr.io/cran/cartography/man/layoutLayer.html
layoutLayer(title = "Espace d'étude sur la baie d'Aytré, 25-02-2021",
            sources = "Source : UMR 7266 LIENSS",
            scale = FALSE, tabtitle = TRUE,
            frame = FALSE,theme = "blue.pal",
            north = TRUE)

###########################################################################
## Créer le template cartographique avec tmap
## CF https://github.com/riatelab/mexusaborder/blob/master/texmex.R
###########################################################################
dev.off()

library(tmap)
tmap_mode("plot") ##One option


basemap <- function(titlep = "") {
  # c(bas, gauche, haut, droite)
  # OMA : augmenter la largeur de la MARGE EXTERNE (oma) sur les bords gauche et droit de 1
  # MAR : reduire la largeur de la MARGE INTERNE (mar), sauf en haut ( + 1.5)
  par(mar = c(1,0,1.5,0), oma = c(2,7,0,7))
  
  map <- tm_shape(footprintz) +
    tm_borders() +
    ##tm_shape(cotez) + 
    ##tm_fill(col="#93C572") +
    tm_shape(coteprecisez) +
    tm_lines(col="#006400", lwd=2) + ##vert foncé
    tm_scale_bar(position = c(0.05,0.2)) + #position = c("left", "bottom")
    tm_compass(position = c(0.05,0.3)) +#c("left", "top")
    tm_layout(main.title = titlep, 
              main.title.size=0.7,
              title.bg.color="#59C7EB",
              legend.outside = TRUE)
  return (map)
}

## Utiliser le template pour exporter en resolution 150 DPI la carte 
## avec la trace du drone et les points d'échantillonage
sizes_aea <- getFigDim(x = zzoom_sf, mar = c(0,0,1.2,0), res = 150)
png("outputs/fig03.png", width = sizes_aea[1], height = sizes_aea[2], res = 150)
map <- basemap("Zoom sur l'espace d'étude : la baie d'Aytré, \n 25-02-2021")
# Voir ce template


map <- map+
  tm_shape(drone_pathz) +
  tm_lines(col="black") + #Trace du drone
  tm_shape(sampling) +
  tm_dots(col = "red", size=0.7)+ #Points d'échantillonage
  tm_text("id", size=0.7, auto.placement=TRUE, bg.color="white", bg.alpha=0.5, just=c(0,0)) 

map #Dessiner la carte dans le device (ici le fichier fig03.png)
dev.off() ## Fermer et sauver le fichier fig03.png

map #Voir la carte dans Plots

dev.off() ## Effacer les plots

##############################################################################
# Analyser l'évolution des paramètres mesurés
##############################################################################

# Pour chaque variable : limitée à la partie dans la baie d'Aytré entre le  temps du point 1 et le temps du point 20 
#   1. évolution des paramètres avec le temps en abscisse, la valeur du paramètre en ordonnée, 
#   2. Superposer les courbes.
#   3. Sur une carte, coloriser la trajectoire avec une palette divergente suivant la valeur de cet indicateur (en dessous/ au dessus de 0)

## 1. evolution des paramètres


## Le pb est que les unités de valeurs ne sont pas comparables
ggplot(data=datasfz, aes(x=local_time, y=property_value, colour=property_name)) +
  geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.1, se=FALSE)



## On filtre sur les 4 variables d'intérêt
## Puis on regroupe par ces 4 variables (group_by) 
## Puis on calcule un nouvel indicateur 'ratio' sans unité, qui vaut 0 quand la variable est moyenne
## Puis on calcule un nouvel indicateur 'std' sans unité, qui vaut 0 quand la variable est moyenne mais dont l'écart est standardisé

subsetdatasf <- datasfz %>% 
  filter(property_name %in% c('turbidity_NTU', 'temp_C','salinity_ppt','chlorophyll_mg/L' ) ) %>%
  group_by(property_name) %>%
  mutate(ratio = -(1-(property_value/mean(property_value))))%>%
  mutate(std = (property_value-mean(property_value))/(sd(property_value)))


# turbidity_ratio
# -1.1362166 -1.1315408 -1.1362166 -1.1315408
# subsetdatasf[subsetdatasf$property_name=='turbidity_NTU', ]$ratio
# [1] -0.60379541 -0.59911962 -0.60379541 -0.59911962

## property_name est une variable factorielle, qui a gardé l'ensemble de ses 12 levels, 
## même après le tri sur 4 variables
unique(subsetdatasf$property_name)
# [1] salinity_ppt     temp_C           turbidity_NTU    chlorophyll_mg/L
# 9 Levels: chlorophyll_mg/L conductivity_mS/cm depth_m fDOM_rfu ODOsat_%sat ... turbidity_NTU

## On reset les levels pour 4 niveaux
levels(subsetdatasf$property_name) <- list(temp_C="temp_C", turbidity_NTU="turbidity_NTU", chlorophyll_RFU="chlorophyll_mg/L", salinity_ppt="salinity_ppt")


### 1.1 Evolution de la turbidité

## Filtrer le dataset zoomé sur Aytré pour ne traiter que turbidity
# Critère : property_name=='turbidity_NTU'
oneparam <- subsetdatasf %>% 
  filter(property_name=='turbidity_NTU') 

summary(oneparam)
colnames(oneparam)
# [1] "context_id"     "reason"         "date"           "sensor_name"   
# [5] "property_name"  "property_value" "unit_code"      "unit_name"     
# [9] "point_id"       "pt_longitude"   "pt_latitude"    "local_time"    
# [13] "is_reference"   "geom_point"     "ratio"          "std"

# 0. Exploration descriptive de la turbidité

summary(oneparam$ratio)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.7126 -0.5256 -0.0472  0.0000  0.2901  5.3707 
hist(oneparam$ratio, main="Distribution de l'écart à la turbidté moyenne")

#### 1.1.1 turbidity dans le temps

dev.off() ##Vide l'écran
g <- ggplot(oneparam , aes(x=local_time, y=property_value)) 
g <- g + geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.1, se=FALSE) 
g <- g + geom_hline(yintercept=mean(oneparam$property_value), linetype="dashed", color = "red")
g <- g + ggtitle("Evolution de la turbidité sur Aytré") # pour le titre principal
g <- g  + xlab("temps") # pour le titre de l'axe des x
g <- g  + ylab("Turbidity") # pour le titre de l'axe des y

g
dev.copy(png,'./outputs/turbidity_zoom.png')
dev.off()

#### 1.1.2 ratio dans le temps

dev.off() ##Vide l'écran
g <- ggplot(oneparam , aes(x=local_time, y=ratio)) 
g <- g + geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.1, se=FALSE) 
g <- g + geom_hline(yintercept=0, linetype="dashed", color = "red")
g <- g + ggtitle("Evolution de la turbidité sur Aytré") # pour le titre principal
g <- g  + xlab("temps") # pour le titre de l'axe des x
g <- g  + ylab("Distance à la moyenne") # pour le titre de l'axe des y
g
dev.copy(png,'./outputs/turbidity_ratio_zoom.png')
dev.off()


#### 1.1.3 std dans le temps

dev.off() ##Vide l'écran
g <- ggplot(oneparam , aes(x=local_time, y=std)) 
g <- g + geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.1, se=FALSE) 
g <- g + geom_hline(yintercept=0, linetype="dashed", color = "red")
g <- g + ggtitle("Evolution de la turbidité sur Aytré") # pour le titre principal
g <- g  + xlab("temps") # pour le titre de l'axe des x
g <- g  + ylab("Distance à la moyenne standardisée") # pour le titre de l'axe des y
g
dev.copy(png,'./outputs/turbidity_std_zoom.png')
dev.off()

### Carto de cette évolution 

#https://github.com/riatelab/mexusaborder/blob/master/texmex.R
#https://bookdown.org/nicohahn/making_maps_with_r5/docs/tmap.html


library(tmap)
dev.off()
map <- basemap("Espace d'étude : la baie d'Aytré, 25-02-2021")
tmap_mode("plot") ##One option
#tmap_mode("view") ##Or the other
## Choisir sa palette de couleur
# tmaptools::palette_explorer()
maPalette <- tmaptools::get_brewer_pal("RdYlBu", n = 4)

## Carte de turbidité
dev.off()
sizes_aea <- getFigDim(x = zzoom_sf, mar = c(0,0,1.2,0), res = 150)
png("outputs/raw_turbidity_map_zoom.png", width = sizes_aea[1], height = sizes_aea[2], res = 150)
map_param <- map + 
  tm_shape(oneparam) +
  tm_dots(col = "property_value", midpoint = median(oneparam$property_value), palette=rev(maPalette), style="order") 
map_param
dev.off()

## Carte de std de turbidité
dev.off()
png("outputs/turbidity_map_zoom.png", width = sizes_aea[1], height = sizes_aea[2], res = 150)
map_param <- map + 
  tm_shape(oneparam) +
  tm_dots(col = "std", 
          midpoint = median(oneparam$std), palette=rev(maPalette), style="order")

map_param
dev.off()

################################################################################
##Refaire pour température, salinity, chlorophylle
################################################################################



## Filtrer le dataset zoomé sur Aytré pour ne traiter que température
# Critère : property_name=='temp_C'
oneparam <- subsetdatasf %>% 
  filter(property_name=='temp_C')

hist(oneparam$ratio)

## Carte de température
dev.off()
png("outputs/temperature_map_zoom.png", width = sizes_aea[1], height = sizes_aea[2], res = 150)
map_param <- map + 
  tm_shape(oneparam) +
  tm_dots(col = "std", midpoint = median(oneparam$std), palette=rev(maPalette), style="order") 
map_param
dev.off()

## Filtrer le dataset zoomé sur Aytré pour ne traiter que salinité
# Critère : property_name=='salinity_ppt'
oneparam <- subsetdatasf %>% 
  filter(property_name=='salinity_ppt')

hist(oneparam$ratio)
median(oneparam$ratio)
## Carte de salinité
dev.off()
png("outputs/salinite_map_zoom.png", width = sizes_aea[1], height = sizes_aea[2], res = 150)
map_param <- map + 
  tm_shape(oneparam) +
  tm_dots(col = "std", midpoint = median(oneparam$std), palette=rev(maPalette), style="order") 
map_param
dev.off()

## Filtrer le dataset zoomé sur Aytré pour ne traiter que chlorophylle
# Critère : property_name=='chlorophyll_RFU'
oneparam <- subsetdatasf %>% 
  filter(property_name=='chlorophyll_RFU')
hist(oneparam$ratio)
median(oneparam$ratio)

## Carte de chlorophylle
dev.off()
png("outputs/chlorophylle_map_zoom.png", width = sizes_aea[1], height = sizes_aea[2], res = 150)
map_param <- map + 
  tm_shape(oneparam) +
  tm_dots(col = "std", midpoint = median(oneparam$std), palette=rev(maPalette), style="order") 
map_param
dev.off()

###############################################################################
## 2. Superposer les courbes.
##############################################################################


## Afficher un graphique montrant l'évolution des 4 variables en fonction du temps
ggplot(data=subsetdatasf, aes(x=local_time, y=property_value, colour=property_name, linetype=property_name)) +
  geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.1, se=FALSE)+
  ggtitle("Evolution de 4 paramètres sur la baie d'Aytré")+ # pour le titre principal
  xlab("temps")+# Légende axe des abscisses 
  ylab("Mesures (différents unités)") # pour le titre de l'axe des y
dev.copy(png,'./outputs/raw_4courbes_Aytré.png')
dev.off()

## Afficher un graphique montrant l'évolution des 4 variables standardisées en fonction du temps
ggplot(data=subsetdatasf, aes(x=local_time, y=std, colour=property_name, linetype=property_name)) +
  geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.1, se=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  ggtitle("Evolution de 4 paramètres sur la baie d'Aytré")+ # pour le titre principal
  xlab("temps")+# Légende axe des abscisses 
  ylab("Ecart standardisé à la moyenne") # pour le titre de l'axe des y
dev.copy(png,'./outputs/4courbes_Aytré_standard.png')
dev.off()




############################################################################
## 4. Corrélations
## https://dominicroye.github.io/en/2019/tidy-correlation-tests-in-r/
############################################################################

#Rappel
# subsetdatasf <- datasfz %>% 
#   filter(property_name %in% c('turbidity_NTU', 'temp_C','salinity_ppt','chlorophyll_mg/L' ) ) %>%
#   group_by(property_name) %>%
#   mutate(ratio = -(1-(property_value/mean(property_value))))%>%
#   mutate(std = (property_value-mean(property_value))/(sd(property_value)))

# On ne prend que le strict nécessaire
tab <- subsetdatasf %>%
  select(c('property_name', 'property_value', 'std', 'point_id' ))  
tab <- st_drop_geometry(tab)

#data_nest <- group_by(tab, property_name) %>% nest()

## On fait la jointure par point_id entre chaque paramètre à étudier conjointement
test <-  tab %>% 
  filter(property_name %in% c('turbidity_NTU'))  %>% 
  inner_join(tab %>% filter(property_name %in% c('chlorophyll_RFU')),  
    by = c("point_id"))     %>% 
  rename(turbidity = property_value.x)%>% 
  rename(chloro = property_value.y)%>% 
  rename(turbiditySTD = std.x)%>% 
  rename(chloroSTD = std.y)%>% 
  select(-c("property_name.x","property_name.y" )) %>%
  inner_join(tab %>% filter(property_name %in% c('temp_C')),  
             by = c("point_id")) %>% 
  rename(temp = property_value)%>% 
  rename(tempSTD = std)%>% 
  select(-c("property_name" )) %>%
  inner_join(tab %>% filter(property_name %in% c('salinity_ppt')),  
             by = c("point_id"))%>%
  rename(salinity = property_value)%>% 
  rename(salinitySTD = std)%>% 
  select(-c("property_name" )) 

colnames(test)
# [1] "turbidity"    "turbiditySTD" "point_id"     "chloro"       "chloroSTD"   
# [6] "temp"         "tempSTD"      "salinity"     "salinitySTD" 

Mat<-test[,c("turbidity", "chloro", "temp", "salinity")]
MatSTD<-test[,c("turbiditySTD", "chloroSTD", "tempSTD", "salinitySTD")]

round(cor(Mat), 2)
round(cor(MatSTD), 2)

#graphique avec corrplot
library(corrplot)
corrplot(cor(Mat),method="number")
dev.copy(png,'./outputs/correlations.png')
dev.off()

corrplot(cor(MatSTD),method="number")

############################################################################
## 5. Calcul de l'oxygène soluble attendu par effet thermique et salin (Aminot & Kérouel, 2004)
############################################################################
test$Oxygene<-0.0223916*100*(exp(-135.90205+(1.575701*10^5)/(test$temp+273.15)-(6.642308*10^7)/(test$temp+273.15)^2+(1.2438*10^10)/(test$temp+273.15)^3-(8.621949*10^11)/(test$temp+273.15)^4-test$salinity*(0.017674-10.754/(test$temp+273.15)+2140.7/(test$temp+273.15)^2))/100)


test$OxygeneSTD<-(test$Oxygene-mean(test$Oxygene))/sd(test$Oxygene)

Mat<-test[,c("turbidity", "chloro", "temp", "salinity", "Oxygene")]
corrplot(cor(Mat),method="number")
dev.copy(png,'./outputs/corr_oxygene_chloro.png')
dev.off()

## Normalement la chlorophylle devrait évoluer/suivre les pics d'oxygène dissous

##Faire la jointure entre test$Oxygene et subsetdatasf par point_id
test2 <-  subsetdatasf %>% 
  left_join(test %>% select(c("Oxygene","OxygeneSTD", "point_id" )), by = c("point_id"))  

colnames(test2)
# [1] "context_id"     "reason"         "date"           "sensor_name"    "property_name"  "property_value"
# [7] "unit_code"      "unit_name"      "point_id"       "pt_longitude"   "pt_latitude"    "local_time"    
# [13] "is_reference"   "geom_point"     "ratio"          "std"            "Oxygene"        "OxygeneSTD"

## Afficher un graphique montrant l'évolution de la chrlorophylle et l'oxygène dissous en fonction du temps
ggplot(test2 %>% filter(property_name %in% c('chlorophyll_RFU')), aes(x=local_time)) + 
  geom_line(aes(y = std), color = "darkgreen") + 
  geom_line(aes(y = OxygeneSTD), color="steelblue", linetype="twodash") +
  ggtitle("Evolution conjointe de la chlorophylle et l'oxygène dissous \n sur la baie d'Aytré")+ # pour le titre principal
  xlab("temps")+# Légende axe des abscisses 
  ylab("Ecart standardisé à la moyenne") # pour le titre de l'axe des y

## Lisser
ggplot(test2 %>% filter(property_name %in% c('chlorophyll_RFU')), aes(x=local_time)) + 
  geom_smooth(aes(y = std), method = 'loess', span=0.1, color = "darkgreen") + 
  geom_smooth(aes(y = OxygeneSTD), method = 'loess', span=0.1, color="steelblue", linetype="twodash") +
  ggtitle("Evolution conjointe de la chlorophylle et l'oxygène dissous \n sur la baie d'Aytré")+ # pour le titre principal
  xlab("temps")+# Légende axe des abscisses 
  ylab("Ecart standardisé à la moyenne") # pour le titre de l'axe des y
dev.copy(png,'./outputs/oxygene_chloro.png')
dev.off()

########################################################################
########################################################################
# calculer la distance à la côte
########################################################################
########################################################################
#https://gis.stackexchange.com/questions/243994/how-to-calculate-distance-from-point-to-linestring-in-r-using-sf-library-and-g

colnames(coteprecisez)
#"NumDep"   "Annee"    "NUM"      "Source"   "COMM"     "Date_PVA" "geometry"
colnames(datasfz)
[1] "context_id"     "reason"         "date"           "sensor_name"   
[5] "property_name"  "property_value" "unit_code"      "unit_name"     
[9] "point_id"       "pt_longitude"   "pt_latitude"    "local_time"    
[13] "is_reference"   "geom_point" 


distances <- st_distance(x = datasfz, y = coteprecisez)
#Pourquoi cette sortie est fausse (expliquez à partir de la lecture de la documentation de st_distance)
rm(distances) 

# Solution
one_singleline <- st_multilinestring(st_geometry(coteprecisez))
one_singleline <- st_sfc(one_singleline)
one_singleline <- st_set_crs(x = one_singleline, value = 4326)

distancesCote <- st_distance(x = subsetdatasf, y = one_singleline)

## Distribution des distances à la côte calculées
hist(distancesCote)
dev.copy(png,'./outputs/histogramme_distance_cote.png')
dev.off()

# head(distancesCote)
# print(distancesCote)
# class(distancesCote)
# class(as.numeric(distancesCote))

subsetdatasf <- cbind(subsetdatasf, distance = as.numeric(distancesCote))

# class(subsetdatasf)
# colnames(subsetdatasf)
# [1] "context_id"     "reason"         "date"           "sensor_name"   
# [5] "property_name"  "property_value" "unit_code"      "unit_name"     
# [9] "point_id"       "pt_longitude"   "pt_latitude"    "local_time"    
# [13] "is_reference"   "ratio"          "std"            "distance"      
# [17] "geom_point"

## Faire une carte de l'évolution de la distance à la cote
dev.off()
png("outputs/distancecote_map_zoom.png", width = sizes_aea[1], height = sizes_aea[2], res = 150)
map_param <- map + 
  tm_shape(subsetdatasf) +
  tm_dots(col = "distance", midpoint = median(subsetdatasf$distance), palette=rev(maPalette), style="order") 
map_param
dev.off()

########################################################################
# calculer la corrélation entre turbidité et distance à la cote
########################################################################
oneparam <- subsetdatasf %>% 
  filter(property_name=='turbidity_NTU') 

Mat<-st_drop_geometry(oneparam[,c("property_value", "distance")])
# testez aussi avec "ratio", "std"
round(cor(Mat), 2)

library(corrplot)
corrplot(cor(Mat),method="number")
dev.copy(png,'./outputs/corr_distance_cote_turbidite.png')
dev.off()

library (Hmisc)
rescorr<-rcorr(as.matrix(Mat))
MatCor<-rescorr$r

# property_value   distance
# property_value      1.0000000 -0.6191052
# distance           -0.6191052  1.0000000

# c(bas, gauche, haut, droite)
# OMA : augmenter la largeur de la MARGE EXTERNE (oma) sur les bords gauche et droit de 1
# MAR : reduire la largeur de la MARGE INTERNE (mar), sauf en haut ( + 1.5)
par(mar = c(1,1,1,1), oma = c(1,1,1,1))

## Afficher un graphique montrant l'évolution des 4 variables standardisées avec la distance standardisée à la côte en fonction du temps
ggplot(data=subsetdatasf, aes(x=local_time, y=std, colour=property_name, linetype=property_name)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_smooth(aes(y=(distance-mean(distance))/sd(distance)), color="#006400", method = 'loess', formula = 'y ~ x', span = 0.1, se=FALSE)+
  geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.1, se=FALSE)+
  ggtitle("Evolution de 4 paramètres sur la baie d'Aytré, \n avec la distance standardisée à la côte")+ # pour le titre principal
  xlab("temps")+# Légende axe des abscisses 
  ylab("Ecart standardisé à la moyenne") # pour le titre de l'axe des y
dev.copy(png,'./outputs/5courbes_Aytré_standard.png')
dev.off()

##############################################################################
# regression linéaire entre distance à la cote et turbidité
# commenter
##############################################################################

model<-lm(property_value~distance,data=oneparam)
summary(model)

# Call:
#   lm(formula = property_value ~ distance, data = oneparam)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -23.625  -9.688  -2.938   4.484 138.971 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 44.7417305  0.3224304  138.76   <2e-16 ***
#   distance    -0.0415067  0.0006943  -59.78   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.03 on 5750 degrees of freedom
# Multiple R-squared:  0.3833,	Adjusted R-squared:  0.3832 
# F-statistic:  3574 on 1 and 5750 DF,  p-value: < 2.2e-16
# 

library("texreg")

screenreg (model, ci.force = TRUE)

# ===========================
#   Model 1       
# ---------------------------
#   (Intercept)    44.74 *     
#   [44.11; 45.37]
# distance       -0.04 *     
#   [-0.04; -0.04]
# ---------------------------
#   R^2             0.38       
# Adj. R^2        0.38       
# Num. obs.    5752          
# ===========================

# Recalculer la valeur de pente a du modèle à la main : 
# turbidity = a * distance + b
# La valeur de turbidity est données par property_value dans notre cas

a <- cor(oneparam$property_value, oneparam$distance) * sd(oneparam$property_value)  / sd(oneparam$distance)
# a = -0.04150674 ok
b <- mean(oneparam$property_value) - a * mean(oneparam$distance) # 44.74173 ok
r <- cov(oneparam$property_value, oneparam$distance)/(sd(oneparam$property_value)*sd(oneparam$distance)) #-0.3769931

## figures

# Modèle 0
ggplot(data = oneparam,
       mapping = aes(x = distance, y=property_value)) + 
  geom_point(cex=0.5)+
  labs(x = "Distance à la côte en mètres",
       y = "Valeur de turbidité",
       title = "Relation entre la turbidité et l'éloignement à la côte",
       subtitle = "La forme de la relation est-elle linéaire ?",
       caption = "Source : master 2 CNRS, projet drone, LIENSS")
dev.copy(png,'./outputs/plot_model_regression.png')
dev.off()

ggplot(data = oneparam,
       mapping = aes(x = distance, y=scale(model$residuals))) + 
  geom_point(cex=0.5)+
  labs(x = "Distance à la côte en mètres",
       y = "Résidus normalisés par rapport à la distance",
       title = "Résidus de la régression entre la turbidité et l'éloignement à la côte",
       subtitle = "les résidus sont-ils homogènes ?",
       caption = "Source : master 2 CNRS, projet drone, LIENSS")
dev.copy(png,'./outputs/plot_model_residus.png')
dev.off()

