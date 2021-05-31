x=c("dismo","ggmap", "rgdal", "rgeos", "maptools", "plyr","dplyr", "tidyr", "tmap", "raster","mapdata","sp","spdep")
lapply(x, library, character.only=TRUE)

#On génère un polygone qui représente les limites de la zone d'étude, soit le massif des Vosges + la plaine jusqu'à la Moselle à l'ouest et le Rhin à l'est
lon=data.frame(c(6.11,6.11,8.37, 8.37))
lat=data.frame(c(47.64,49.17, 49.17,47.64))
p=cbind(lon,lat)
colnames(p)=c("lon","lat")
p=Polygon(p)
p=Polygons(list(p),1)
p=SpatialPolygons(list(p))
proj4string(p)=CRS("+init=epsg:4326") #Cette commande permet d'assigner le système de projection géographique à l'objet
p=spTransform(p, CRS("+init=epsg:2154")) #Cette commande permet de modifier le système de projection géographiques ainsi que toutes les coordonnées associées
r=raster(p)
res(r)=500 #500 correspond à la taille des mailles en mètres. On peut faire varier ce paramètre à loisir.
r
data=as.data.frame(matrix(1:(nrow(r)*ncol(r)),nrow=ncol(r), ncol=nrow(r)))
data=as.data.frame(t(as.matrix(data)))
rownames(data)=data$V1
values(r)=as.matrix(data)


#Plus loin dans le script on voudra retirer les régions qui ne sont pas dans le Grand Est (Allemagne, Haute-Saône)
#Pour cela, on télécharge les contours de la régions depuis la couche OPENSTREETMAP disponible sur le l'URL https://www.data.gouv.fr/en/datasets/r/01fdab09-ed86-4259-b863-69913a3e04d1
# On dépose la couche dans le dossier de travail
#download.file(
#   url = "http://osm13.openstreetmap.fr/~cquest/openfla/export/departements-20140306-100m-shp.zip",
#   destfile = "shapefiles/departements-20140306-100m-shp.zip"
# )
# unzip(zipfile = "shapefiles/departements-20140306-100m-shp.zip",
#       exdir = "shapefiles/departements-20140306-100m-shp")
dpt=readOGR(dsn="departements-20180101.shp", layer="departements-20180101") #attention car la couche téléchargée est régulièrement mise à jour. Ainsi, il faudra corriger le nom de la couche lors d'une mise à jour ultérieure à 2018
#On extrait le Grand Est de la couche de tous les départements de France
gdEst=rbind(dpt[dpt@data$code_insee=="08",],dpt[dpt@data$code_insee=="10",],dpt[dpt@data$code_insee=="51",],dpt[dpt@data$code_insee=="52",],dpt[dpt@data$code_insee=="54",],dpt[dpt@data$code_insee=="55",],dpt[dpt@data$code_insee=="57",],dpt[dpt@data$code_insee=="67",],dpt[dpt@data$code_insee=="68",],dpt[dpt@data$code_insee=="88",])
gdEst=spTransform(gdEst,CRS("+init=epsg:2154"))


grid.poly=rasterToPolygons(r)
grid.poly=spTransform(grid.poly,CRS("+init=epsg:2154"))
grid.poly@data$ID=values(r)
grid.poly@data$layer=NULL
grid.poly=crop(grid.poly, gdEst)
raster::shapefile(grid.poly, file="grille_globale.shp") #Cette commande permet de sauvegarder la grille en couche shape

#Si on veut compartimenter en 3 zones avec 150 parcelles échantillons chacune
#D'abord, on sélectionne 150 lignes de la grille au hasard
d=data.frame(data[,1])
colnames(d)="ID"
sample=sample(d$ID,150, replace=F)

#subset1 (Lorraine)
rsub1=r[1:nrow(r),1:117,drop=F]
rsub1
d.sub1=as.data.frame(matrix(values(rsub1),nrow=round(ncol(r)/3), ncol=nrow(r)))
d.sub1=as.data.frame(t(as.matrix(d.sub1)))
d2=d.sub1
rownames(d2)=data$V1
d3=d2[which(rownames(d2) %in% sample),]
d4=gather(d3)
sample1=sample(d4$value, 150, replace=F)
grid.poly1=rasterToPolygons(rsub1)
grid.poly1=spTransform(grid.poly1,CRS("+init=epsg:2154"))
grid.poly1@data$ID=values(rsub1)
grid.poly1@data$layer=NULL
grid.sample1=crop(grid.sample1, gdEst)
grid.sample1=grid.poly1[grid.poly1@data$ID %in% sample1,]
rsubsample1=raster(grid.sample1)
res(rsubsample1)=100
subsample1=rasterToPolygons(rsubsample1)
subsample1=raster::intersect(subsample1, grid.sample1)

#subset2 (Vosges)
rsub2=r[1:nrow(r),(round(ncol(r)/3)+1):((round(ncol(r)/3)+1)+round(ncol(r)/3)),drop=F]
rsub2
d.sub2=as.data.frame(matrix(values(rsub2),nrow=118, ncol=358))
d.sub2=as.data.frame(t(as.matrix(d.sub2)))
d2=d.sub2
rownames(d2)=data$V1
d3=d2[which(rownames(d2) %in% sample),]
d4=gather(d3)
sample2=sample(d4$value, 150, replace=F)
grid.poly2=rasterToPolygons(rsub2)
grid.poly2=spTransform(grid.poly2,CRS("+init=epsg:2154"))
grid.poly2@data$ID=values(rsub2)
grid.poly2@data$layer=NULL
grid.sample2=crop(grid.sample2, gdEst)
grid.sample2=grid.poly2[grid.poly2@data$ID %in% sample2,]
rsubsample2=raster(grid.sample2)
res(rsubsample2)=100
subsample2=rasterToPolygons(rsubsample2)
subsample2=raster::intersect(subsample2, grid.sample2)

#subset3 (Alsace)
rsub3=r[1:nrow(r),(((round(ncol(r)/3)+1)+round(ncol(r)/3))+1):ncol(r),drop=F]
rsub3
d.sub3=as.data.frame(matrix(values(rsub3),nrow=117, ncol=358))
d.sub3=as.data.frame(t(as.matrix(d.sub3)))
d2=d.sub3
rownames(d2)=data$V1
d3=d2[which(rownames(d2) %in% sample),]
d4=gather(d3)
sample3=sample(d4$value, 150, replace=F)
grid.poly3=rasterToPolygons(rsub3)
grid.poly3=spTransform(grid.poly3,CRS("+init=epsg:2154"))
grid.poly3@data$ID=values(rsub3)
grid.poly3@data$layer=NULL
grid.sample3=crop(grid.sample3, gdEst)
grid.sample3=grid.poly3[grid.poly3@data$ID %in% sample3,]
rsubsample3=raster(grid.sample3)
res(rsubsample3)=100
subsample3=rasterToPolygons(rsubsample3)
subsample3=raster::intersect(subsample3, grid.sample3)

#on assemble les trois sous-ensembles pour obtenir l'échantillon
grid.sample=rbind(grid.sample1, grid.sample2, grid.sample3)
raster::shapefile(grid.sample, file="grille_echantillon.shp") #pour sauvegarder en couche shape

#on assemble les trois subsample pour avoir la grille globale des réplicats spatiaux, puis on sélectionne au hasard 5 mailles au sein des réplicats spatiaux
subsample=rbind(subsample1, subsample2, subsample3)
subsample$ID_plot=subsample@data$ID
subsample$ID_subplot=seq.int(nrow(subsample))
raster::shapefile(subsample,file="sous_grille_globale.shp")
spac.rep=ddply(as.data.frame(subsample),.(ID), function(x) x[sample(nrow(x),5),])
spac.rep=subsample[match(spac.rep$ID_subplot, subsample@data$ID_subplot, nomatch=0),]
spac.rep@data$ID_rep=ave(spac.rep@data$ID_subplot, spac.rep@data$ID_plot, FUN=seq_along)
spac.rep@data$layer=NULL
spac.rep@data$ID=NULL
spac.rep
raster::shapefile(spac.rep, file="replicats_spatiaux.shp")