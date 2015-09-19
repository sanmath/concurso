library(ddplyr) #Librería para manipular datos
library(plyr)
library(ggplot2) #Librería para gráficos
library(lattice)
library(rgdal) #Para usar shapefiles shp
library(rgeos) #Para hacer operaciones geométricas
library(spatstat) #Para hacer gráficos de Voronoi
library(sp)#Para usar coordenadas
library(maptools)# para construir mapas
library(maps)#para construir mapas
library(RColorBrewer)#Para construir paletas de color
library(grDevices)#para construir dispositivos de gráficos para paletas
library(reshape2)
library(rCharts)
library(knitr)
library(base64enc)
library(googleVis)
ecuador_shp<-readOGR(dsn="ECU_adm1.shp",layer="ECU_adm1")


ec<-readOGR(dsn=".", layer="ECU_adm1")
View(ec)
#al fin maldita sea........ ya pude fusionar las dos bases ahora si voy a poder los nombres ::::))))))))
ec = readOGR(dsn=".", layer="ECU_adm1")
names(ec)
ec@data$id = rownames(ec@data)
ec.points = fortify(ec, region="id")
ec.df = join(ec.points, ec@data, by="id")

ec.points = fortify(ec.df, region="id")
View(ec.points)
View(ec.df)
cnames <- aggregate(cbind(long, lat) ~ NAME_1, data=ec.df, 
                    FUN=function(x)mean(range(x)))
colourCount = length(unique(ec.df$NAME_1))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))


ecu<-ggplot(ec.df) + 
  aes(long,lat,group=group,fill=NAME_1) + 
  geom_polygon() +
  geom_path(color="blue") +
  coord_equal() +
  scale_fill_manual(values = getPalette(colourCount))+
  geom_text(data=cnames, aes(long, lat,group=NAME_1, label = NAME_1), size=2)+
  coord_map()
ecu
