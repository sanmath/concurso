#################################
#####   Concurso - Mintel   #####
#################################

library(dplyr) #Librería para manipular datos
library(ggplot2) #Librería para gráficos
library(lattice)
library(rgdal) #Para usar shapefiles shp
library(rgeos) #Para hacer operaciones geométricas
library(spatstat) #Para hacer gráficos de Voronoi
library(sp)#Para usar coordenadas
library(maptools)# para construir mapas
library(maps)#para construir mapas
library(RColorBrewer)#Para construir paletas de color
library(reshape2)
library(knitr)
library(base64enc)
library(readxl)
library(data.table)
library(gdata)
library(gridExtra)

dir <- "/Users/usuario4/Dropbox/Source Stat Lab/Concurso/"
setwd(dir)

# Lectura nodos principales
setwd("./nodos_principales")
nodos_principales <- readOGR(dsn=".", layer="nodos_principales")
slotNames(nodos_principales)
nodos_principales@data

# Lectura nodos secundarios
setwd(dir)
setwd("./nodos_secundarios")
nodos_secundarios <- readOGR(dsn=".", layer="nodos_secundarios")

# Lectura de enlaces
setwd(dir)
enlaces <- fread("enlaces.csv")


#####################################################################################
setwd("C:/Users/Usuario/Documents/GitHub/Lecciones-de-R/MAPAS/concurso/Reto MINTEL Campus Party")
#Leemos los nodos principales y secundarios por separado
Nodos_principales<-read_excel("Informacion_ Nodos_ Principales y Secundarios_RTF.xlsx",sheet = 1)
Nodos_secundarios<-read_excel("Informacion_ Nodos_ Principales y Secundarios_RTF.xlsx",sheet = 2)
#Leemos los enlaces
Enlaces<-fread("Informacion_Enlaces_RAF.csv")
names(Nodos_secundarios)<-names(Nodos_principales)

nodo_enlace_pri<-inner_join(Nodos_principales,Enlaces,by="CODIGO")
nodo_enlace_sec<-inner_join(Nodos_secundarios,Enlaces,by="CODIGO")


##########################Informacion hospitales##########33
setwd("C:/Users/Usuario/Documents/GitHub/Lecciones-de-R/MAPAS/concurso/Reto MINTEL Campus Party/concurso_codigos/03 Información Hospitales")
hospitales<-read_excel("SISTEMA DE SALUD ESTATAL.xlsx")

###############Poblacion por sectores################3
poblacion<-read_excel("Poblacion por Sectores.xlsx")



link_by_code<-data.frame(table(enlaces$CODIGO))
names(link_by_code)<-c("CODIGO","Nro_enlaces")
nodos_principales@data<-inner_join(nodos_principales@data,link_by_code,by="CODIGO")
View(nodos_principales@data)
setnames(nodos_secundarios@data,names(nodos_secundarios@data),c("CODIGO","PROVINCIA","CANTON","latDD","longDD"))
nodos_secundarios@data<-inner_join(nodos_secundarios@data,link_by_code,by="CODIGO")

########################PARA MAPAS DE PROVINCIAS
setwd("C:/Users/Usuario/Documents/GitHub/Lecciones-de-R/MAPAS/concurso/Reto MINTEL Campus Party/concurso_codigos/mapas_Ecuador")
map_provincias<-readOGR(dsn=".",layer="ECU_adm1")
plot(map_provincias)




map_provincias@data$id = rownames(map_provincias@data)
map_provincias.points = fortify(map_provincias, region="id")
map_provincias.df = join(map_provincias.points, map_provincias@data, by="id")
prov_names <- aggregate(cbind(long, lat) ~ NAME_1, data=map_provincias.df, 
                        FUN=function(x)mean(range(x)))




rgb(red, green, blue, alpha, names = NULL, maxColorValue = 1)
color1<-rgb(runif(5),runif(5),runif(5),alpha=10,maxColorValue = 20) 
colourCount = length(unique(map_provincias.df$NAME_1))
getPalette = colorRampPalette(color1)
dt_nro_enlaces<-data.table(nodos_principales@data)[,.(sum(Nro_enlaces)),by=PROVINCIA]
mapa_prov<-function(n1,N2){
  ggplot(map_provincias.df[map_provincias.df$NAME_1==n1,]) + 
    aes(long,lat,group=group,fill=NAME_1) + 
    geom_polygon() +
    geom_path(color="black") +
    coord_equal() +
    scale_fill_manual(values = "lightblue")+
    geom_text(data=prov_names[prov_names$NAME_1==n1,], aes(long, lat,group=NAME_1, label = NAME_1), size=3)+
    coord_map()+
    theme(legend.position="none")+
    geom_point(aes(DDLong, DDLat,fill = NULL,group = NULL), size = 4,data=nodos_principales@data[nodos_principales@data$PROVINCIA==N2,],col="black")+
    geom_point(aes(longDD, latDD,fill = NULL,group = NULL), size = 1.5,data=nodos_secundarios@data[nodos_secundarios@data$PROVINCIA==N2,],col="red" )+
    ggtitle(N2)
}



mapa_prov("Guayas","GUAYAS")

dob("Imbabura","IMBABURA")
dob<-function(n1,N2){
  setnames(dt_nro_enlaces,names(dt_nro_enlaces),c("Provincia", "# de enlaces"))
  # Set theme to allow for plotmath expressions
  tt <- theme.default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(dt_nro_enlaces[Provincia==N2,], rows=NULL, theme=tt)
  # Plot chart and table into one object
  grid.arrange( tbl,mapa_prov(n1,N2),
                nrow=2,
                as.table=TRUE,
                heights=c(3,1))
}

dt_nro_enlaces<-data.table(nodos_principales@data)[,.(sum(Nro_enlaces)),by=PROVINCIA]
dt_nro_enlaces[PROVINCIA==N2,]
View(dt_nro_enlaces)








########################PARA MAPAS DE CIUDADES
setwd("C:/Users/Usuario/Documents/GitHub/Lecciones-de-R/MAPAS/concurso/Reto MINTEL Campus Party/concurso_codigos/mapas_Ecuador")
map_ciudades<-readOGR(dsn=".",layer="ECU_adm2")
plot(map_ciudades)




map_ciudades@data$id = rownames(map_ciudades@data)
map_ciudades.points = fortify(map_ciudades, region="id")
map_ciudades.df = join(map_ciudades.points, map_ciudades@data, by="id")
city_names <- aggregate(cbind(long, lat) ~ NAME_2, data=map_ciudades.df, 
                        FUN=function(x)mean(range(x)))


######FUNCION para graficar mapa de cada provincia... hace falta sacar lista de mapas q se pueden graficar por la escritura
mapa_ciudades<-function(n1,N2){
  ggplot(map_ciudades.df[map_ciudades.df$NAME_2==n1,]) + 
    aes(long,lat,group=group,fill=NAME_2) + 
    geom_polygon() +
    geom_path(color="black") +
    coord_equal() +
    scale_fill_manual(values = "lightblue")+
    geom_text(data=city_names[city_names$NAME_2==n1,], aes(long, lat,group=NAME_2, label = NAME_2), size=2.3)+
    coord_map()+
    theme(legend.position="none")+
    geom_point(aes(DDLong, DDLat,fill = NULL,group = NULL), size = 2,data=nodos_principales@data[nodos_principales@data$ciudad==N2,],col="black")+
    geom_point(aes(longDD, latDD,fill = NULL,group = NULL), size = 1.5,data=nodos_secundarios@data[nodos_secundarios@data$CANTON==N2,],col="red")  +
    ggtitle(N2)
}







mapa_ciudades("TulcÃ¡n","TULCAN")

