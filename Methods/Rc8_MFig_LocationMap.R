# MAPA DE UBICACION PARA ARTICULO

# Editado por: Jorge Hurtado

# Fecha: 17 de abril de 2023

# Elementos aincluir: 

# a) Ubicacion de la cuenca en Napo y en Ecuador
# b) Estaciones meteorologicas e hidrológicas
# c) Puntos de simulacion de caudal
# d) Red hidrica
# e) Modelo digital de elevaciones con hillshade

# ..................................................................................................

# Preparing the environment

cat("\014")       #clean conosle   
rm(list = ls())   #clean environment
graphics.off()    #clean plots

# libraries

{
#library(mapview)
library(rasterVis)
library(rayshader)
library(tidyverse)
library(tmap)
#library(rgeos)
#library(sp)
library(rgdal)
library(raster)
library(RColorBrewer)
library(sf)
library(latticeExtra)
library(tmaptools)
}

# load data

utm17sur <- "+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0" #utm zona 17 sur con datum WGS84

# mapa sudamerica

sudamer <-st_read("./Methods/shapes/SUDAMERICA_UTM17S.shp")

sudamer.3p <- subset(sudamer, (PAÍS == "Ecuador" |PAÍS == "Colombia" |PAÍS == "Perú"))


# Mapa Ecuador

ecu <-st_read("./Methods/shapes/ecuador_limite.shp")

# vamos a borrar galapagos del mapa

# Definir las cuatro coordenadas como pares de longitud y latitud
coordenadas <- matrix(c(-94, 3,  -94, -3,  -86, -3,  -86, 3, -94, 3), ncol = 2, byrow = TRUE)

# Crear un objeto de tipo Polygon a partir de las coordenadas

poligono_sf <- st_sf(st_sfc(st_polygon(list(coordenadas))))

# asignando las coordenadas primer

st_crs(poligono_sf) <- st_crs(4326)

# transformo a utm 17s

(poligono_utm <- st_transform(poligono_sf, utm17sur))

# resta con libreria rgeos

poligono_diferencia <- st_difference(ecu, poligono_utm)
ecu <- poligono_diferencia # este objeto tipo sf, es ecuador sin galapagos en utm17s 

plot(ecu$geometry)

# hasta aqui listo el limite de ecuador (sin galapagos)

# ahora vamos a cargar limite de napo (utm17s)

napo <-  st_read("./Methods/shapes/Napo.shp")

ctena <- st_read("./Methods/shapes/CuencaTena.shp") # basin boundaries

# mapa de ubicacion de la cuenca en ecuador

plot(sudamer.3p$geometry)
#plot(ecu, add=T)
plot(napo$geometry, col="gray", add=T)
plot(ctena$geometry, col="#4682B4",add=T)

# mapa de ubicacion de la cuenca en napo
graphics.off()   
plot(napo$geometry, col="gray")
plot(ctena, col="#4682B4",add=T)

# mapa de la cuenca (principal)

# preparacion de capas en wgs84-utm17s

# modelo de elevaciones SRTM 90m
dem <- raster("./Methods/raster/filldem/w001001.adf") # dem

hillshade <- raster("./Methods/raster/hillshade/w001001.adf")

# rios obtenidos con el dem en whitebox
riosdem <- st_read("./Methods/shapes/streams.shp")

# asignar sistema de coordenadas UTM 17 sur
st_crs(riosdem) # compruebo la proyeccion en este caso no tiene 
st_crs(riosdem) <- st_crs(utm17sur) # asigno la proyeccion

# estaciones meteorologicas

# primero tengo que pasar a utm17s las de inamhi que estan en grados decimales
 
stations1 <- tribble(
  ~Xutm, ~Yutm, ~tipo, ~codigo,~nombre,~opera,
  -77.81917, -0.91681, "METEO", "M1219","Tena_Hda_Chaupishungo","INAMHI"
  # -77.83907, -0.93472, "METEO","M0484","Archidona","INAMHI",
  # -77.8399,-0.87666,"METEO","M0488","Cotundo","INAMHI",
)

stations1_geo <- st_as_sf(stations1, coords = c("Xutm","Yutm"),
                          crs="+proj=longlat +datum=WGS84")

stations1_utm <- st_transform(stations1_geo, utm17sur)

# estacion meteo ikiam

stations2 <- tribble(
  ~Xutm, ~Yutm, ~tipo, ~codigo,~nombre,~opera,
  849394, 9894597, "METEO", "M5147","ikiam_universidad","IKIAM",
)

stations2_utm <- st_as_sf(stations2, coords = c("Xutm","Yutm"),
                          crs=utm17sur)

meteo_utm <- rbind(stations1_utm,stations2_utm)
#plot(meteo_utm$geometry)


#estacion hidrologica

stathid <- tribble(
  ~Xutm, ~Yutm, ~tipo, ~codigo,~nombre,~opera, 
   854592, 9890354, "hidro","H001","ikiam_tena","IKIAM",
) 

hidro_utm <- st_as_sf(stathid, coords = c("Xutm","Yutm"), 
                          crs=utm17sur)
st_crs(hidro_utm)

# puntos de simulacion de caudal

tetisim <- st_read("./Methods/shapes/qpoints_attr.shp")
#plot(tetisim$geometry)

# limite de la reserva biologica Colonso-Chalupas

rbcc <- st_read("./Methods/shapes/RBCC_limite.shp")
#plot(rbcc,add=T)

# vamos a cortar el poligono usando de limite el hillshade
r_ext <- st_as_sfc(st_bbox(hillshade)) # poligono con el limite del hillshade
plot(r_ext)

rbcc_cort <- st_intersection(rbcc, r_ext)
#plot(rbcc_cort$geometry)

# limite de la ciudad de tena

ciudad <- st_read("./Methods/shapes/LimiteCiudadTena.shp")


# plot con tmap

# para generar un area de ploteo

r_ext_b <- st_as_sfc(st_bbox(dem)) # poligono con el limite del dem
plot(r_ext_b)
plot(ctena$geometry, add=T)

rbcc_cort <- st_intersection(rbcc, r_ext)
plot(rbcc_cort$geometry)


# area de influencia de canales >=590m

r <- raster(dem)
r[] <- ifelse(dem[]<=590, 1,0) 
plot(r)

area_rios <- rasterToPolygons(r,dissolve = T) 
area_rios <- subset(area_rios, layer == 1)
plot(area_rios)

# tmap plot

  tm_shape(r_ext_b)+
  tm_borders(col = "gray", alpha = 0)+
  tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE, alpha = 0.4)+
  tm_shape(dem)+
  tm_raster(style = "cont", palette = "Spectral",legend.show = TRUE,alpha = 0.7,
            title = "m.a.s.l",legend.reverse = T)+
  
  tm_legend(position = c("RIGHT", "TOP"))+
  
  tm_layout(legend.title.size = 1.5, legend.text.size = 1,legend.outside=F) +
  
  tm_scale_bar(width=0.25,text.size = 1, position = c("center", "BOTTOM"))+
  
  tm_shape(ctena) +
  tm_borders(col = "black",lwd = 2)+
  tm_shape(area_rios) +
  tm_borders(col = "green3",lty=1,lwd = 2)+
  tm_shape(ciudad) +
  tm_borders(col = "black",lty=4,lwd = 1.5)+
    
  tm_shape(riosdem) +
  tm_lines(lwd = 2, col = "dodgerblue2")+
  tm_shape(rbcc_cort) +
  tm_borders(col = "red",lwd=2)+
  tm_shape(tetisim) +
  tm_dots(shape = 19, size=0.8,col = "black")+
  tm_shape(meteo_utm) +
  tm_dots(shape = 15, size=1.5,col = "orange")+
  tm_shape(hidro_utm) +
  tm_dots(shape = 17, size=1.5,col = "green") +
  
  tm_grid(ticks = T,lines = F ,labels.size = 1,projection = 4326)

  
# end..................
