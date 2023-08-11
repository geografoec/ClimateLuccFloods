# Codigo escrito por Jorge Hurtado Pidal, abril 2023

# Graficos de hietogramas de disenio. 
# Seccion de metodologia del articulo "climate change, lucc, floods"

# ........................................................................................................

rm(list = ls()) # liberar espacio de memoria
graphics.off() # limpiar plots

# cargando librerias
library(readxl)
library(tidyverse)
library(ggpubr)

# preparando entorno de trabajo

# cargando tormentas
tormentas <- read_excel("./Methods/ClimateScenarioData.xlsx", sheet = 2)
tormentas <- round(tormentas, digits = 2)
#tormentas <- tormentas %>% dplyr::filter(tormentas$tr1 > 0)
#tormentas <- tormentas[,2:4]
tormentas$dt <- seq(10,(length(tormentas$tr2a))*10,10)


# hietogramas

# M1219 (actual)

h1 <- barplot(tormentas$tr2a, width=1, space=NULL,names.arg = tormentas$dt, 
              col="grey",border="black", axis.lty=1, ylim = c(0,35),
              main= "Storm Rp2", xlab="Time (min)", ylab="Intensity (mm/10min)")

h2 <- barplot(tormentas$tr10a, width=1, space=NULL,names.arg = tormentas$dt, 
              col="grey",border="black", axis.lty=1, ylim = c(0,35),
              main= "Storm Rp10", xlab="Time (min)", ylab="Intensity (mm/10min)")

h3 <- barplot(tormentas$tr100a, width=1, space=NULL,names.arg = tormentas$dt, 
              col="grey",border="black", axis.lty=1, ylim = c(0,35),
              main= "Storm Rp100", xlab="Time (min)", ylab="Intensity (mm/10min)")

# IPSL (futuro)

h4 <- barplot(tormentas$tr2f, width=1, space=NULL,names.arg = tormentas$dt, 
              col="grey",border="black", axis.lty=1, ylim = c(0,40),
              main= "Storm Rp2", xlab="Time (min)", ylab="Intensity (mm/10min)")

h5 <- barplot(tormentas$tr10f, width=1, space=NULL,names.arg = tormentas$dt, 
              col="grey",border="black", axis.lty=1, ylim = c(0,40),
              main= "Storm Rp10", xlab="Time (min)", ylab="Intensity (mm/10min)")

h6 <- barplot(tormentas$tr100f, width=1, space=NULL,names.arg = tormentas$dt, 
              col="grey",border="black", axis.lty=1, ylim = c(0,40),
              main= "Storm Rp100", xlab="Time (min)", ylab="Intensity (mm/10min)")

# ggarrange(h1,h2,h3,h4,h5,h6, 
#           labels = c("(a)", "","","(b)","","",""),
#           ncol = 3, nrow = 2)

