# Proyecto Inundaciones Tena
# Objetivo 2: Interaccion entre cambio climatico y de cobertura del suelo

# Autor: Jorge Hurtado - Pidal
# Fecha: 04 de abril de 2023

# Tabla N2 de la seccion resultados del articulo N3
# resumen de p-values

# .................................................................................................

# libraries

{
  #library(ggplot2)
  #library(ggpubr)
  #library(tidyverse)
  #library(ggtext)
}

# Preparing the environment

cat("\014")       #clean conosle   
rm(list = ls())   #clean environment
graphics.off()    #clean plots

# Load data

data <- read.csv("./Results/datafig.csv"); head(data)

n <- 42

# kruskal-wallis
p.kwtr2p <- ((sum(data$kwtr2p < 0.05)/n)); p.kwtr2p
p.kwtr10p <- ((sum(data$kwtr10p < 0.05)/n)); p.kwtr10p
p.kwtr100p <- ((sum(data$kwtr100p < 0.05)/n)); p.kwtr100p

# dunn test
p.dncltr2p <- ((sum(data$dntr2p_babf < 0.05)/n)); p.dncltr2p
p.dncltr10p <- ((sum(data$dntr10p_babf < 0.05)/n)); p.dncltr10p
p.dncltr100p <- ((sum(data$dntr100p_babf < 0.05)/n)); p.dncltr100p

p.dnluctr2p <- ((sum(data$dntr2p_baaa < 0.05)/n)); p.dnluctr2p
p.dnluctr10p <- ((sum(data$dntr10p_baaa < 0.05)/n)); p.dnluctr10p
p.dnluctr100p <- ((sum(data$dntr100p_baaa < 0.05)/n)); p.dnluctr100p

p.dnccluctr2p <- ((sum(data$dntr2p_baaf < 0.05)/n)); p.dnccluctr2p
p.dnccluctr10p <- ((sum(data$dntr10p_baaf < 0.05)/n)); p.dnccluctr10p
p.dnccluctr100p <- ((sum(data$dntr100p_baaf < 0.05)/n)); p.dnccluctr100p

# sheirer-ray-hare test

p.srhcltr2p <- ((sum(data$srhtr2p_cl < 0.05)/n)); p.srhcltr2p
p.srhcltr10p <- ((sum(data$srhtr10p_cl < 0.05)/n)); p.srhcltr10p
p.srhcltr100p <- ((sum(data$srhtr100p_cl < 0.05)/n)); p.srhcltr100p

p.srhluctr2p <- ((sum(data$srhtr2p_cob < 0.05)/n)); p.srhluctr2p
p.srhluctr10p <- ((sum(data$srhtr10p_cob < 0.05)/n)); p.srhluctr10p
p.srhluctr100p <- ((sum(data$srhtr100p_cob < 0.05)/n)); p.srhluctr100p

p.srhccluctr2p <- ((sum(data$srhtr2p_clcob < 0.05)/n)); p.srhccluctr2p
p.srhccluctr10p <- ((sum(data$srhtr10p_clcob < 0.05)/n)); p.srhccluctr10p
p.srhccluctr100p <- ((sum(data$srhtr100p_clcob < 0.05)/n)); p.srhccluctr100p
