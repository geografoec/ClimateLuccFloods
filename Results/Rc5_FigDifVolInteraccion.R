
# Proyecto Inundaciones Tena
# Objetivo 2: Interaccion entre cambio climatico y de cobertura del suelo

# Autor: Jorge Hurtado - Pidal
# Fecha: 23 de marzo de 2023

# Figura N4 de la seccion resultados del articulo N3
# Interaccion con diferencias absolutas de volumen de escorrentia (v) modelo exponencial y lineal

# .................................................................................................

# libraries

{
  library(ggplot2)
  library(ggpubr)
  library(tidyverse)
  #library(ggtext)
  #library(extrafont)
}

# Preparing the environment

cat("\014")       #clean conosle   
rm(list = ls())   #clean environment
graphics.off()    #clean plots
#loadfonts(device = "win", quiet = TRUE) 

# .................................................................................................

# Load data ----

data <- read.csv("./Results/datafig.csv"); head(data)

# Preparing table for plot

# Transformation from Hm3 to m3

# vol <- data %>% mutate(v_diftr2 = (v_diftr2*1e6),
#                        v_diftr10 = (v_diftr10*1e6),
#                        v_diftr100 = (v_diftr100*1e6),)

vol <- data

# Table setting for ggplot 

{
  v.diftr2 <- vol %>% select(elev,nceldas,strahler,v_diftr2) %>% mutate (group = "vdiftr2")
  names(v.diftr2) <- c("elev","nceldas","strahler","vdif","group")
  
  v.diftr10 <- vol %>% select(elev,nceldas,strahler,v_diftr10) %>% mutate (group = "vdiftr10")
  names(v.diftr10) <- c("elev","nceldas","strahler","vdif","group")
  
  v.diftr100 <- vol %>% select(elev,nceldas,strahler,v_diftr100) %>% mutate (group = "vdiftr100")
  names(v.diftr100) <- c("elev","nceldas","strahler","vdif","group")
  
  v.dif <- rbind(v.diftr2, v.diftr10, v.diftr100)
}

# Filtering data to fit the models

#v.dif1 <- v.dif %>% filter(vdif<2500); head(v.dif1) # se comprobo que con valores <2500 m3 se ajustaron todos los modelos exponenciales

v.dif1 <- v.dif %>% filter(vdif<(2500/1000000)); head(v.dif1) 

# .................................................................................................

# plot modelo exponencial con elevacion

# General equation

formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(p1 <-  ggplot(data=v.dif1,
               mapping=aes(x=elev, y=vdif,group=group,color=group,linetype=group)) + 
    
        geom_point(aes(shape=factor(strahler)), 
                   show.legend=F, size=2, alpha=0.7) +
    
        geom_smooth(data=v.dif1, mapping=aes(x=elev, y=vdif), 
                method = "nls",formula = formula, se = F,show.legend = F)+
 
        scale_colour_manual(name = "Return Period",
                            breaks = c("vdiftr2","vdiftr10","vdiftr100"),
                            labels = c("Rp2","Rp10","Rp100"),
                            values = c("black","blue","red"))+
    
        labs(x = "Elevation (m.a.s.l)", 
             y = expression(paste("Interaction ( ",Hm^3,")")),
             title = "", size=5) +
    
        theme(plot.title=element_text(hjust=0.5, size=14),
              axis.title.x = element_text(size = 14),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              panel.background = element_rect(fill = 'white'),
              panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
              axis.line.x = element_line(size = 0.5, color = "black"),
              axis.line.y = element_line(size = 0.5, color = "black"),
              legend.key.size = unit(0.9, 'cm'),
              legend.text = element_text(size=16),
              legend.title = element_text(size=14),
              legend.key = element_rect(fill = "transparent")))

   #theme_bw())



# .................................................................................................

# Plot with nceldas and lineal model

# General equation

formula1 <- (y ~ x)

(p2 <-  ggplot(data=v.dif1,
               mapping=aes(x=nceldas, y=vdif,group=group,color=group,linetype=group)) + 
    
    geom_point(aes(shape=factor(strahler)), 
               show.legend=F, size=2, alpha=0.7) +
    
    geom_smooth(data=v.dif1, mapping=aes(x=nceldas, y=vdif), 
                method = "lm",formula = formula1, se = F,show.legend = F)+
    
    scale_colour_manual(name = "Return Period",
                        breaks = c("vdiftr2","vdiftr10","vdiftr100"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Flow accumulation (n cells)", 
         y = expression(paste("Interaction ( ",Hm^3,")")),
         title = "", size=5) +
    
    theme(plot.title=element_text(hjust=0.5, size=14),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          axis.line.x = element_line(size = 0.5, color = "black"),
          axis.line.y = element_line(size = 0.5, color = "black"),
          legend.key.size = unit(0.9, 'cm'),
          legend.text = element_text(size=16),
          legend.title = element_text(size=14),
          legend.key = element_rect(fill = "transparent")))

#theme_bw())

# Figure with two plots


(p3 <- ggarrange(p1,p2, 
                 labels = c("(a)", "(b)"),
                 ncol = 1, nrow = 2))

