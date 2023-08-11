# Proyecto Inundaciones Tena
# Objetivo 2: Interaccion entre cambio climatico y de cobertura del suelo

# Autor: Jorge Hurtado - Pidal
# Fecha: 20 de marzo de 2023

# Figura N1 de la seccion resultados del articulo N3
# Diferencia entre todos los escenarios con el test de kruskal-Wallis

# .................................................................................................

# libraries

{
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggtext)
}

# Preparing the environment

cat("\014")       #clean conosle   
rm(list = ls())   #clean environment
graphics.off()    #clean plots

# Load data

data <- read.csv("./Results/datafig.csv"); head(data)

{
kwtr2 <- data %>% select(elev,kwtr2p) %>% mutate (group = "kwtr2")
names(kwtr2) <- c("elev","kwpv","group")

kwtr10 <- data %>% select(elev, kwtr10p) %>% mutate (group = "kwtr10")
names(kwtr10) <- c("elev","kwpv","group")

kwtr100 <- data %>% select(elev, kwtr100p) %>% mutate (group = "kwtr100")
names(kwtr100) <- c("elev","kwpv","group")

kwdata <- rbind(kwtr2,kwtr10,kwtr100)
} 

# .................................................................................................

# grafico ----

formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(f1 <-  ggplot(data=kwdata,mapping=aes(x=elev, y=kwpv, color=group, linetype=group)) + 
   
        geom_point(aes(shape=group), 
                   show.legend=F,size = 2, alpha = 0.7) +
         
        stat_smooth(aes(fill = group, color = group), 
                    method = "nls", formula = formula, se = F,show.legend=F)+
         
        scale_colour_manual(name = "Return Period",
                             breaks = c("kwtr2","kwtr10","kwtr100"),
                             labels = c("Rp2","Rp10","Rp100"),
                             values = c("black","blue","red"))+
      
        labs(x = "Elevation (m.a.s.l)", y = "p-value (-)", 
             title = "Kruskal-Wallis Test", size=5) +
    
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
              legend.title = element_text(size=17),
              legend.key = element_rect(fill = "transparent")))
            
        #theme_bw())
        
        
# trash code ..................................