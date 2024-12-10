# Proyecto Inundaciones Tena

# Autor: Jorge Hurtado - Pidal
# Fecha: 21 de marzo de 2023

# Fig. 8. Effect of climate change and LUCC on floods with kruskal-Wallis with Dunn test

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

# .................................................................................................

# Load data ----

data <- read.csv("./Results/datafig.csv"); head(data)


# preparin table for climate and land cover change

{
  dntr2.baaf <- data %>% select(elev,dntr2p_baaf) %>% mutate (group = "dntr2.baaf")
  names(dntr2.baaf) <- c("elev","dnpv","group")
  
  dntr10.baaf <- data %>% select(elev, dntr10p_baaf) %>% mutate (group = "dntr10.baaf")
  names(dntr10.baaf) <- c("elev","dnpv","group")
  
  dntr100.baaf <- data %>% select(elev, dntr100p_baaf) %>% mutate (group = "dntr100.baaf")
  names(dntr100.baaf) <- c("elev","dnpv","group")
  
  dn.baaf<- rbind(dntr2.baaf,dntr10.baaf,dntr100.baaf)
}

# .................................................................................................


# .................................................................................................

# Figure of Climate and Lucc Effect

#formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(f3 <-  ggplot(data=dn.baaf,mapping=aes(x=elev, y=dnpv, color=group, linetype=group)) + 
    
    geom_point(aes(shape=group), 
               show.legend=F,size = 2, alpha = 0.7) +
    
    stat_smooth(aes(fill = group, color = group), 
                method = "nls", formula = formula, se = F,show.legend = F)+
    
        scale_colour_manual(name = "Return Period",
                        breaks = c("dntr2.baaf","dntr10.baaf","dntr100.baaf"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Elevation (m.a.s.l)", y = "p-value (-)",
         title = "Dunn Test: Climate Change and LUCC", size=5) +
    
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


#   theme_bw())

# .................................................................................................



