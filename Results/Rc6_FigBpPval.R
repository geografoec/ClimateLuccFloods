
# Proyecto Inundaciones Tena
# Objetivo 2: Interaccion entre cambio climatico y de cobertura del suelo

# Autor: Jorge Hurtado - Pidal
# Fecha: 28 de marzo de 2023

# Figura N5 de la seccion resultados del articulo N3
# BoxPlots de los p-values de los test de Dunn y Scheirer-Ray-Hare

# .................................................................................................

# libraries

{
  library(ggplot2)
  library(ggpubr)
  library(tidyverse)
  library(ggtext)
  library(extrafont)
  library(ggdist)
  library(ggbeeswarm)
  library(forcats)
}

# Preparing the environment

cat("\014")       #clean conosle   
rm(list = ls())   #clean environment
graphics.off()    #clean plots
loadfonts(device = "win", quiet = TRUE) 

# .................................................................................................

# Load data ----

data <- read.csv("./Results/datafig.csv"); head(data)

# .................................................................................................

# Box plots for Dunn test

# tr2 ----

{
  d_cctr2 <- data.frame(data$dntr2p_babf) %>% mutate(group="CC")
  names(d_cctr2) <- c("p_value","group")
  
  d_luctr2 <- data.frame(data$dntr2p_baaa) %>% mutate(group="LUCC")
  names(d_luctr2) <- c("p_value","group")
  
  d_ccluctr2 <- data.frame(data$dntr2p_baaf) %>% mutate(group="CC:LUCC")
  names(d_ccluctr2) <- c("p_value","group")
  
  d_datatr2 <- rbind(d_cctr2,d_luctr2, d_ccluctr2)
}

# definir el orden deseado de los niveles del factor "class"
orden_deseado_tr2 <- c("CC", "LUCC", "CC:LUCC")

# aplicar a la variable group el orden deseado
d_datatr2$group <- fct_relevel(d_datatr2$group, .order = orden_deseado_tr2)

# statistics for plot
#means <- aggregate(p_value ~ group, d_datatr2, mean)

# boxplot

(d1 <-  ggplot(d_datatr2,aes(group, p_value,fill=group, color=group))+
        geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
        coord_cartesian(ylim = c(0, 1))+
        stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
        stat_summary(fun = median, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
        labs(title = "", x = "", 
             y = "Dunn test p-value", show.legend = F)+
        
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

# tr10 ----

{
  d_cctr10 <- data.frame(data$dntr10p_babf) %>% mutate(group="CC")
  names(d_cctr10) <- c("p_value","group")
  
  d_luctr10 <- data.frame(data$dntr10p_baaa) %>% mutate(group="LUCC")
  names(d_luctr10) <- c("p_value","group")
  
  d_ccluctr10 <- data.frame(data$dntr10p_baaf) %>% mutate(group="CC:LUCC")
  names(d_ccluctr10) <- c("p_value","group")
  
  d_datatr10 <- rbind(d_cctr10,d_luctr10, d_ccluctr10)
}


# definir el orden deseado de los niveles del factor "class"
orden_deseado_tr10 <- c("CC", "LUCC", "CC:LUCC")

# aplicar a la variable group el orden deseado
d_datatr10$group <- fct_relevel(d_datatr10$group, .order = orden_deseado_tr10)

# statistics for plot
#means <- aggregate(p_value ~ group, d_datatr10, mean)

# boxplot

(d2 <-  ggplot(d_datatr10,aes(group, p_value,fill=group, color=group))+
    geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
    coord_cartesian(ylim = c(0, 1))+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
    stat_summary(fun = median, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
    labs(title = "", x = "", 
         y = "Dunn test p-value", show.legend = F)+
    
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


# tr100 ----
  
{
    d_cctr100 <- data.frame(data$dntr100p_babf) %>% mutate(group="CC")
    names(d_cctr100) <- c("p_value","group")
    
    d_luctr100 <- data.frame(data$dntr100p_baaa) %>% mutate(group="LUCC")
    names(d_luctr100) <- c("p_value","group")
    
    d_ccluctr100 <- data.frame(data$dntr100p_baaf) %>% mutate(group="CC:LUCC")
    names(d_ccluctr100) <- c("p_value","group")
    
    d_datatr100 <- rbind(d_cctr100,d_luctr100, d_ccluctr100)
}

# definir el orden deseado de los niveles del factor "class"
orden_deseado_tr100 <- c("CC", "LUCC", "CC:LUCC")

# aplicar a la variable group el orden deseado
d_datatr100$group <- fct_relevel(d_datatr100$group, .order = orden_deseado_tr100)

# statistics for plot
#means <- aggregate(p_value ~ group, d_datatr100, mean)
  
# boxplot
  
(d3 <-  ggplot(d_datatr100,aes(group, p_value,fill=group, color=group))+
    geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
    coord_cartesian(ylim = c(0, 1))+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
    stat_summary(fun = median, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
    labs(title = "", x = "", 
         y = "Dunn test p-value", show.legend = F)+
    
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


# tres boxplots de Dunn

ggarrange(d1,d2,d3 , 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

# .................................................................................................

# Box plots for Sheirer test

# tr2 ----

{
  s_cctr2 <- data.frame(data$srhtr2p_cl) %>% mutate(group="CC")
  names(s_cctr2) <- c("p_value","group")
  
  s_luctr2 <- data.frame(data$srhtr2p_cob) %>% mutate(group="LUCC")
  names(s_luctr2) <- c("p_value","group")
  
  s_ccluctr2 <- data.frame(data$srhtr2p_clcob) %>% mutate(group="CC:LUCC")
  names(s_ccluctr2) <- c("p_value","group")
  
  s_datatr2 <- rbind(s_cctr2,s_luctr2, s_ccluctr2)
}

# aplicar a la variable group el orden deseado
s_datatr2$group <- fct_relevel(s_datatr2$group, .order = orden_deseado_tr2)

# statistics for plot
#means <- aggregate(p_value ~ group, s_datatr2, mean)

# boxplot

(s1 <-  ggplot(s_datatr2,aes(group, p_value,fill=group, color=group))+
    geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
    coord_cartesian(ylim = c(0, 1))+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
    stat_summary(fun = median, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
    labs(title = "", x = "", 
         y = "Scheirer-Ray-Hare test p-value", show.legend = F)+
    
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


# tr10 ----

{
  s_cctr10 <- data.frame(data$srhtr10p_cl) %>% mutate(group="CC")
  names(s_cctr10) <- c("p_value","group")
  
  s_luctr10 <- data.frame(data$srhtr10p_cob) %>% mutate(group="LUCC")
  names(s_luctr10) <- c("p_value","group")
  
  s_ccluctr10 <- data.frame(data$srhtr10p_clcob) %>% mutate(group="CC:LUCC")
  names(s_ccluctr10) <- c("p_value","group")
  
  s_datatr10 <- rbind(s_cctr10,s_luctr10, s_ccluctr10)
}

# aplicar a la variable group el orden deseado
s_datatr10$group <- fct_relevel(s_datatr10$group, .order = orden_deseado_tr10)

# statistics for plot
#means <- aggregate(p_value ~ group, s_datatr10, mean)

# boxplot

(s2 <-  ggplot(s_datatr10,aes(group, p_value,fill=group, color=group))+
    geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
    coord_cartesian(ylim = c(0, 1))+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
    stat_summary(fun = median, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
    labs(title = "", x = "", 
         y = "Scheirer-Ray-Hare test p-value", show.legend = F)+
    
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

# tr100 ----

{
  s_cctr100 <- data.frame(data$srhtr100p_cl) %>% mutate(group="CC")
  names(s_cctr100) <- c("p_value","group")
  
  s_luctr100 <- data.frame(data$srhtr100p_cob) %>% mutate(group="LUCC")
  names(s_luctr100) <- c("p_value","group")
  
  s_ccluctr100 <- data.frame(data$srhtr100p_clcob) %>% mutate(group="CC:LUCC")
  names(s_ccluctr100) <- c("p_value","group")
  
  s_datatr100 <- rbind(s_cctr100,s_luctr100, s_ccluctr100)
}

# aplicar a la variable group el orden deseado
s_datatr100$group <- fct_relevel(s_datatr100$group, .order = orden_deseado_tr100)

# statistics for plot
#means <- aggregate(p_value ~ group, s_datatr100, mean)

# boxplot

(s3 <-  ggplot(s_datatr100,aes(group, p_value,fill=group, color=group))+
    geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
    coord_cartesian(ylim = c(0, 1))+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
    stat_summary(fun = median, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
    labs(title = "", x = "", 
         y = "Scheirer-Ray-Hare test p-value", show.legend = F)+
    
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

# tres boxplots de Sheirer

ggarrange(s1,s2,s3 , 
          labels = c("D", "E", "F"),
          ncol = 3, nrow = 1)


# seis boxplots

ggarrange(d1,d2,d3,s1,s2,s3, 
          labels = c("(a)", "", "", "(b)","",""),
          ncol = 3, nrow = 2)


# trash code ----

