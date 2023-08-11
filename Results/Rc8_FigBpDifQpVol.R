
# Proyecto Inundaciones Tena
# Objetivo 2: Interaccion entre cambio climatico y de cobertura del suelo

# Autor: Jorge Hurtado - Pidal
# Fecha: 31 de marzo de 2023

# Figura N7 de la seccion resultados del articulo N3
# Box Plots de las diferencias absolutas de caudal punta (q) y volumen de escorrentia (v)

# ..................................................................................................

# libraries

{
  library(ggplot2)
  library(ggpubr)
  library(tidyverse)
  library(ggtext)
  library(extrafont)
  #library(ggdist)
  #library(ggbeeswarm)
  library(forcats)
}

# Preparing the environment

cat("\014")       #clean conosle   
rm(list = ls())   #clean environment
graphics.off()    #clean plots
#loadfonts(device = "win", quiet = TRUE) 

# .................................................................................................

# Load data ----

data <- read.csv("./Results/datafig.csv"); head(data)

# .................................................................................................

# Box plots for peak flow

# tr2 ----

{
  q_cctr2 <- data.frame(data$q_cctr2) %>% mutate(group="CC")
  names(q_cctr2) <- c("qdif","group")
  
  q_luctr2 <- data.frame(data$q_luctr2) %>% mutate(group="LUCC")
  names(q_luctr2) <- c("qdif","group")
  
  q_ccluctr2 <- data.frame(data$q_ccluctr2) %>% mutate(group="CC:LUCC")
  names(q_ccluctr2) <- c("qdif","group")
  
  q_datatr2 <- rbind(q_cctr2, q_luctr2, q_ccluctr2)
}

# definir el orden deseado de los niveles del factor "class"
orden_deseado_tr2 <- c("CC", "LUCC", "CC:LUCC")

# aplicar a la variable group el orden deseado
q_datatr2$group <- fct_relevel(q_datatr2$group, .order = orden_deseado_tr2)

# boxplot

(q1 <- ggplot(q_datatr2,aes(group, qdif,fill=group, color=group))+
       geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
       coord_cartesian(ylim = c(0, 1000))+
       stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
       stat_summary(fun = mean, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
       labs(title = "", x = "", 
           y = expression(paste("Peak flow difference (", m^3, "/s)")), show.legend = F)+
      
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

# tr10 ----

{
  q_cctr10 <- data.frame(data$q_cctr10) %>% mutate(group="CC")
  names(q_cctr10) <- c("qdif","group")
  
  q_luctr10 <- data.frame(data$q_luctr10) %>% mutate(group="LUCC")
  names(q_luctr10) <- c("qdif","group")
  
  q_ccluctr10 <- data.frame(data$q_ccluctr10) %>% mutate(group="CC:LUCC")
  names(q_ccluctr10) <- c("qdif","group")
  
  q_datatr10 <- rbind(q_cctr10, q_luctr10, q_ccluctr10)
}

# definir el orden deseado de los niveles del factor "class"
orden_deseado_tr10 <- c("CC", "LUCC", "CC:LUCC")

# aplicar a la variable group el orden deseado
q_datatr10$group <- fct_relevel(q_datatr10$group, .order = orden_deseado_tr10)

# boxplot

(q2 <- ggplot(q_datatr10,aes(group, qdif,fill=group, color=group))+
       geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
       coord_cartesian(ylim = c(0, 1000))+
       stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
       stat_summary(fun = mean, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
       labs(title = "", x = "", 
            y = expression(paste("Peak flow difference (", m^3, "/s)")), show.legend = F)+
       
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


# tr100 ----

{
  q_cctr100 <- data.frame(data$q_cctr100) %>% mutate(group="CC")
  names(q_cctr100) <- c("qdif","group")
  
  q_luctr100 <- data.frame(data$q_luctr100) %>% mutate(group="LUCC")
  names(q_luctr100) <- c("qdif","group")
  
  q_ccluctr100 <- data.frame(data$q_ccluctr100) %>% mutate(group="CC:LUCC")
  names(q_ccluctr100) <- c("qdif","group")
  
  q_datatr100 <- rbind(q_cctr100, q_luctr100, q_ccluctr100)
}

# definir el orden deseado de los niveles del factor "class"
orden_deseado_tr100 <- c("CC", "LUCC", "CC:LUCC")

# aplicar a la variable group el orden deseado
q_datatr100$group <- fct_relevel(q_datatr100$group, .order = orden_deseado_tr100)

# boxplot

(q3 <- ggplot(q_datatr100,aes(group, qdif,fill=group, color=group))+
    geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
    coord_cartesian(ylim = c(0, 1000))+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
    stat_summary(fun = mean, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
    labs(title = "", x = "", 
         y = expression(paste("Peak flow difference (", m^3, "/s)")), show.legend = F)+
    
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

# Box plots for volume

# tr2 ----

{
  v_cctr2 <- data.frame(data$v_cctr2) %>% mutate(group="CC")
  names(v_cctr2) <- c("vdif","group")
  
  v_luctr2 <- data.frame(data$v_luctr2) %>% mutate(group="LUCC")
  names(v_luctr2) <- c("vdif","group")
  
  v_ccluctr2 <- data.frame(data$v_ccluctr2) %>% mutate(group="CC:LUCC")
  names(v_ccluctr2) <- c("vdif","group")
  
  v_datatr2 <- rbind(v_cctr2, v_luctr2, v_ccluctr2)
}

# definir el orden deseado de los niveles del factor "class"
#orden_deseado_tr2 <- c("cc.tr2", "luc.tr2", "ccluc.tr2")

# aplicar a la variable group el orden deseado
v_datatr2$group <- fct_relevel(v_datatr2$group, .order = orden_deseado_tr2)

# boxplot

(v1 <- ggplot(v_datatr2,aes(group, vdif,fill=group, color=group))+
    geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
    coord_cartesian(ylim = c(0, 5))+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones est&ar, mult 1 indican 1 desviacion estandar
    stat_summary(fun = mean, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
    labs(title = "", x = "", 
         y = expression(paste("Stormflow difference ( ", Hm^3, ")")), show.legend = F)+
    
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

# tr10 ----

{
  v_cctr10 <- data.frame(data$v_cctr10) %>% mutate(group="CC")
  names(v_cctr10) <- c("vdif","group")
  
  v_luctr10 <- data.frame(data$v_luctr10) %>% mutate(group="LUCC")
  names(v_luctr10) <- c("vdif","group")
  
  v_ccluctr10 <- data.frame(data$v_ccluctr10) %>% mutate(group="CC:LUCC")
  names(v_ccluctr10) <- c("vdif","group")
  
  v_datatr10 <- rbind(v_cctr10, v_luctr10, v_ccluctr10)
}

# definir el orden deseado de los niveles del factor "class"
#orden_deseado_tr10 <- c("cc.tr10", "luc.tr10", "ccluc.tr10")

# aplicar a la variable group el orden deseado
v_datatr10$group <- fct_relevel(v_datatr10$group, .order = orden_deseado_tr10)

# boxplot

(v2 <- ggplot(v_datatr10,aes(group, vdif,fill=group, color=group))+
    geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
    coord_cartesian(ylim = c(0, 5))+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
    stat_summary(fun = mean, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
    labs(title = "", x = "", 
         y = expression(paste("Stormflow difference ( ", Hm^3, ")")), show.legend = F)+
    
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


# tr100 ----

{
  v_cctr100 <- data.frame(data$v_cctr100) %>% mutate(group="CC")
  names(v_cctr100) <- c("vdif","group")
  
  v_luctr100 <- data.frame(data$v_luctr100) %>% mutate(group="LUCC")
  names(v_luctr100) <- c("vdif","group")
  
  v_ccluctr100 <- data.frame(data$v_ccluctr100) %>% mutate(group="CC:LUCC")
  names(v_ccluctr100) <- c("vdif","group")
  
  v_datatr100 <- rbind(v_cctr100, v_luctr100, v_ccluctr100)
}

# definir el orden deseado de los niveles del factor "class"
#orden_deseado_tr100 <- c("cc.tr100", "luc.tr100", "ccluc.tr100")

# aplicar a la variable group el orden deseado
v_datatr100$group <- fct_relevel(v_datatr100$group, .order = orden_deseado_tr100)

# boxplot

(v3 <- ggplot(v_datatr100,aes(group, vdif,fill=group, color=group))+
    geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
    coord_cartesian(ylim = c(0, 5))+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
    stat_summary(fun = mean, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
    labs(title = "", x = "", 
         y = expression(paste("Stormflow difference ( ", Hm^3, ")")), show.legend = F)+
    
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


# seis boxplots

ggarrange(q1,q2,q3,v1,v2,v3, 
          labels = c("(a)", "", "", "(b)","",""),
          ncol = 3, nrow = 2)
