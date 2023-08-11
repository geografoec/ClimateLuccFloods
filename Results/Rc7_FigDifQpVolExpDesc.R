
# Proyecto Inundaciones Tena
# Objetivo 2: Interaccion entre cambio climatico y de cobertura del suelo

# Autor: Jorge Hurtado - Pidal
# Fecha: 29 de marzo de 2023

# Figura N6 de la seccion resultados del articulo N3
# Diferencias absolutas de caudal punta (q) y volumen de escorrentia (v) con modelo exponencial y lineal

# ..................................................................................................

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

# .................................................................................................

# Peak flow differences by climate change (cc)

# Preparing table for plots

{
  q.cctr2 <- data %>% select(elev,q_cctr2) %>% mutate (group = "qcctr2")
  names(q.cctr2) <- c("elev","qdif","group")
  
  q.cctr10 <- data %>% select(elev,q_cctr10) %>% mutate (group = "qcctr10")
  names(q.cctr10) <- c("elev","qdif","group")
  
  q.cctr100 <- data %>% select(elev,q_cctr100) %>% mutate (group = "qcctr100")
  names(q.cctr100) <- c("elev","qdif","group")
  
  q.cc <- rbind(q.cctr2, q.cctr10, q.cctr100)
}

# Figure of peak flow differences by cc

formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(p1 <-  ggplot(data = q.cc, 
               mapping = aes(x = elev, y = qdif, group = group, color = group,linetype = group)) + 
    
        geom_point(aes(shape=group), 
                   show.legend=F, size=2, alpha=0.7) +
        
        geom_smooth(method = "nls",formula = formula, se = F, show.legend = F)+
        
        scale_colour_manual(name = "Return Period",
                            breaks = c("qcctr2","qcctr10","qcctr100"),
                            labels = c("Rp2","Rp10","Rp100"),
                            values = c("black","blue","red"))+
        
        labs(x = "Elevation (m.a.s.l)", 
             y = expression(paste("Peak flow difference (", m^3, "/s)")),
             title = "Climate Change", size=5) +
         
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

#        theme_bw())

# .................................................................................................

# Peak flow differences by lucc

# Preparing table for plots

{
  q.luctr2 <- data %>% select(elev,q_luctr2) %>% mutate (group = "qluctr2")
  names(q.luctr2) <- c("elev","qdif","group")
  
  q.luctr10 <- data %>% select(elev,q_luctr10) %>% mutate (group = "qluctr10")
  names(q.luctr10) <- c("elev","qdif","group")
  
  q.luctr100 <- data %>% select(elev,q_luctr100) %>% mutate (group = "qluctr100")
  names(q.luctr100) <- c("elev","qdif","group")
  
  q.luc <- rbind(q.luctr2, q.luctr10, q.luctr100)
}

# Figure of peak flow differences by lucc

#formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(p2 <-  ggplot(data = q.luc, 
               mapping = aes(x = elev, y = qdif, group = group, color = group,linetype = group)) + 
    
    geom_point(aes(shape=group), 
               show.legend=F, size=2, alpha=0.7) +
    
    geom_smooth(method = "nls",formula = formula, se = F, show.legend = F)+
    
    scale_colour_manual(name = "Return Period",
                        breaks = c("qluctr2","qluctr10","qluctr100"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Elevation (m.a.s.l)", 
         y = expression(paste("Peak flow difference (", m^3, "/s)")),
         title = "LUCC", size=5) +
    
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

#        theme_bw())


# .................................................................................................

# Peak flow differences by cc and lucc

# Preparing table for plots

{
  q.ccluctr2 <- data %>% select(elev,q_ccluctr2) %>% mutate (group = "qccluctr2")
  names(q.ccluctr2) <- c("elev","qdif","group")
  
  q.ccluctr10 <- data %>% select(elev,q_ccluctr10) %>% mutate (group = "qccluctr10")
  names(q.ccluctr10) <- c("elev","qdif","group")
  
  q.ccluctr100 <- data %>% select(elev,q_ccluctr100) %>% mutate (group = "qccluctr100")
  names(q.ccluctr100) <- c("elev","qdif","group")
  
  q.ccluc <- rbind(q.ccluctr2, q.ccluctr10, q.ccluctr100)
}

# Figure of peak flow differences by cc and lucc 

#formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(p3 <-  ggplot(data = q.ccluc, 
               mapping = aes(x = elev, y = qdif, group = group, color = group,linetype = group)) + 
    
    geom_point(aes(shape=group), 
               show.legend=F, size=2, alpha=0.7) +
    
    geom_smooth(method = "nls",formula = formula, se = F, show.legend = F)+
    
    scale_colour_manual(name = "Return Period",
                        breaks = c("qccluctr2","qccluctr10","qccluctr100"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Elevation (m.a.s.l)", 
         y = expression(paste("Peak flow difference (", m^3, "/s)")),
         title = "Climate Change and LUCC", size=5) +
    
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

#        theme_bw())


(f1 <- ggarrange(p1,p2,p3, 
                 labels = c("(a)", "(b)","(c)"),
                 ncol = 1, nrow = 3))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# .................................................................................................

vol <- data

# Volume differences by climate change (cc)

# Preparing table for plots

{
  v.cctr2 <- vol %>% select(elev,v_cctr2) %>% mutate (group = "vcctr2")
  names(v.cctr2) <- c("elev","vdif","group")
  
  v.cctr10 <- vol %>% select(elev,v_cctr10) %>% mutate (group = "vcctr10")
  names(v.cctr10) <- c("elev","vdif","group")
  
  v.cctr100 <- vol %>% select(elev,v_cctr100) %>% mutate (group = "vcctr100")
  names(v.cctr100) <- c("elev","vdif","group")
  
  v.cc <- rbind(v.cctr2, v.cctr10, v.cctr100)
}

# Figure of peak flow differences by cc

#formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(g1 <-  ggplot(data = v.cc, 
               mapping = aes(x = elev, y = vdif, group = group, color = group,linetype = group)) + 
    
    geom_point(aes(shape=group), 
               show.legend=F, size=2, alpha=0.7) +
    
    geom_smooth(method = "nls",formula = formula, se = F, show.legend = F)+
    
    scale_colour_manual(name = "Return Period",
                        breaks = c("vcctr2","vcctr10","vcctr100"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Elevation (m.a.s.l)", 
         y = expression(paste("Stormflow volume difference (", Hm^3, ")")),
         title = "Climate Change", size=5) +
    
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

#        theme_bw())

# .................................................................................................

# Volume differences by lucc

# Preparing table for plots

{
  v.luctr2 <- vol %>% select(elev,v_luctr2) %>% mutate (group = "vluctr2")
  names(v.luctr2) <- c("elev","vdif","group")
  
  v.luctr10 <- vol %>% select(elev,v_luctr10) %>% mutate (group = "vluctr10")
  names(v.luctr10) <- c("elev","vdif","group")
  
  v.luctr100 <- vol %>% select(elev,v_luctr100) %>% mutate (group = "vluctr100")
  names(v.luctr100) <- c("elev","vdif","group")
  
  v.luc <- rbind(v.luctr2, v.luctr10, v.luctr100)
}

# Figure of peak flow differences by lucc .......

#formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(g2 <-  ggplot(data = v.luc, 
               mapping = aes(x = elev, y = vdif, group = group, color = group,linetype = group)) + 
    
    geom_point(aes(shape=group), 
               show.legend=F, size=2, alpha=0.7) +
    
    geom_smooth(method = "nls",formula = formula, se = F, show.legend = F)+
    
    scale_colour_manual(name = "Return Period",
                        breaks = c("vluctr2","vluctr10","vluctr100"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Elevation (m.a.s.l)", 
         y = expression(paste("Stormflow volume difference (", Hm^3, ")")),
         title = "LUCC", size=5) +
    
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

#        theme_bw())

# .................................................................................................

# Volume differences by cc and lucc

# Preparing table for plots

{
  v.ccluctr2 <- vol %>% select(elev,v_ccluctr2) %>% mutate (group = "vccluctr2")
  names(v.ccluctr2) <- c("elev","vdif","group")
  
  v.ccluctr10 <- vol %>% select(elev,v_ccluctr10) %>% mutate (group = "vccluctr10")
  names(v.ccluctr10) <- c("elev","vdif","group")
  
  v.ccluctr100 <- vol %>% select(elev,v_ccluctr100) %>% mutate (group = "vccluctr100")
  names(v.ccluctr100) <- c("elev","vdif","group")
  
  v.ccluc <- rbind(v.ccluctr2, v.ccluctr10, v.ccluctr100)
}

# Figure of peak flow differences by lucc 

#formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(g3 <-  ggplot(data = v.ccluc, 
               mapping = aes(x = elev, y = vdif, group = group, color = group,linetype = group)) + 
    
    geom_point(aes(shape=group), 
               show.legend=F, size=2, alpha=0.7) +
    
    geom_smooth(method = "nls",formula = formula, se = F, show.legend = F)+
    
    scale_colour_manual(name = "Return Period",
                        breaks = c("vccluctr2","vccluctr10","vccluctr100"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Elevation (m.a.s.l)", 
         y = expression(paste("Stormflow volume difference (", Hm^3, ")")),
         title = "Climate Change and LUCC", size=5) +
    
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

#        theme_bw())

(f2 <- ggarrange(g1,g2,g3, 
                 labels = c("(d)", "(e)","(f)"),
                 ncol = 1, nrow = 3))

# six plots

(f3 <- ggarrange(p1,g1,p2,g2,p3,g3, 
                 labels = c("(a)", "(d)","(b)","(e)", "(c)","(f)"),
                 ncol = 2, nrow = 3))
