# Proyecto Inundaciones Tena

# Autor: Jorge Hurtado - Pidal
# Fecha: 22 de marzo de 2023

# Fig. 6. and Fig. 7. Isolated effects with Scheirer-Ray-Hare test.

# .................................................................................................

# libraries

{
  library(ggplot2)
  library(ggpubr)
  library(tidyverse)
  library(ggtext)
  library(extrafont)
}

# Preparing the environment

cat("\014")       #clean conosle   
rm(list = ls())   #clean environment
graphics.off()    #clean plots
#loadfonts(device = "win", quiet = TRUE) 

# .................................................................................................

# Load data ----

data <- read.csv("./Results/datafig.csv"); head(data)

# preparing table for climate change

{
  srhtr2.cl <- data %>% select(elev,srhtr2p_cl) %>% mutate (group = "srhtr2.cl")
  names(srhtr2.cl) <- c("elev","srhpv","group")
  
  srhtr10.cl <- data %>% select(elev, srhtr10p_cl) %>% mutate (group = "srhtr10.cl")
  names(srhtr10.cl) <- c("elev","srhpv","group")
  
  srhtr100.cl <- data %>% select(elev, srhtr100p_cl) %>% mutate (group = "srhtr100.cl")
  names(srhtr100.cl) <- c("elev","srhpv","group")
  
  srh.cl<- rbind(srhtr2.cl,srhtr10.cl,srhtr100.cl)
}

# preparin table for land cover change

{
  srhtr2.cob <- data %>% select(elev,srhtr2p_cob) %>% mutate (group = "srhtr2.cob")
  names(srhtr2.cob) <- c("elev","srhpv","group")
  
  srhtr10.cob <- data %>% select(elev, srhtr10p_cob) %>% mutate (group = "srhtr10.cob")
  names(srhtr10.cob) <- c("elev","srhpv","group")
  
  srhtr100.cob <- data %>% select(elev, srhtr100p_cob) %>% mutate (group = "srhtr100.cob")
  names(srhtr100.cob) <- c("elev","srhpv","group")
  
  srh.cob<- rbind(srhtr2.cob,srhtr10.cob,srhtr100.cob)
}

# preparin table for climate and land cover change

{
  srhtr2.clcob <- data %>% select(elev,srhtr2p_clcob) %>% mutate (group = "srhtr2.clcob")
  names(srhtr2.clcob) <- c("elev","srhpv","group")
  
  srhtr10.clcob <- data %>% select(elev, srhtr10p_clcob) %>% mutate (group = "srhtr10.clcob")
  names(srhtr10.clcob) <- c("elev","srhpv","group")
  
  srhtr100.clcob <- data %>% select(elev, srhtr100p_clcob) %>% mutate (group = "srhtr100.clcob")
  names(srhtr100.clcob) <- c("elev","srhpv","group")
  
  srh.clcob<- rbind(srhtr2.clcob,srhtr10.clcob,srhtr100.clcob)
}

# .................................................................................................

# Figure of Climate Effect

# General equation

srh.cl1 <- srh.cl %>% filter(group=="srhtr2.cl")
srh.cl2 <- srh.cl %>% filter(group=="srhtr10.cl"| group=="srhtr100.cl")

formula1 <- (y ~ x)
formula2 <- (y ~ SSasymp(x, Asym, R0, lrc))

(f1 <-  ggplot(data=srh.cl1,mapping=aes(x=elev, y=srhpv, color=group, linetype=group)) + 
    
    geom_point(aes(shape=group,color = group), 
               show.legend= F,size=2, alpha=0.7) +
    
    stat_smooth(aes(fill = group, color = group), 
                method = "lm", formula = formula1, se = F, show.legend = F)+
    
    geom_point(data=srh.cl2,mapping=aes(x=elev, y=srhpv,shape=group,color = group),
               show.legend = F,size=2, alpha=0.7)+
    
    geom_smooth(data=srh.cl2,mapping=aes(x=elev, y=srhpv,color = group), 
                method = "nls", formula = formula2, se = F,show.legend = F)+
    
   
    scale_colour_manual(name = "Return Period",
                        breaks = c("srhtr2.cl","srhtr10.cl","srhtr100.cl"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Elevation (m.a.s.l)", y = "p-value (-)",
         title = "Scheirer-Ray-Hare Test: Climate Change", size=5) +
    
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

# Figure of Lucc effect

# dev.new()

formula <- (y ~ SSasymp(x, Asym, R0, lrc))

(f2 <-  ggplot(data=srh.cob,mapping=aes(x=elev, y=srhpv, color=group, linetype=group)) + 
    
    geom_point(aes(shape=group), 
               show.legend=F, size=2, alpha=0.7) +
    
    stat_smooth(aes(fill = group, color = group), 
                method = "nls", formula = formula, se = F,show.legend = F)+
    
        scale_colour_manual(name = "Return Period",
                        breaks = c("srhtr2.cob","srhtr10.cob","srhtr100.cob"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Elevation (m.a.s.l)", y = "p-value (-)",
         title = "Scheirer-Ray-Hare Test: LUCC", size=5) +
    
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

# Figure of climate and Lucc Effect

# General equation

srh.clcob1 <- srh.clcob %>% filter(group=="srhtr2.clcob")
srh.clcob2 <- srh.clcob %>% filter(group=="srhtr10.clcob"| group=="srhtr100.clcob")

# formula2 <- (y ~ SSasymp(x, Asym, R0, lrc)) # para tr10 y 100
# formula1 <- (y ~ x)                         # para tr2

(f3 <-  ggplot(data=srh.clcob1,mapping=aes(x=elev, y=srhpv, color=group, linetype=group)) + 
    
    geom_point(aes(shape = group), 
               show.legend = F, size = 2, alpha = 0.7) +
    
    stat_smooth(aes(fill = group, color = group), 
                method = "nls", formula = formula2, se = F, show.legend = F) +
    
    geom_point(data=srh.clcob2,mapping = aes(x = elev, y = srhpv, shape = group, color = group),
               show.legend = F, size = 2, alpha = 0.7) +
    
    geom_smooth(data=srh.clcob2,mapping=aes(x=elev, y=srhpv), 
                method = "lm", formula = formula1, se = FALSE, show.legend = F) +
    
    scale_colour_manual(name = "Return Period",
                        breaks = c("srhtr2.clcob","srhtr10.clcob","srhtr100.clcob"),
                        labels = c("Rp2","Rp10","Rp100"),
                        values = c("black","blue","red"))+
    
    labs(x = "Elevation (m.a.s.l)", y = "p-values (-)",
         title = "Scheirer-Ray-Hare Test: Climate Change and LUCC", size=5) +
    
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

# Figure with three plots


(f <- ggarrange(f1,f2, 
                 labels = c("(a)", "(b)"),
                 ncol = 1, nrow = 2))


# ggsave(plot=f3, filename = ("./Rc_outputs/rf2.png"), units = "in", 
#          width = 9, height = 12, dpi = 300)

