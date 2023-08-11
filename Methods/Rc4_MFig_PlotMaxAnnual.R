# El objetivo de este script es hacer un ajuste de distribucion de frecuencias 
# usando la serie de precipitaci?n maxima en 24h del modelo IPLS SSP585, per?odo 2041-2070.
# A esta serie se le corrigi? el bias usando la estacion M1219 rellena con missForest 

# Editado por: Jorge Hurtado
# Fecha: 9 de septiembre de 2022

# Pasos a seguir:
# a. Preparar las series
# b. Hacer al ajuste a varias distribuciones teoricas (weibull, gamma, lognormal, normal)  
# c. Evaluar el ajuste para seleccionar el mejor modelo

#..................................................................................................

# Preparing the environment

rm(list = ls())
graphics.off()

# libraries

{
#library(fitdistrplus)
#library(MASS)
#library(survival)
#library(actuar)
#library(distrMod)
#library(dplyr)
library(readxl)
library(tidyverse)
library(ggpubr)
}

# load data

data <- read_excel("./Methods/BitacoraClimateScenarioV2.xlsx",sheet=1); head(data)

# preparacion de la tabla

data1 <- data.frame(data) 
data1$nyear <- seq(1,30,1)
names(data1) <- c("obs","IPSL","MIROC","NorESM2","year")


{

obs <- data1 %>% dplyr::select(year,obs) %>% mutate (group = "Observed")
    names(obs) <- c("year","PP","Group")
    
ipsl <- data1 %>% dplyr::select(year,IPSL) %>% mutate (group = "IPSL")
    names(ipsl) <- c("year","PP","Group")

miroc <- data1 %>% dplyr::select(year,MIROC) %>% mutate (group = "MIROC")
    names(miroc) <- c("year","PP","Group")

noresm <- data1 %>% dplyr::select(year,NorESM2) %>% mutate (group = "NorESM2")
    names(noresm) <- c("year","PP","Group")

data2 <- rbind(obs,ipsl,miroc,noresm)

}

    
# plot maximos anuales todas las series

(f1 <-  ggplot(data=data2, mapping=aes(x=year, y=PP, color=Group,linetype=Group)) + 

        geom_line (show.legend=F,size = 1, alpha = 0.7) +
        
        scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 50))+
        
        geom_point(aes(shape=Group),
                       show.legend=F,size = 2, alpha = 0.7) +
        
        scale_colour_manual(#name = "Simbology",
                            breaks = c("Observed","IPSL","MIROC", "NorESM2"),
                            #labels = c("Observed","GCM-BC:IPSL","GCM-BC:MIROC", "GCM-BC:NorESM2"),
                            values = c("black","blue","red", "green"))+
        
        labs(x = "", y = "", 
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
              legend.title = element_text(size=17),
              legend.key = element_rect(fill = "transparent")))



# plot de box plots

(f2 <- ggplot(data=data2, aes(Group, PP,fill=Group, color=Group))+
        geom_boxplot(width=0.5,alpha=0.7,show.legend = F)+
        coord_cartesian(ylim = c(0, 250))+
        stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2,show.legend = F) +     # mean_sld grafica las barras de las desviaciones estandar, mult 1 indican 1 desviacion estandar
        stat_summary(fun = mean, geom = "point", shape = 1,size = 3, color = "black",show.legend = F) +
        labs(title = "", x = "", y = "", show.legend = F)+
        
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

ggarrange(f1,f2, 
          labels = c("", ""),
          ncol = 2, nrow = 1)

# histogram of frequencies and empirical cumulative distribution


# End of script....................................................................................
