# ..................................................................................................

# Objetivo: Preparar una tabla para realizar las figuras de los resultados del articulo

# Autor: Jorge Hurtado-Pidal

# Fecha: 13 de marzo 2023

# ..................................................................................................

# Libraries

{
  library(tidyverse)
  library(stringr)  
  library(readxl)    # read excel files
  library(ggpubr)
  library(rstatix)   # kruska_test function
  library(FSA)       # dunn test, para post hoc kruskal wallis 
  library(rcompanion)# Scheirer-Ray-Hare test
}

# Preparing the environment

cat("\014")       #clean console   
rm(list = ls())   #clean environment
graphics.off()    #clean plots

# ..................................................................................................

# 1. Preparing Q series----

# load data

#setwd("C:/JorgeR/DoctTenaFloods/3lulcclimatefloods/MHydrologicalModeling/TetisOutputs/N5_SeriesOutput42pts")

list.series <- list()
Qid <- sprintf("Q%d", 1:42)
names <- list(c("Dt", Qid))

for(i in 1:12){
  list.series[[i]] <- read_excel("./Results/QSeriesTetis.xlsx", sheet = i) 
  list.series[[i]] <- dplyr::select(list.series[[i]],contains(c("Dt","Qs")))
  names(list.series[[i]]) <- names[[1]]
}

# tr2 scenarios ........

s1tr2 <- list()
s2tr2 <- list()
s3tr2 <- list()
s4tr2 <- list()

q.tr2 <- list()
varname <- data.frame(Qid)
sheet_names <- data.frame(excel_sheets("./Results/QSeriesTetis.xlsx")); sheet_names

for(i in 1:42){
  
  for(j in 1:42){
    
    s1tr2[[j]] <- data.frame(list.series[1]) %>% select(varname[i,]) %>% mutate(group=sheet_names[1,])
    s2tr2[[j]] <- data.frame(list.series[2]) %>% select(varname[i,]) %>% mutate(group=sheet_names[2,])
    s3tr2[[j]] <- data.frame(list.series[3]) %>% select(varname[i,]) %>% mutate(group=sheet_names[3,])
    s4tr2[[j]] <- data.frame(list.series[4]) %>% select(varname[i,]) %>% mutate(group=sheet_names[4,])
    
    names(s1tr2[[i]]) <- c("Caudal","Grupo")
    names(s2tr2[[i]]) <- c("Caudal","Grupo")
    names(s3tr2[[i]]) <- c("Caudal","Grupo")
    names(s4tr2[[i]]) <- c("Caudal","Grupo")
    
    q.tr2[[i]] <- (rbind(s1tr2[[i]],s2tr2[[i]],s3tr2[[i]],s4tr2[[i]]))
    
  }
}

#q.tr2[[42]]

# tr10 scenarios .......

s5tr10 <- list()
s6tr10 <- list()
s7tr10 <- list()
s8tr10 <- list()

q.tr10 <- list()
#varname <- data.frame(Qid)
#sheet_names <- data.frame(excel_sheets("./N5QSeriesTetisLuccClimateFloods.xlsx")); sheet_names

for(i in 1:42){
  
  for(j in 1:42){
    
    s5tr10[[j]] <- data.frame(list.series[5]) %>% select(varname[i,]) %>% mutate(group=sheet_names[5,])
    s6tr10[[j]] <- data.frame(list.series[6]) %>% select(varname[i,]) %>% mutate(group=sheet_names[6,])
    s7tr10[[j]] <- data.frame(list.series[7]) %>% select(varname[i,]) %>% mutate(group=sheet_names[7,])
    s8tr10[[j]] <- data.frame(list.series[8]) %>% select(varname[i,]) %>% mutate(group=sheet_names[8,])
    
    names(s5tr10[[i]]) <- c("Caudal","Grupo")
    names(s6tr10[[i]]) <- c("Caudal","Grupo")
    names(s7tr10[[i]]) <- c("Caudal","Grupo")
    names(s8tr10[[i]]) <- c("Caudal","Grupo")
    
    q.tr10[[i]] <- (rbind(s5tr10[[i]],s6tr10[[i]],s7tr10[[i]],s8tr10[[i]]))
    
  }
}

#q.tr10[[42]]

# tr100 scenarios ......

s9tr100 <- list()
s10tr100 <- list()
s11tr100 <- list()
s12tr100 <- list()

q.tr100 <- list()
#varname <- data.frame(Qid)
#sheet_names <- data.frame(excel_sheets("./N5QSeriesTetisLuccClimateFloods.xlsx")); sheet_names

for(i in 1:42){
  
  for(j in 1:42){
    
    s9tr100[[j]] <- data.frame(list.series[9]) %>% select(varname[i,]) %>% mutate(group=sheet_names[9,])
    s10tr100[[j]] <- data.frame(list.series[10]) %>% select(varname[i,]) %>% mutate(group=sheet_names[10,])
    s11tr100[[j]] <- data.frame(list.series[11]) %>% select(varname[i,]) %>% mutate(group=sheet_names[11,])
    s12tr100[[j]] <- data.frame(list.series[12]) %>% select(varname[i,]) %>% mutate(group=sheet_names[12,])
    
    names(s9tr100[[i]]) <- c("Caudal","Grupo")
    names(s10tr100[[i]]) <- c("Caudal","Grupo")
    names(s11tr100[[i]]) <- c("Caudal","Grupo")
    names(s12tr100[[i]]) <- c("Caudal","Grupo")
    
    q.tr100[[i]] <- (rbind(s9tr100[[i]],s10tr100[[i]],s11tr100[[i]],s12tr100[[i]]))
    
  }
}

#q.tr100[[42]]


#...................................................................................................

# 2. Kruskal-Wallis----- 
# Statistical difference among all scenarios

# tr2 ........

kwtr2 <- list()
kwtr2p <- list()

for(i in 1:42){
  kwtr2[[i]] <- q.tr2[[i]] %>% kruskal_test(Caudal ~ Grupo)
  kwtr2p[[i]] <- dplyr::select(kwtr2[[i]],contains(c("p")))
  
}

#kwtr2[[42]]

kwtr2p.df <- do.call(rbind.data.frame, kwtr2p)

# tr10 .......

kwtr10 <- list()
kwtr10p <- list()

for(i in 1:42){
  kwtr10[[i]] <- q.tr10[[i]] %>% kruskal_test(Caudal ~ Grupo)
  kwtr10p[[i]] <- dplyr::select(kwtr10[[i]],contains(c("p")))
  
}

#kwtr10[[42]]

kwtr10p.df <- do.call(rbind.data.frame, kwtr10p)

# tr100 ......

kwtr100 <- list()
kwtr100p <- list()

for(i in 1:42){
  kwtr100[[i]] <- q.tr100[[i]] %>% kruskal_test(Caudal ~ Grupo)
  kwtr100p[[i]] <- dplyr::select(kwtr100[[i]],contains(c("p")))
  
}

#kwtr100[[42]]

kwtr100p.df <- do.call(rbind.data.frame, kwtr100p)


# table with all kruskal-wallis test ......

kw.pv.df <- cbind(kwtr2p.df, kwtr10p.df, kwtr100p.df); kw.pv.df
names(kw.pv.df) <- c("kwtr2p","kwtr10p","kwtr100p")
kw.pv.df$QID <- seq(1,42);kw.pv.df


#..................................................................................................

# 3. Dunn test----
# Statistical difference among pairs for climate change, land cover change or both

# tr2 ........

dntr2 <- list()
dntr2p <- list()

for(i in 1:42){
  dntr2[[i]] <- dunnTest(Caudal ~ Grupo, data=q.tr2[[i]],method="bh")
  dntr2p[[i]] <- (dntr2[[i]]$res$P.adj)
  
}

#dntr2[[41]]

dntr2p.df <- do.call(rbind.data.frame, dntr2p)
dntr2p.df <- dntr2p.df[,c(1,2,4)]
names(dntr2p.df) <- c("dntr2p_babf","dntr2p_baaa","dntr2p_baaf")

# tr10 ........

dntr10 <- list()
dntr10p <- list()

for(i in 1:42){
  dntr10[[i]] <- dunnTest(Caudal ~ Grupo, data=q.tr10[[i]],method="bh")
  dntr10p[[i]] <- (dntr10[[i]]$res$P.adj)
  
}

#dntr10[[41]]

dntr10p.df <- do.call(rbind.data.frame, dntr10p)
dntr10p.df <- dntr10p.df[,c(1,2,4)]
names(dntr10p.df) <- c("dntr10p_babf","dntr10p_baaa","dntr10p_baaf")

# tr100 ........

dntr100 <- list()
dntr100p <- list()

for(i in 1:42){
  dntr100[[i]] <- dunnTest(Caudal ~ Grupo, data=q.tr100[[i]],method="bh")
  dntr100p[[i]] <- (dntr100[[i]]$res$P.adj)
  
}

#dntr100[[41]]

dntr100p.df <- do.call(rbind.data.frame, dntr100p)
dntr100p.df <- dntr100p.df[,c(4,5,6)]                                     # estas columnas son los pares BA-BF;BA-AA;BA-AF
names(dntr100p.df) <- c("dntr100p_babf","dntr100p_baaa","dntr100p_baaf")

# table with all Dunn test ......

dn.pv.df <- cbind(dntr2p.df, dntr10p.df, dntr100p.df); dn.pv.df

dn.pv.df$QID <- seq(1,42);dn.pv.df

#..................................................................................................

# 4. Scheirer-Ray-Hare test ----
# Equivalent to two-way ANOVA for non normal distributions

# H0: El clima no tiene efecto significativo sobre el caudal
# H0: La cobertura no tiene efecto significativo sobre el caudal
# H0: La interaccion entre cobertura y clima no tiene efecto significativo sobre caudal

# tr2 ........

# preparing data

for( i in 1:42){
 
  q.tr2[[i]] <- q.tr2[[i]] %>% mutate (clima = if_else(Grupo == "S1T2BA", 1,
                                               if_else(Grupo == "S2T2BF", 2,
                                               if_else(Grupo == "S3T2AA", 1,
                                               if_else(Grupo == "S4T2AF", 2,0)))))

  q.tr2[[i]] <- q.tr2[[i]] %>% mutate (cobertura = if_else(Grupo == "S1T2BA", 1,
                                                   if_else(Grupo == "S2T2BF", 1,
                                                   if_else(Grupo == "S3T2AA", 2,
                                                   if_else(Grupo == "S4T2AF", 2,0)))))
  }

#q.tr2[[1]]

# test

srhtr2 <- list()
srhtr2p <- list()

for(i in 1:42){
  
  srhtr2[[i]] <- scheirerRayHare(Caudal ~ clima + cobertura, data=q.tr2[[i]])
  srhtr2p[[i]] <- (srhtr2[[i]]$p.value)
}

(srhtr2[[41]])
(srhtr2p[[41]])

srhtr2p.df <- do.call(rbind.data.frame, srhtr2p)
srhtr2p.df <- srhtr2p.df[,-4] 
names(srhtr2p.df) <- c("srhtr2p_cl","srhtr2p_cob","srhtr2p_clcob")

# tr10 ........

# preparing data

for( i in 1:42){
  
  q.tr10[[i]] <- q.tr10[[i]] %>% mutate (clima = if_else(Grupo == "S5T10BA", 1,
                                                 if_else(Grupo == "S6T10BF", 2,
                                                 if_else(Grupo == "S7T10AA", 1,
                                                 if_else(Grupo == "S8T10AF", 2,0)))))
  
  q.tr10[[i]] <- q.tr10[[i]] %>% mutate (cobertura = if_else(Grupo == "S5T10BA", 1,
                                                     if_else(Grupo == "S6T10BF", 1,
                                                     if_else(Grupo == "S7T10AA", 2,
                                                     if_else(Grupo == "S8T10AF", 2,0)))))
  }

#q.tr10[[1]]

# test

srhtr10 <- list()
srhtr10p <- list()

for(i in 1:42){
  
  srhtr10[[i]] <- scheirerRayHare(Caudal ~ clima + cobertura, data=q.tr10[[i]])
  srhtr10p[[i]] <- (srhtr10[[i]]$p.value)
}

(srhtr10[[41]])
(srhtr10p[[41]])

srhtr10p.df <- do.call(rbind.data.frame, srhtr10p)
srhtr10p.df <- srhtr10p.df[,-4] 
names(srhtr10p.df) <- c("srhtr10p_cl","srhtr10p_cob","srhtr10p_clcob")

# tr100 ........

# preparing data

for( i in 1:42){
  
  q.tr100[[i]] <- q.tr100[[i]] %>% mutate (clima = if_else(Grupo == "S9T100BA", 1,
                                                   if_else(Grupo == "S10T100BF", 2,
                                                   if_else(Grupo == "S11T100AA", 1,
                                                   if_else(Grupo == "S12T100AF", 2,0)))))
  
  q.tr100[[i]] <- q.tr100[[i]] %>% mutate (cobertura = if_else(Grupo == "S9T100BA", 1,
                                                       if_else(Grupo == "S10T100BF", 1,
                                                       if_else(Grupo == "S11T100AA", 2,
                                                       if_else(Grupo == "S12T100AF", 2,0)))))
}

#q.tr100[[1]]

# test

srhtr100 <- list()
srhtr100p <- list()

for(i in 1:42){
  
  srhtr100[[i]] <- scheirerRayHare(Caudal ~ clima + cobertura, data=q.tr100[[i]])
  srhtr100p[[i]] <- (srhtr100[[i]]$p.value)
}

(srhtr100[[41]])
(srhtr100p[[41]])

srhtr100p.df <- do.call(rbind.data.frame, srhtr100p)
srhtr100p.df <- srhtr100p.df[,-4] 
names(srhtr100p.df) <- c("srhtr100p_cl","srhtr100p_cob","srhtr100p_clcob")

# table with all ScheirerRayHare test ......

srh.pv.df <- cbind(srhtr2p.df, srhtr10p.df, srhtr100p.df); srh.pv.df

srh.pv.df$QID <- seq(1,42);srh.pv.df

#..................................................................................................

# 5. Absolute differences of Qmax ----

# The contribution of climate and lucc will be analyzed with absolute differences of 
# maximum stormflow (m3/s; qmax) among scenarios, using Zhang et al (2018) equations 

# Preparing qmax series

# load data

list.qmax <- list()

for(i in 1:12){
  list.qmax[[i]] <- data.frame((list.series[[i]])) %>% 
  summarise_if(is.numeric, max)                           # q max en cada punto
}

# transpose. from horizontal to vertical

t.qmax <- list()

for(i in 1:12){
  t.qmax[[i]] <- data.frame(t(list.qmax[[i]]))
}

# table of qmax 

qmax <- do.call(cbind, t.qmax)
qmax <- qmax [-1,]

sheet_names1 <- excel_sheets("./Results/QSeriesTetis.xlsx"); sheet_names1
names(qmax) <- sheet_names1

qmax$QID <- str_sub(row.names(qmax), start = 2) # library "stringr"

# difference of qmax among escenarios. Zhang et al (2018) equations

dif.qmax <- data.frame(qmax)

dif.qmax <- dif.qmax %>% 
            mutate(q_cctr2 = ((1/2)*((S2T2BF-S1T2BA)+(S4T2AF-S3T2AA))), 
                  q_cctr10 = ((1/2)*((S6T10BF-S5T10BA)+(S8T10AF-S7T10AA))),
                 q_cctr100 = ((1/2)*((S10T100BF-S9T100BA)+(S12T100AF-S11T100AA))), 
                  q_luctr2 = ((1/2)*((S3T2AA-S1T2BA)+(S4T2AF-S2T2BF))),
                 q_luctr10 = ((1/2)*((S7T10AA-S5T10BA)+(S8T10AF-S6T10BF))),
                q_luctr100 = ((1/2)*((S11T100AA-S9T100BA)+(S12T100AF-S10T100BF))),
                q_ccluctr2 = (S4T2AF-S1T2BA),
               q_ccluctr10 = (S8T10AF-S5T10BA),
              q_ccluctr100 = (S12T100AF-S9T100BA),
                  q_diftr2 = ((S4T2AF-S1T2BA)-((S2T2BF-S1T2BA)+(S3T2AA-S1T2BA))),
                 q_diftr10 = ((S8T10AF-S5T10BA)-((S6T10BF-S5T10BA)+(S7T10AA-S5T10BA))),
                q_diftr100 = ((S12T100AF-S9T100BA)-((S10T100BF-S9T100BA)+(S11T100AA-S9T100BA))), 
)

dif.qmax <- dif.qmax[,13:25]

#..................................................................................................

# 6. Absolute differences of Vol ----

# The contribution of climate and lucc will be analyzed with absolute differences of 
# stormflow volume (Hm3; vol) among scenarios, using Zhang et al (2018) equations 

# Preparing vol series

# load data. 

list.vol <- list()
Vid <- sprintf("V%d", 1:42)
names2 <- list(c("Dt", Vid))

for(i in 1:12){
  list.vol[[i]] <- read_excel("./Results/VolSeriesTetis.xlsx", sheet = i) 
  list.vol[[i]] <- dplyr::select(list.vol[[i]],contains(c("Dt","Vs")))
  names(list.vol[[i]]) <- names2[[1]]
}

# transpose. from horizontal to vertical

t.vol <- list()

for(i in 1:12){
  t.vol[[i]] <- data.frame(t(list.vol[[i]]))
}

# table of volume

vol <- do.call(cbind, t.vol)
vol <- vol[-1,]

sheet_names2 <- excel_sheets("./Results/VolSeriesTetis.xlsx"); sheet_names2
names(vol) <- sheet_names2

vol$QID <- str_sub(row.names(vol), start = 2) # library "stringr"

# difference of volume among escenarios. Zhang et al (2018) equations

dif.vol <- data.frame(vol)

dif.vol <- dif.vol %>% 
           mutate(v_cctr2 = ((1/2)*((S2T2BF-S1T2BA)+(S4T2AF-S3T2AA))), 
                 v_cctr10 = ((1/2)*((S6T10BF-S5T10BA)+(S8T10AF-S7T10AA))),
                v_cctr100 = ((1/2)*((S10T100BF-S9T100BA)+(S12T100AF-S11T100AA))), 
                 v_luctr2 = ((1/2)*((S3T2AA-S1T2BA)+(S4T2AF-S2T2BF))),
                v_luctr10 = ((1/2)*((S7T10AA-S5T10BA)+(S8T10AF-S6T10BF))),
               v_luctr100 = ((1/2)*((S11T100AA-S9T100BA)+(S12T100AF-S10T100BF))),
               v_ccluctr2 = (S4T2AF-S1T2BA),
              v_ccluctr10 = (S8T10AF-S5T10BA),
             v_ccluctr100 = (S12T100AF-S9T100BA),
                 v_diftr2 = ((S4T2AF-S1T2BA)-((S2T2BF-S1T2BA)+(S3T2AA-S1T2BA))),
                v_diftr10 = ((S8T10AF-S5T10BA)-((S6T10BF-S5T10BA)+(S7T10AA-S5T10BA))),
               v_diftr100 = ((S12T100AF-S9T100BA)-((S10T100BF-S9T100BA)+(S11T100AA-S9T100BA))), 
)

dif.vol <- dif.vol[,13:25]

#..................................................................................................

# 7. Final table with all variables ----

# load points with phisical attributes of the basin

attrib <- read_excel("./Results/qpoints.xlsx", sheet = 1)

# join tables

datafig <- merge(attrib, 
           merge(dif.qmax, 
           merge(dif.vol, 
           merge(kw.pv.df,
           merge(dn.pv.df, srh.pv.df, 
           by="QID"), by="QID"), by="QID"), by="QID"), by="QID")
 
# save table

write.csv(datafig,"./Results/datafig.csv", row.names = FALSE)

# script end ......