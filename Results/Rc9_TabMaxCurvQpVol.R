
# Proyecto Inundaciones Tena
# Objetivo 2: Interaccion entre cambio climatico y de cobertura del suelo

# Autor: Jorge Hurtado - Pidal
# Fecha: 03 de abril de 2023

# Tabla1 de la seccion resultados del articulo N3
# valores de curvatura maxima en las diferencias absolutas de Qp y Volesc

# ..................................................................................................

# libraries

{
  library(ggplot2)
  library(ggpubr)
  library(tidyverse)
  #library(ggtext)
  #library(extrafont)
  library(soilphysics)
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

# Preparing table

{
  q.cctr2 <- data %>% select(elev,q_cctr2) %>% mutate (group = "qcctr2")
  names(q.cctr2) <- c("elev","qdif","group")
  
  q.cctr10 <- data %>% select(elev,q_cctr10) %>% mutate (group = "qcctr10")
  names(q.cctr10) <- c("elev","qdif","group")
  
  q.cctr100 <- data %>% select(elev,q_cctr100) %>% mutate (group = "qcctr100")
  names(q.cctr100) <- c("elev","qdif","group")
  
  q.cc <- rbind(q.cctr2, q.cctr10, q.cctr100)
}

# Tr2 ...........................................

#plot
ggplot(q.cctr2, aes(x = elev, y = qdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
q1.x.tr2 <- q.cctr2$elev; q1.x.tr2
q1.y.tr2 <- q.cctr2$qdif; q1.y.tr2

q1.fit.tr2 <- nls(q1.y.tr2 ~ SSasymp(q1.x.tr2, Asym, R0, lrc))

q1.a.tr2 = coef(q1.fit.tr2)[["R0"]]; q1.a.tr2 #a
q1.s.tr2 = exp(coef(q1.fit.tr2)[["lrc"]]); q1.s.tr2 #s

# change detection point
q1.f.tr2 <- function(x) q1.a.tr2 * exp(-q1.s.tr2 * x)
(q1.mc.tr2 = (maxcurv(x.range = c(550, 700), fun = q1.f.tr2, method="pd")))
(q1x.mc.tr2 = q1.mc.tr2$x0)
(q1y.mc.tr2 = q1.mc.tr2$y0)

# Tr10 ..........................................

#plot
ggplot(q.cctr10, aes(x = elev, y = qdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
q1.x.tr10 <- q.cctr10$elev; q1.x.tr10
q1.y.tr10 <- q.cctr10$qdif; q1.y.tr10

q1.fit.tr10 <- nls(q1.y.tr10 ~ SSasymp(q1.x.tr10, Asym, R0, lrc))

q1.a.tr10 = coef(q1.fit.tr10)[["R0"]]; q1.a.tr10 #a
q1.s.tr10 = exp(coef(q1.fit.tr10)[["lrc"]]); q1.s.tr10 #s

# change detection point
q1.f.tr10 <- function(x) q1.a.tr10 * exp(-q1.s.tr10 * x)
(q1.mc.tr10 = (maxcurv(x.range = c(550, 700), fun = q1.f.tr10, method="pd")))
(q1x.mc.tr10 = q1.mc.tr10$x0)
(q1y.mc.tr10 = q1.mc.tr10$y0)

# tr100 .........................................

#plot
ggplot(q.cctr100, aes(x = elev, y = qdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
q1.x.tr100 <- q.cctr100$elev; q1.x.tr100
q1.y.tr100 <- q.cctr100$qdif; q1.y.tr100

q1.fit.tr100 <- nls(q1.y.tr100 ~ SSasymp(q1.x.tr100, Asym, R0, lrc))

q1.a.tr100 = coef(q1.fit.tr100)[["R0"]]; q1.a.tr100 #a
q1.s.tr100 = exp(coef(q1.fit.tr100)[["lrc"]]); q1.s.tr100 #s

# change detection point
q1.f.tr100 <- function(x) q1.a.tr100 * exp(-q1.s.tr100 * x)
(q1.mc.tr100 = (maxcurv(x.range = c(550, 700), fun = q1.f.tr100, method="pd")))
(q1x.mc.tr100 = q1.mc.tr100$x0)
(q1y.mc.tr100 = q1.mc.tr100$y0)
# .................................................................................................

# Peak flow differences by lucc

# Preparing table

{
  q.luctr2 <- data %>% select(elev,q_luctr2) %>% mutate (group = "qcctr2")
  names(q.luctr2) <- c("elev","qdif","group")
  
  q.luctr10 <- data %>% select(elev,q_luctr10) %>% mutate (group = "qcctr10")
  names(q.luctr10) <- c("elev","qdif","group")
  
  q.luctr100 <- data %>% select(elev,q_luctr100) %>% mutate (group = "qcctr100")
  names(q.luctr100) <- c("elev","qdif","group")
  
  q.luc <- rbind(q.luctr2, q.luctr10, q.luctr100)
}

# Tr2 .............

#plot
ggplot(q.luctr2, aes(x = elev, y = qdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
q2.x.tr2 <- q.luctr2$elev; q2.x.tr2
q2.y.tr2 <- q.luctr2$qdif; q2.y.tr2

q2.fit.tr2 <- nls(q2.y.tr2 ~ SSasymp(q2.x.tr2, Asym, R0, lrc))

q2.a.tr2 = coef(q2.fit.tr2)[["R0"]]; q2.a.tr2 #a
q2.s.tr2 = exp(coef(q2.fit.tr2)[["lrc"]]); q2.s.tr2 #s

# change detection point
q2.f.tr2 <- function(x) q2.a.tr2 * exp(-q2.s.tr2 * x)
(q2.mc.tr2 = (maxcurv(x.range = c(550, 700), fun = q2.f.tr2, method="pd")))
(q2x.mc.tr2 = q2.mc.tr2$x0)
(q2y.mc.tr2 = q2.mc.tr2$y0)


# Tr10 .............

#plot
ggplot(q.luctr10, aes(x = elev, y = qdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
q2.x.tr10 <- q.luctr10$elev; q2.x.tr10
q2.y.tr10 <- q.luctr10$qdif; q2.y.tr10

q2.fit.tr10 <- nls(q2.y.tr10 ~ SSasymp(q2.x.tr10, Asym, R0, lrc))

q2.a.tr10 = coef(q2.fit.tr10)[["R0"]]; q2.a.tr10 #a
q2.s.tr10 = exp(coef(q2.fit.tr10)[["lrc"]]); q2.s.tr10 #s

# change detection point
q2.f.tr10 <- function(x) q2.a.tr10 * exp(-q2.s.tr10 * x)
(q2.mc.tr10 = (maxcurv(x.range = c(550, 700), fun = q2.f.tr10, method="pd")))
(q2x.mc.tr10 = q2.mc.tr10$x0)
(q2y.mc.tr10 = q2.mc.tr10$y0)


# tr100 .............

#plot
ggplot(q.luctr100, aes(x = elev, y = qdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
q2.x.tr100 <- q.luctr100$elev; q2.x.tr100
q2.y.tr100 <- q.luctr100$qdif; q2.y.tr100

q2.fit.tr100 <- nls(q2.y.tr100 ~ SSasymp(q2.x.tr100, Asym, R0, lrc))

q2.a.tr100 = coef(q2.fit.tr100)[["R0"]]; q2.a.tr100 #a
q2.s.tr100 = exp(coef(q2.fit.tr100)[["lrc"]]); q2.s.tr100 #s

# change detection point
q2.f.tr100 <- function(x) q2.a.tr100 * exp(-q2.s.tr100 * x)
(q2.mc.tr100 = (maxcurv(x.range = c(550, 700), fun = q2.f.tr100, method="pd")))
(q2x.mc.tr100 = q2.mc.tr100$x0)
(q2y.mc.tr100 = q2.mc.tr100$y0)


# .................................................................................................

# Peak flow differences by cc and lucc

# Preparing table for plots

{
  q.ccluctr2 <- data %>% select(elev,q_ccluctr2) %>% mutate (group = "qcctr2")
  names(q.ccluctr2) <- c("elev","qdif","group")
  
  q.ccluctr10 <- data %>% select(elev,q_ccluctr10) %>% mutate (group = "qcctr10")
  names(q.ccluctr10) <- c("elev","qdif","group")
  
  q.ccluctr100 <- data %>% select(elev,q_ccluctr100) %>% mutate (group = "qcctr100")
  names(q.ccluctr100) <- c("elev","qdif","group")
  
  q.ccluc <- rbind(q.ccluctr2, q.ccluctr10, q.ccluctr100)
}

# Tr2 .............

#plot
ggplot(q.ccluctr2, aes(x = elev, y = qdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
q3.x.tr2 <- q.ccluctr2$elev; q3.x.tr2
q3.y.tr2 <- q.ccluctr2$qdif; q3.y.tr2

q3.fit.tr2 <- nls(q3.y.tr2 ~ SSasymp(q3.x.tr2, Asym, R0, lrc))

q3.a.tr2 = coef(q3.fit.tr2)[["R0"]]; q3.a.tr2 #a
q3.s.tr2 = exp(coef(q3.fit.tr2)[["lrc"]]); q3.s.tr2 #s

# change detection point
q3.f.tr2 <- function(x) q3.a.tr2 * exp(-q3.s.tr2 * x)
(q3.mc.tr2 = (maxcurv(x.range = c(550, 700), fun = q3.f.tr2, method="pd")))
(q3x.mc.tr2 = q3.mc.tr2$x0)
(q3y.mc.tr2 = q3.mc.tr2$y0)


# Tr10 .............

#plot
ggplot(q.ccluctr10, aes(x = elev, y = qdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
q3.x.tr10 <- q.ccluctr10$elev; q3.x.tr10
q3.y.tr10 <- q.ccluctr10$qdif; q3.y.tr10

q3.fit.tr10 <- nls(q3.y.tr10 ~ SSasymp(q3.x.tr10, Asym, R0, lrc))

q3.a.tr10 = coef(q3.fit.tr10)[["R0"]]; q3.a.tr10 #a
q3.s.tr10 = exp(coef(q3.fit.tr10)[["lrc"]]); q3.s.tr10 #s

# change detection point
q3.f.tr10 <- function(x) q3.a.tr10 * exp(-q3.s.tr10 * x)
(q3.mc.tr10 = (maxcurv(x.range = c(550, 700), fun = q3.f.tr10, method="pd")))
(q3x.mc.tr10 = q3.mc.tr10$x0)
(q3y.mc.tr10 = q3.mc.tr10$y0)


# tr100 .............

#plot
ggplot(q.ccluctr100, aes(x = elev, y = qdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
q3.x.tr100 <- q.ccluctr100$elev; q3.x.tr100
q3.y.tr100 <- q.ccluctr100$qdif; q3.y.tr100

q3.fit.tr100 <- nls(q3.y.tr100 ~ SSasymp(q3.x.tr100, Asym, R0, lrc))

q3.a.tr100 = coef(q3.fit.tr100)[["R0"]]; q3.a.tr100 #a
q3.s.tr100 = exp(coef(q3.fit.tr100)[["lrc"]]); q3.s.tr100 #s

# change detection point
q3.f.tr100 <- function(x) q3.a.tr100 * exp(-q3.s.tr100 * x)
(q3.mc.tr100 = (maxcurv(x.range = c(550, 700), fun = q3.f.tr100, method="pd")))
(q3x.mc.tr100 = q3.mc.tr100$x0)
(q3y.mc.tr100 = q3.mc.tr100$y0)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# .................................................................................................

# Volume differences by climate change (cc)

# Preparing table

{
  v.cctr2 <- data %>% select(elev,v_cctr2) %>% mutate (group = "vcctr2")
  names(v.cctr2) <- c("elev","vdif","group")
  
  v.cctr10 <- data %>% select(elev,v_cctr10) %>% mutate (group = "vcctr10")
  names(v.cctr10) <- c("elev","vdif","group")
  
  v.cctr100 <- data %>% select(elev,v_cctr100) %>% mutate (group = "vcctr100")
  names(v.cctr100) <- c("elev","vdif","group")
  
  v.cc <- rbind(v.cctr2, v.cctr10, v.cctr100)
}

# Tr2 ...........................................

#plot
ggplot(v.cctr2, aes(x = elev, y = vdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
v1.x.tr2 <- v.cctr2$elev; v1.x.tr2
v1.y.tr2 <- v.cctr2$vdif; v1.y.tr2

v1.fit.tr2 <- nls(v1.y.tr2 ~ SSasymp(v1.x.tr2, Asym, R0, lrc))

v1.a.tr2 = coef(v1.fit.tr2)[["R0"]]; v1.a.tr2 #a
v1.s.tr2 = exp(coef(v1.fit.tr2)[["lrc"]]); v1.s.tr2 #s

# change detection point
v1.f.tr2 <- function(x) v1.a.tr2 * exp(-v1.s.tr2 * x)
(v1.mc.tr2 = (maxcurv(x.range = c(550, 700), fun = v1.f.tr2, method="pd")))
(v1x.mc.tr2 = v1.mc.tr2$x0)
(v1y.mc.tr2 = v1.mc.tr2$y0)

# Tr10 ..........................................

#plot
ggplot(v.cctr10, aes(x = elev, y = vdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
v1.x.tr10 <- v.cctr10$elev; v1.x.tr10
v1.y.tr10 <- v.cctr10$vdif; v1.y.tr10

v1.fit.tr10 <- nls(v1.y.tr10 ~ SSasymp(v1.x.tr10, Asym, R0, lrc))

v1.a.tr10 = coef(v1.fit.tr10)[["R0"]]; v1.a.tr10 #a
v1.s.tr10 = exp(coef(v1.fit.tr10)[["lrc"]]); v1.s.tr10 #s

# change detection point
v1.f.tr10 <- function(x) v1.a.tr10 * exp(-v1.s.tr10 * x)
(v1.mc.tr10 = (maxcurv(x.range = c(550, 700), fun = v1.f.tr10, method="pd")))
(v1x.mc.tr10 = v1.mc.tr10$x0)
(v1y.mc.tr10 = v1.mc.tr10$y0)

# tr100 .........................................

#plot
ggplot(v.cctr100, aes(x = elev, y = vdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
v1.x.tr100 <- v.cctr100$elev; v1.x.tr100
v1.y.tr100 <- v.cctr100$vdif; v1.y.tr100

v1.fit.tr100 <- nls(v1.y.tr100 ~ SSasymp(v1.x.tr100, Asym, R0, lrc))

v1.a.tr100 = coef(v1.fit.tr100)[["R0"]]; v1.a.tr100 #a
v1.s.tr100 = exp(coef(v1.fit.tr100)[["lrc"]]); v1.s.tr100 #s

# change detection point
v1.f.tr100 <- function(x) v1.a.tr100 * exp(-v1.s.tr100 * x)
(v1.mc.tr100 = (maxcurv(x.range = c(550, 700), fun = v1.f.tr100, method="pd")))
(v1x.mc.tr100 = v1.mc.tr100$x0)
(v1y.mc.tr100 = v1.mc.tr100$y0)
# .................................................................................................

# Volume differences by lucc

# Preparing table

{
  v.luctr2 <- data %>% select(elev,v_luctr2) %>% mutate (group = "vcctr2")
  names(v.luctr2) <- c("elev","vdif","group")
  
  v.luctr10 <- data %>% select(elev,v_luctr10) %>% mutate (group = "vcctr10")
  names(v.luctr10) <- c("elev","vdif","group")
  
  v.luctr100 <- data %>% select(elev,v_luctr100) %>% mutate (group = "vcctr100")
  names(v.luctr100) <- c("elev","vdif","group")
  
  v.luc <- rbind(v.luctr2, v.luctr10, v.luctr100)
}

# Tr2 .............

#plot
ggplot(v.luctr2, aes(x = elev, y = vdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
v2.x.tr2 <- v.luctr2$elev; v2.x.tr2
v2.y.tr2 <- v.luctr2$vdif; v2.y.tr2

v2.fit.tr2 <- nls(v2.y.tr2 ~ SSasymp(v2.x.tr2, Asym, R0, lrc))

v2.a.tr2 = coef(v2.fit.tr2)[["R0"]]; v2.a.tr2 #a
v2.s.tr2 = exp(coef(v2.fit.tr2)[["lrc"]]); v2.s.tr2 #s

# change detection point
v2.f.tr2 <- function(x) v2.a.tr2 * exp(-v2.s.tr2 * x)
(v2.mc.tr2 = (maxcurv(x.range = c(550, 700), fun = v2.f.tr2, method="pd")))
(v2x.mc.tr2 = v2.mc.tr2$x0)
(v2y.mc.tr2 = v2.mc.tr2$y0)


# Tr10 .............

#plot
ggplot(v.luctr10, aes(x = elev, y = vdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
v2.x.tr10 <- v.luctr10$elev; v2.x.tr10
v2.y.tr10 <- v.luctr10$vdif; v2.y.tr10

v2.fit.tr10 <- nls(v2.y.tr10 ~ SSasymp(v2.x.tr10, Asym, R0, lrc))

v2.a.tr10 = coef(v2.fit.tr10)[["R0"]]; v2.a.tr10 #a
v2.s.tr10 = exp(coef(v2.fit.tr10)[["lrc"]]); v2.s.tr10 #s

# change detection point
v2.f.tr10 <- function(x) v2.a.tr10 * exp(-v2.s.tr10 * x)
(v2.mc.tr10 = (maxcurv(x.range = c(550, 700), fun = v2.f.tr10, method="pd")))
(v2x.mc.tr10 = v2.mc.tr10$x0)
(v2y.mc.tr10 = v2.mc.tr10$y0)


# tr100 .............

#plot
ggplot(v.luctr100, aes(x = elev, y = vdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
v2.x.tr100 <- v.luctr100$elev; v2.x.tr100
v2.y.tr100 <- v.luctr100$vdif; v2.y.tr100

v2.fit.tr100 <- nls(v2.y.tr100 ~ SSasymp(v2.x.tr100, Asym, R0, lrc))

v2.a.tr100 = coef(v2.fit.tr100)[["R0"]]; v2.a.tr100 #a
v2.s.tr100 = exp(coef(v2.fit.tr100)[["lrc"]]); v2.s.tr100 #s

# change detection point
v2.f.tr100 <- function(x) v2.a.tr100 * exp(-v2.s.tr100 * x)
(v2.mc.tr100 = (maxcurv(x.range = c(550, 700), fun = v2.f.tr100, method="pd")))
(v2x.mc.tr100 = v2.mc.tr100$x0)
(v2y.mc.tr100 = v2.mc.tr100$y0)


# .................................................................................................

# Volume differences by cc and lucc

# Preparing table for plots

{
  v.ccluctr2 <- data %>% select(elev,v_ccluctr2) %>% mutate (group = "vcctr2")
  names(v.ccluctr2) <- c("elev","vdif","group")
  
  v.ccluctr10 <- data %>% select(elev,v_ccluctr10) %>% mutate (group = "vcctr10")
  names(v.ccluctr10) <- c("elev","vdif","group")
  
  v.ccluctr100 <- data %>% select(elev,v_ccluctr100) %>% mutate (group = "vcctr100")
  names(v.ccluctr100) <- c("elev","vdif","group")
  
  v.ccluc <- rbind(v.ccluctr2, v.ccluctr10, v.ccluctr100)
}

# Tr2 .............

#plot
ggplot(v.ccluctr2, aes(x = elev, y = vdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
v3.x.tr2 <- v.ccluctr2$elev; v3.x.tr2
v3.y.tr2 <- v.ccluctr2$vdif; v3.y.tr2

v3.fit.tr2 <- nls(v3.y.tr2 ~ SSasymp(v3.x.tr2, Asym, R0, lrc))

v3.a.tr2 = coef(v3.fit.tr2)[["R0"]]; v3.a.tr2 #a
v3.s.tr2 = exp(coef(v3.fit.tr2)[["lrc"]]); v3.s.tr2 #s

# change detection point
v3.f.tr2 <- function(x) v3.a.tr2 * exp(-v3.s.tr2 * x)
(v3.mc.tr2 = (maxcurv(x.range = c(550, 700), fun = v3.f.tr2, method="pd")))
(v3x.mc.tr2 = v3.mc.tr2$x0)
(v3y.mc.tr2 = v3.mc.tr2$y0)


# Tr10 .............

#plot
ggplot(v.ccluctr10, aes(x = elev, y = vdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
v3.x.tr10 <- v.ccluctr10$elev; v3.x.tr10
v3.y.tr10 <- v.ccluctr10$vdif; v3.y.tr10

v3.fit.tr10 <- nls(v3.y.tr10 ~ SSasymp(v3.x.tr10, Asym, R0, lrc))

v3.a.tr10 = coef(v3.fit.tr10)[["R0"]]; v3.a.tr10 #a
v3.s.tr10 = exp(coef(v3.fit.tr10)[["lrc"]]); v3.s.tr10 #s

# change detection point
v3.f.tr10 <- function(x) v3.a.tr10 * exp(-v3.s.tr10 * x)
(v3.mc.tr10 = (maxcurv(x.range = c(550, 700), fun = v3.f.tr10, method="pd")))
(v3x.mc.tr10 = v3.mc.tr10$x0)
(v3y.mc.tr10 = v3.mc.tr10$y0)


# tr100 .............

#plot
ggplot(v.ccluctr100, aes(x = elev, y = vdif)) + 
  geom_point() +
  stat_smooth(method = "nls", formula = y ~ SSasymp(x, Asym, R0, lrc), se = FALSE)

# modelo exponencial descendente y = a*exp(-S*x)
v3.x.tr100 <- v.ccluctr100$elev; v3.x.tr100
v3.y.tr100 <- v.ccluctr100$vdif; v3.y.tr100

v3.fit.tr100 <- nls(v3.y.tr100 ~ SSasymp(v3.x.tr100, Asym, R0, lrc))

v3.a.tr100 = coef(v3.fit.tr100)[["R0"]]; v3.a.tr100 #a
v3.s.tr100 = exp(coef(v3.fit.tr100)[["lrc"]]); v3.s.tr100 #s

# change detection point
v3.f.tr100 <- function(x) v3.a.tr100 * exp(-v3.s.tr100 * x)
(v3.mc.tr100 = (maxcurv(x.range = c(550, 700), fun = v3.f.tr100, method="pd")))
(v3x.mc.tr100 = v3.mc.tr100$x0)
(v3y.mc.tr100 = v3.mc.tr100$y0)

# .................................................................................................

# final tables

# note: q1, q2 and q3 is for differences in peak flow by cc, luc and cc and lucc respectively
# note: v1, v2 and v3 is for differences in peak flow by cc, luc and cc and lucc respectively

# elevation of maximum curvature

mc.x <- data.frame(q.Rp2 = c(q1x.mc.tr2,q2x.mc.tr2,q3x.mc.tr2), 
                   q.Rp10 = c(q1x.mc.tr10,q2x.mc.tr10,q3x.mc.tr10),
                   q.Rp100 = c(q1x.mc.tr100,q2x.mc.tr100,q3x.mc.tr100),
                   v.Rp2 = c(v1x.mc.tr2,v2x.mc.tr2,v3x.mc.tr2), 
                   v.Rp10 = c(v1x.mc.tr10,v2x.mc.tr10,v3x.mc.tr10),
                   v.Rp100 = c(v1x.mc.tr100,v2x.mc.tr100,v3x.mc.tr100))


# difference of maximum curvature 

mc.y <- data.frame(q.Rp2 = c(q1y.mc.tr2,q2y.mc.tr2,q3y.mc.tr2), 
                   q.Rp10 = c(q1y.mc.tr10,q2y.mc.tr10,q3y.mc.tr10),
                   q.Rp100 = c(q1y.mc.tr100,q2y.mc.tr100,q3y.mc.tr100),
                   v.Rp2 = c(v1y.mc.tr2,v2y.mc.tr2,v3y.mc.tr2), 
                   v.Rp10 = c(v1y.mc.tr10,v2y.mc.tr10,v3y.mc.tr10),
                   v.Rp100 = c(v1y.mc.tr100,v2y.mc.tr100,v3y.mc.tr100))

mc <- rbind(mc.x, mc.y); mc
row.names(mc) <- c("cc.elev","luc.elev","ccluc.elev","cc.dif","luc.dif","ccluc.dif")

# end code ....