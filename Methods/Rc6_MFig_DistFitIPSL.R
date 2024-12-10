# El objetivo de este script es hacer un ajuste de distribucion de frecuencias 
# usando la serie de precipitaci?n maxima en 24h del modelo IPLS SSP585, per?odo 2041-2070.
# A esta serie se le corrigi? el bias usando la estacion M1219 rellena con missForest 

# Editado por: Jorge Hurtado
# Fecha: 9 de septiembre de 2022

# Pasos a seguir:
# a. Preparar las series
# b. Hacer al ajuste a varias distribuciones teoricas (weibull, gamma, lognormal, normal)  
# c. Evaluar el ajuste para seleccionar el mejor modelo

# Fig. 3. Fitted extreme value distributions. 

#..................................................................................................

# Preparing the environment

cat("\014")   
rm(list = ls())
graphics.off()

# libraries

{
    library(fitdistrplus)
    library(MASS)
    library(survival)
    library(actuar)
    library(distrMod)
    library(readxl)
    library(ggplot2)
    library(RColorBrewer)
    library(tidyverse)
}

# load data

data <- read_excel("./Methods/ClimateScenarioData.xlsx",sheet=1); head(data)
data <- data$IPSL



# histogram of frequencies and empirical cumulative distribution

plotdist(data, histo=T, demp=T)

# Graphic of kurtosis and skewness

descdist(data)

# fit distributions

weibull.fit <- fitdist(data, "weibull"); summary(weibull.fit)
gamma.fit <- fitdist(data, "gamma"); summary(gamma.fit)
lnorm.fit <- fitdist(data, "lnorm"); summary(lnorm.fit)
norm.fit <- fitdist(data, "norm"); summary(norm.fit)

#Distribui?n Gumbel
dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
qgumbel <- function(p, a, b) a-b*log(-log(p))

gumbel.fit <- fitdist(data, "gumbel",start=list(a=10 , b=10)); summary(gumbel.fit)

# graphical comparison of distributions

#par(mfrow = c(1, 1),cex.axis=1.4,cex.lab=1.4,cex.main=1.4)

plot.legend <- c("Gamma","Gumbel","log-Normal","Normal","Weibull")

denscomp(list(gamma.fit,gumbel.fit,lnorm.fit, norm.fit,weibull.fit), 
         main = 'Densities', xlab = 'MAP (mm/day)', xlim = c(0,300), 
         ylab = 'Density',legendtext = plot.legend,
         xlegend = 'topright', ylegend = NULL,cex=1.2)
box()

cdfcomp(list(gamma.fit,gumbel.fit,lnorm.fit, norm.fit,weibull.fit), 
        main = 'CDFs', xlab = 'MAP (mm/day)', xlim = c(0,250), 
        ylab = 'CDF', legendtext = plot.legend,
        xlegend = 'topleft', ylegend = NULL, cex=1.2)

ppcomp(list(gamma.fit,gumbel.fit,lnorm.fit, norm.fit,weibull.fit),
       main = 'P-P Plot', xlab = 'Theoretical probabilities', xlim = c(0,1), 
       ylab = 'Empirical probabilities', legendtext = plot.legend,
       xlegend = 'topleft', ylegend = NULL,cex=1.2)

# goodness of fit

gofstat(list(gamma.fit,gumbel.fit,lnorm.fit, norm.fit,weibull.fit),
        fitnames = c("Gamma","Gumbel","log-Normal","Normal","Weibull"))

# nota: mientras mas peque?o el estad?stico mejor se ajusta la distribuci?n
# en general Gamma y log-Nnormal son las mejores en este caso

# linear regression and r2: teorical vs empirical probabilities

# vector of probabilities
a <- length(data)
dp <- 1/a
vprob <- c(dp)
for(i in 1:(a-1)) {
    vprob[i+1] <- vprob[i]+dp  
    print(vprob)
}
vprob

# sorting data values
data.sort <- sort(data, decreasing = F); data.sort

# Weibull
shape <- (weibull.fit$estimate["shape"])
scale <- (weibull.fit$estimate["scale"])
teor.weibull <- qweibull(vprob, shape = shape, scale = scale) 
mod.weibull <- lm(teor.weibull ~ data.sort); summary(mod.weibull)
plot(data.sort,teor.weibull,pch=4, main="P-P Plot (Weibull)")
abline(mod.weibull,lwd = 1, col = "red")

# Gamma
shape1 <- (gamma.fit$estimate["shape"])
rate <- (gamma.fit$estimate["rate"])
teor.gamma <- qgamma(vprob, shape = shape1, rate = rate) 
mod.gamma <- lm(teor.gamma ~ data.sort); summary(mod.gamma)
plot(data.sort,teor.gamma,pch=4, main="P-P Plot (Gamma)")
abline(mod.gamma,lwd = 1, col = "red")

# log-Normal
meanlog <- (lnorm.fit$estimate["meanlog"])
sdlog <- (lnorm.fit$estimate["sdlog"])
teor.lnorm <- qlnorm(vprob, meanlog = meanlog, sdlog = sdlog) 
mod.lnorm <- lm(teor.lnorm ~ data.sort); summary(mod.lnorm)
plot(data.sort,teor.lnorm,pch=4, main="P-P Plot (log-Normal)")
abline(mod.lnorm,lwd = 1, col = "red")

# Normal
mean <- (norm.fit$estimate["mean"])
sd <- (norm.fit$estimate["sd"])
teor.norm <- qnorm(vprob, mean = mean, sd = sd) 
mod.norm <- lm(teor.norm ~ data.sort); summary(mod.norm)
plot(data.sort,teor.norm,pch=4, main="P-P Plot (Normal)")
abline(mod.norm,lwd = 1, col = "red")

# Gumbel
a <- (gumbel.fit$estimate["a"])
b <- (gumbel.fit$estimate["b"])
teor.gumbel <- qgumbel(vprob, a = a, b = b) 
mod.gumbel <- lm(teor.gumbel ~ data.sort); summary(mod.gumbel)
plot(data.sort,teor.gumbel,pch=4, main="P-P Plot (Gumbel)")
abline(mod.gumbel,lwd = 1, col = "red")

# Obtaining the quantiles with theorical distributions

# Return period
{
    tr1 <- 1-(1/1.025)
    tr2 <- 1-(1/2)
    tr5 <- 1-(1/5)
    tr10 <- 1-(1/10)
    tr25 <- 1-(1/25)
    tr50 <- 1-(1/50)
    tr100 <- 1-(1/100)
    rp <- c(tr1, tr2, tr5, tr10, tr25, tr50, tr100)
}

# Weibull
weibull.rp <- c(0)
for(i in 1:7) {
    weibull.rp[i+1] <- qweibull(rp[i], shape = shape, scale = scale)  
}; weibull.rp

# Gamma
gamma.rp <- c(0)
for(i in 1:7) {
    gamma.rp[i+1] <- qgamma(rp[i], shape = shape1, rate =  rate)  
}; gamma.rp

# log-Normal
lognormal.rp <- c(0)
for(i in 1:7) {
    lognormal.rp[i+1] <- qlnorm(rp[i], meanlog = meanlog, sdlog = sdlog)  
}; lognormal.rp

# Normal
normal.rp <- c(0)
for(i in 1:7) {
    normal.rp[i+1] <- qnorm(rp[i], mean = mean, sd = sd)  
}; normal.rp

# Gumbel
gumbel.rp <- c(0)
for(i in 1:7) {
    gumbel.rp[i+1] <- qgumbel(rp[i], a = a, b = b)  
}; gumbel.rp

# Final Table
table <- data.frame(rbind(weibull.rp, gamma.rp, lognormal.rp, normal.rp,gumbel.rp)); table
table <- table[,(2:8)]; table
names(table) <- c("tr1","tr2","tr5","tr10","tr25","tr50","tr100"); table


# plot barras tr2, tr10, tr100

(data <- table %>% mutate (dist = rownames(table)))
row.names(data) <-  NULL; data

data$dist <- substr(data$dist, 1, nchar(data$dist) - 3)

{
    
    data.tr2 <- data %>% dplyr::select(tr2, dist) %>% mutate (group = "Rp2")
    names(data.tr2) <- c("prec","dist","Rp")
    
    data.tr10 <- data %>% dplyr::select(tr10, dist) %>% mutate (group = "Rp10")
    names(data.tr10) <- c("prec","dist","Rp")
    
    data.tr100 <- data %>% dplyr::select(tr100, dist) %>% mutate (group = "Rp100")
    names(data.tr100) <- c("prec","dist","Rp")
    
    data2 <- rbind(data.tr2, data.tr10, data.tr100)
    
}

# Especificar el orden de los nombres de barra
orden_rp <- c("Rp2", "Rp10", "Rp100")

# Convertir la variable "nombre" en un factor con el orden deseado
data2$Rp <- factor(data2$Rp, levels = orden_rp)

#colores <- c("red","blue", "darkgreen", "orange", "green")

(f1 <- ggplot(data2, aes(x = Rp, y = prec, fill = dist)) +
        geom_col(position = "dodge", alpha = 0.7, show.legend = F) +
        coord_cartesian(ylim = c(0, 300))+
        labs(title = "", x = "", y = "", show.legend = F)+
        scale_fill_manual(values = brewer.pal(5, "Set1")) +
        
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


# End of script....................................................................................