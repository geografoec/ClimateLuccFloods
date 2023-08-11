# El objetivo de este script es hacer una correcion del bias con el m?todo "quantite mapping"
# Los datos diarios de lluvia observada son de la estacion meteorol?gica M1219-Tena Hda Chaupishungo 
# (-0.9168;-77.819) rellenos con missForest y los datos simulados son del CMIP6-MIROC (historical) 

# Editado por: Jorge Hurtado-Pidal
# Fecha: 18 octubre de 2022

# Los pasos a seguir son:
# a. Preparar las series diarias de precipitaci?n del per?odo 1981-2010 de observaciones (M1219) 
#    y del modelo MIROC (historical data). Hacer un split de la serie 70:30 (calibracion:validaci?n).
# b. Hacer las correccion del bias (package ?qmap?), calculado la funcion de transferencia 
#    usando el per?odo comun con el 70% de los datos (training data set).
# c. Evaluar la correcci?n del bias (escala diaria) con gr?ficos (frecuencia acumulada y de quantiles)
#    y estad?sticos (RMSE, PBIAS, entre otros), con el 30% de datos restantes (test data set)
# d. Repetir la validaci?n a escala anual.
# e. Preparar las series diarias y anuales del escenario futuro

#...................................................................................................

# Preparing the environment
cat("\014")   
rm(list = ls())
graphics.off()
#load("D:/DoctTenaFloods/3lulcclimatefloods/ClimateScenario/BiasCorrection/RWorkingSpace/rcode9_wspace.RData")

# libraries

{
library(lubridate)
library(ncdf4)
library(raster)
library(rgdal)
library(maptools)
library(sp)
library(tidyverse)
library(readxl)
library(qmap)
library(zoo)
library(latticeExtra)
library(hydroGOF)
library(hydroTSM)
library(EnvStats)
library(overlapping)
library(trend)
library(groupdata2) 
}

# a. Precipitation time series---------------------------------------------------------------------

# observed M1219 daily precipitation from 1981-01-01 to 2016-12-31.

# load data
obs <- read.csv("./Methods/tenafill_mF.csv", sep =","); head(obs) # datos rellenados con "missForest"

# formating dates
date <- strptime(obs$date, format="%Y-%m-%d")
obs$date <- date
names(obs) <- c("date","precip")

plot(obs$date, obs$precip, type="l", col="blue",
     main="Observed Precipitation", xlab="Time (days)",ylab="Precipitation (mm)")

# annual maximum from gauge

obs$date2 <- as.Date(obs$date, "%Y-%m-%d"); head(obs)
obs.zoo <- zoo(obs$precip, order.by = obs$date2) # observed data
#hydroplot(obs.zoo, var.type="Precipitation", var.unit="mm",ylab="Precipitation") # herramienta hydroplot(hydroTSM): graficos diarios, mensuales y anuales
obs.year <- daily2annual(obs.zoo, FUN = max, na.rm=TRUE); head(obs.year) # maximos anuales para la hoja de excel

# historical daily precipitation CMIP6-MIROC from 1980-01-01 to 2014-12-31

# load data 
historical <- "./Methods/pr_day_MIROC-ES2L_historical_r1i1p1f2_gn_19800101-20141231_v20191129.nc"
hist.nc <- nc_open(historical)
hist.nc <- brick(historical,varname="pr")

# gauge
gauge <- readOGR("./Methods/shapes/M1219point.shp")

# area Ecuador
areaec <- readOGR("./Methods/shapes/areaec.shp")

# plots 
plot(hist.nc[[1]])
plot(gauge, add=T)
plot(areaec, add=T)

# extracting and formating precipitation time series from climate model
hist <- raster::extract(hist.nc, gauge, method='simple', df=TRUE)
hist <- data.frame(t(hist))
hist$date <- row.names(hist)
hist <- data.frame(hist[-1,])
rownames(hist)<- NULL
hist$date1 <- substring(hist$date, first=2) # fixing dates
date1 <- strptime(hist$date1, format="%Y.%m.%d") # date object
hist$date1 <- date1
hist$precip <- (hist$t.hist.*86400) # x 86400 (seconds per day) original units are kg/m2/segundo
hist <- hist[,3:4] # delete columns 1,2,3
names(hist) <- c("date","precip")

# Time series in the same period from 01-01-2008 to 31-12-2014

from = force_tz(ymd("1981-01-01")) # forced to maintain the utc 
to = force_tz(ymd("2010-12-31"))

obs <- obs %>% dplyr::select(date, precip) %>% filter(date >= from & date <= to) 
hist <- hist %>% dplyr::select(date, precip) %>% filter(date >= from & date <= to) 

# putting series in the same data frame
prec <- merge(obs, hist, by="date", all.x=TRUE); head(prec) 
names(prec) <- c("date","pobs","phist")
prec$pobs <- round(prec$pobs, digits = 1)
prec$phist <- round(prec$phist, digits = 1)
colSums(is.na(prec)) # we have 4 NA in pobs
#prec$pobs[is.na(prec$pobs)] <- 0 # para no tener NA

# split de la serie 70:30 (calibracion:validaci?n)

set.seed(10)
partition <- partition(prec, p=0.7, list_out = FALSE)

train <- partition %>% dplyr::select(date, pobs, phist, .partitions) %>% 
  filter(.partitions == 1);head(train) 

test <- partition %>% dplyr::select(date, pobs, phist, .partitions) %>% 
  filter(.partitions == 2);head(test) 

# b. bias correction (BC)---------------------------------------------------------------------------

# fitting a transfer function, using the series (observed and modeled) in the same period (2016-2020)
qm.fit <- fitQmapQUANT(train$pobs,train$phist,
                       qstep = 0.1,nboot = 1,wet.day = TRUE); qm.fit

# using fitted function for historical test data BC of MIROC model
qm.a <- doQmapQUANT(test$phist, qm.fit, type="linear");qm.a
qm.s <- doQmapQUANT(test$phist, qm.fit, type="tricub");qm.s

# c. Plots and statistics. Evaluation of BC --------------------------------------------------------

# plots of test data without boias correction
plot(test$date, test$pobs, type="l", col="blue")
plot(test$date, test$phist, type="l")

hist(test$pobs)
hist(test$phist)

plot(test$pobs, test$phist)

# cumulative frequency curve (CFC)
dt <- (1/length(test$date)); dt # incrementos
dt1 <- seq((0+dt), 1, dt); dt1 # vector

cfcpobs <- data.frame(sort(test$pobs, decreasing=FALSE))# CFC of observations
names(cfcpobs) <- c("cfcpobs")

cfcphist <- data.frame(sort(test$phist, decreasing=FALSE))# CFC of historical simulation
names(cfcphist) <- c("cfcphist")

# CFC of observations (M1219)
plot(cfcpobs$cfcpobs,dt100, xlim = c(0, 150),type="l", col ="red", lwd=3, 
     main="Cumulative frequency Curve", xlab="", ylab="");par(new=TRUE) # to overlap plots

# CFC of historical (MIROC)
plot(cfcphist$cfcphist,dt100, xlim = c(0, 150),type="l", col ="blue", lwd=1,
     main="Cumulative frequency Curve", xlab="", ylab="")

# scatterplots: observado vs historico correguido
plot(test$pobs, qm.a) # BC-linear model

# quantile plot
quant <- function(x,qstep=0.01){
  qq <- quantile(x,prob=seq(0,1,by=qstep))
  #sqrt(qq)
}

plot(quant(test$phist),
     quant(test$pobs), pch=1, xlab="", ylab="")
lines(quant(test$phist),
      quant(qm.a),
      col="red")
# lines(sqrtquant(qm.s),
#       sqrtquant(test$phist),
#       col="blue")
# points(sqrt(qm.fit$par$fitq),
#        sqrt(qm.fit$par$modq),
#        pch=19,cex=0.5,col="green")
# legend("topleft",
#        legend=c("data","model"),
#        lty=c(NA,1),pch=c(1,NA),
#        col=c("black","red"))

# plot series. observed and modeled (qmap bias corrected)
par(mfrow=c(2,1))
plot(test$date, test$pobs, ylim = c(0, 250), type="l", col = "blue",
     main="Observed",xlab="", ylab="")

plot(test$date, qm.a, ylim = c(0, 250), type="l", col = "black",
     main="Historical-BC",xlab="", ylab="")

# plot modeled series, without and with correction 
plot(test$date, test$phist, ylim = c(0, 250), type="l", col = "blue",
     main="Historical-RAW",xlab="", ylab="")

plot(test$date, qm.a, ylim = c(0, 250), type="l", col = "black",
     main="Historical-BC",xlab="", ylab="")

# Cumulative Frequency Curve with Bias Correction. CFC-BC 
cfcph.bc <- data.frame(sort(qm.a, decreasing=FALSE))# simulated bias corrected
names(cfcph.bc) <- c("cfcph.bc")

par(mfrow=c(1,1))

# CFC observed
plot(cfcpobs$cfcpobs,dt1, xlim = c(0, 250),type="l", lwd=2, 
     main="", xlab="", ylab="");par(new=TRUE) 

#CFC historical-raw
plot(cfcphist$cfcphist,dt1, xlim = c(0, 250),type="l", lwd=2, lty=2, col="blue",
     main="", xlab="", ylab="");par(new=TRUE) 

#CFC historical-BC
plot(cfcph.bc$cfcph.bc,dt1, xlim = c(0, 250),type="l", lwd=2,col="red",
     main="", xlab="", ylab="");par(new=TRUE) 

# legend("bottomright",
#        legend=c("Observed","RAW","GCM-BC"),
#        lty=c(1,2,1),
#        lwd = c(2,2,2),
#        col=c("black","blue","red"))

# density plot
epdfPlot(test$pobs,epdf.col = "red")
epdfPlot(test$phist, add=T)
epdfPlot(qm.a, add=T,epdf.col = "blue",epdf.lwd = 1)

# PDF score, overlapping area
x <- list(obs=test$pobs, raw=test$phist, bc=qm.a)
out <- overlap(x, plot=TRUE)
out$OV

# daily summary statistics
summary(test$pobs)
summary(test$phist)
summary(qm.a)

# daily performance statistics. Using hydroGOF library
gof1 <- gof(sim=test$phist, obs = test$pobs);gof1 # without bias correction
gof2 <- gof(sim=qm.a, obs = test$pobs); gof2 # with BC using lineal model

gof.daily <- cbind(gof1, gof2); gof.daily
colnames(gof.daily) <- c("hist","BC"); gof.daily

# Mann-kendal tests for trend analysis

# H0: NO hay tendencia, H1: SI hay tendencia
# Si p<0.05 se rechaza la H0

mkt.obs <- mk.test(test$pobs); mkt.obs    #> p>0.05; NO hay tendencia

mkt.hist <- mk.test(test$phist); mkt.hist #> p>0.05; NO hay tendencia

mkt.bc <- mk.test(qm.a); mkt.bc           #> p>0.05; NO hay tendencia

# d. Annual Analysis (BONUS)----------------------------------------------------------------------

# note1: We hypothesize that at annual time scale, the performance metrics will improve.

# hydroplot with hydroTSM library
df.daily <- cbind(test, qm.a); head(df.daily)
df.daily$date2 <- as.Date(df.daily$date, "%Y-%m-%d"); head(df.daily)
names(df.daily) <- c("date","pobs","phist","partition","qmbc","date2"); head(df.daily)
obs.daily <- zoo(df.daily$pobs, order.by = df.daily$date2) # observed data
hist.daily <- zoo(df.daily$phist, order.by = df.daily$date2) # historical raw test data (MIROC model) 
bc.daily <- zoo(df.daily$qmbc, order.by = df.daily$date2) # bias corrected 

#dev.new(). no se pueden hacer porque hay muchos faltantes porque el test data set es aleatorio no en orden
#hydroplot(obs.daily, var.type="Precipitation", var.unit="mm",ylab="prec") # herramienta hydroplot(hydroTSM): graficos diarios, mensuales y anuales
#dev.new()
#hydroplot(bc.daily, var.type="Precipitation", var.unit="mm",ylab="prec") # herramienta hydroplot(hydroTSM): graficos diarios, mensuales y anuales

# Annual Analysis 

# Annual - Maximun daily - testipitation
obs.annual <- daily2annual(obs.daily, FUN = max, na.rm=TRUE); head(obs.annual)
hist.annual <- daily2annual(hist.daily, FUN = max, na.rm=TRUE); head(hist.annual)
bc.annual <- daily2annual(bc.daily, FUN = max, na.rm=TRUE); head(bc.annual)

# Two plots overlapped
#par(mfrow=c(1,1))
plot(obs.annual,ylim = c(0, 250),
     main="Annual Max24hPP",xlab="Year", ylab="testipitation (mm)");par(new=TRUE)
plot(bc.annual, ylim = c(0, 250), type= "l", col= "blue",
     main="",xlab="", ylab="")

# annual summary statistics
df.annual <- data.frame(cbind(obs.annual, hist.annual, bc.annual))

summary(df.annual$obs.annual)
summary(df.annual$hist.annual)
summary(df.annual$bc.annual)

# Annual performance statistics. Using hydroGOF library

gof5 <- gof(sim=df.annual$hist.annual, obs = df.annual$obs.annual);gof5 
gof6 <- gof(sim=df.annual$bc.annual, obs = df.annual$obs.annual);gof6 

gof.annual <- cbind(gof5, gof6); gof.annual
colnames(gof.annual) <- c("hist","BC"); gof.annual
 
# e. Future time series-----------------------------------------------------------------------------

# daily precipitation CMIP6-MIROC-SSP5-8.5 from 2015-01-01 to 2070-12-31
# note: once the transfer function is ready, we can apply it to the future scenario time series

# load data 
future <- "./Methods/pr_day_MIROC-ES2L_ssp585_r1i1p1f2_gn_20150101-20701231_v20200318.nc"
ssp.nc <- nc_open(future)
ssp.nc <- brick(future,varname="pr")

# plots 
plot(ssp.nc[[1]])
plot(gauge, add=T)
plot(areaec, add=T)

# extracting and formating precipitation time series from climate model
ssp <- raster::extract(ssp.nc, gauge, method='simple', df=TRUE)
ssp <- data.frame(t(ssp))
ssp$date <- row.names(ssp)
ssp <- data.frame(ssp[-1,])
rownames(ssp)<- NULL
ssp$date1 <- substring(ssp$date, first=2) # fixing dates
date1 <- strptime(ssp$date1, format="%Y.%m.%d") # date object
ssp$date1 <- date1
ssp$precip <- (ssp$t.ssp.*86400) # x 86400 (seconds per day) original units are kg/m2/segundo
ssp <- ssp[,3:4] # delete columns 1,2,3
names(ssp) <- c("date","precip")

# Time series in the same period from 2041-01-01 to 2060-12-31

from1 = force_tz(ymd("2041-01-01")) # forced to maintain the utc 
to1 = force_tz(ymd("2070-12-31"))

ssp <- ssp %>% dplyr::select(date, precip) %>% 
  filter(date >= from1 & date <= to1)
ssp$precip <- round(ssp$precip, digits = 1)
colSums(is.na(ssp))

# bias correction
qm.a1 <- doQmapQUANT(ssp$precip, qm.fit, type="linear");qm.a1
qm.a1 <- round(qm.a1, digits = 1)
df.ssp <- cbind(ssp,qm.a1)
names(df.ssp) <- c("date","rawssp","bcssp")

# ssp daily summary statistics
summary(df.ssp$bcssp)

# Mann-kendal tests for trend analysis
mkt.rawssp <- mk.test(df.ssp$rawssp); mkt.rawssp #> p<0.05; si hay tendencia
mkt.bcssp <- mk.test(df.ssp$bcssp); mkt.bcssp #> p<0.05; si hay tendencia

# bias corrected ssp hydroplot
df.ssp$date2 <- as.Date(df.ssp$date, "%Y-%m-%d"); head(df.ssp)
bcssp.daily <- zoo(df.ssp$bcssp, order.by = df.ssp$date2) # bias corrected 
hydroplot(bcssp.daily, var.type="Precipitation", var.unit="mm",ylab="Precipitation") # herramienta hydroplot(hydroTSM): graficos diarios, mensuales y anuales

# Annual - Maximun daily - Precipitation
bcssp.annual <- daily2annual(bcssp.daily, FUN = max, na.rm=TRUE); head(bcssp.annual)
bcssp.annual <- data.frame(bcssp.annual) # maximos anuales futuros para la hoja de excel
bcssp.annual$year <- seq(2041,2070,1)
names(bcssp.annual) <- c("ppmax24h","year")

summary(bcssp.annual$ppmax24h)

ggplot()+
  geom_bar(data = bcssp.annual, mapping=aes(x=year, y=ppmax24h), stat ="identity")+
  geom_line(data= bcssp.annual, aes(x=year, y=ppmax24h),alpha = 1,size = 1, 
            color="blue",linetype="dashed")

mkt.max24h <- mk.test(bcssp.annual$ppmax24h); mkt.max24h #> p>0.05; no hay tendencia

# Saving tables with bias correction time series----------------------------------------------------

#write.csv(df.ssp,"./BiasCorrection/Routputs/rcode9_daily_mirocssp585_bcm1219splited.csv",row.names = FALSE)
#save.image("./BiasCorrection/RWorkingSpace/rcode9_wspace.RData")

# fin script.......................................................................................
