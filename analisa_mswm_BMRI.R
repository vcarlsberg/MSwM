library(quantmod)
library(tidyverse)
library(tseries)
library(tidyquant)
library(xts)
library(tsbox)
library(moments) #skewness and kurtosis library

#retrieve data from yahoo for BMRI.JK
getSymbols("BMRI.JK")

#LOCF (Last Observation Carried Forward) pre-processing. This eliminates NA on non-trading days.
#start and date index from previous symbol is captured, then interpolated using na.locf
index.date<-seq(start(BMRI.JK),end(BMRI.JK),"day")
BMRI.JK.Close.Value<-na.locf(BMRI.JK$BMRI.JK.Close,xout=index.date,x)
plot(BMRI.JK.Close.Value)

#Plot 1 step lag difference --> ga tau ini buat apa
BMRI.JK.Close.LaggedDifferences<-diff(BMRI.JK.Close.Value,differences = 1)
plot(BMRI.JK.Close.LaggedDifferences)

#plot histogram of original value and the 1-lagged difference
hist(BMRI.JK.Close.Value)
plot(density(BMRI.JK.Close.Value))

hist(BMRI.JK.Close.LaggedDifferences)
plot(density(na.omit(BMRI.JK.Close.LaggedDifferences)))

#skewness and kurtosis measurement
moments::skewness(BMRI.JK.Close.Value)
