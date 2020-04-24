library(quantmod)
library(tidyverse)
library(tseries)
library(tidyquant)
library(xts)
library(tsbox)
library(moments) #skewness and kurtosis library
library(MSwM)
library(mixtools)

#retrieve data from yahoo for BMRI.JK
getSymbols("BMRI.JK")
data<-data.frame(coredata(BMRI.JK))
data<-na.omit(data)
data.close<-data.frame(y=data[,4])

#LOCF (Last Observation Carried Forward) pre-processing. 
#This eliminates NA on non-trading days by copying the value of last value (before NA) to the next observation
index.date<-seq(start(BMRI.JK),end(BMRI.JK),"day")
BMRI.JK.Close.LOCF<-na.locf(BMRI.JK$BMRI.JK.Close,xout=index.date)
BMRI.JK.Close.Value<-data.frame(y=as.numeric(BMRI.JK.Close.LOCF))

#stationarity test
adf.test(BMRI.JK.Close.Value$y)
sum(is.na(BMRI.JK.Close.Value$y))

#Plot 1 step lag difference 
BMRI.JK.Close.LaggedDifferences<-diff(BMRI.JK.Close.LOCF)
BMRI.JK.Close.LD.Value<-na.omit(data.frame(y=as.numeric(BMRI.JK.Close.LaggedDifferences)))
rownames(BMRI.JK.Close.LD.Value) <- NULL

adf.test(na.omit(BMRI.JK.Close.LaggedDifferences))

plot(BMRI.JK.Close.LD.Value$y,type="l")

data.lag<-data.frame(y=diff(data.close$y))

#plot histogram of original value and the 1-lagged difference
#hist(BMRI.JK.Close.Value$y,
#     col="peachpuff",
#     border="black",
#     prob=TRUE)
#lines(density(BMRI.JK.Close.Value$y),
#      lwd=2,
#      col="chocolate3")

#hist(BMRI.JK.Close.LaggedDifferences$BMRI.JK.Close,
#     col="green2",
#     prob=TRUE)
#lines(density(na.omit(BMRI.JK.Close.LaggedDifferences$BMRI.JK.Close)),
#      lwd=2,
#      col="black"
#      )

#skewness and kurtosis measurement
moments::skewness(BMRI.JK.Close.Value$y)
moments::kurtosis(BMRI.JK.Close.Value$y)

moments::skewness(BMRI.JK.Close.LaggedDifferences$BMRI.JK.Close,na.rm = TRUE)
moments::kurtosis(BMRI.JK.Close.LaggedDifferences$BMRI.JK.Close,na.rm = TRUE)


#mixture model
normalmix<-mixtools::normalmixEM(BMRI.JK.Close.Value$y,
                                 k=4)
normalmix$loglik
summary(normalmix)



#fitting MSwM
#y1<-coredata(na.omit(BMRI.JK.Close.LaggedDifferences$BMRI.JK.Close))
#x1<-Lag(coredata(na.omit(BMRI.JK.Close.LaggedDifferences$BMRI.JK.Close)),k=1)

#y1a<-coredata(BMRI.JK.Close.LaggedDifferences$BMRI.JK.Close)[3:4854]
#x1a<-Lag(coredata((BMRI.JK.Close.LaggedDifferences$BMRI.JK.Close)),k=1)[3:4854]

s.diff<-data.lag
rownames(s.diff) <- NULL
dim(s.diff)
s.diff4<-s.diff[5:3292,1]
s.diff3<-s.diff[4:3291,1]
s.diff2<-s.diff[3:3290,1]
s.diff1<-s.diff[2:3289,1]
s.difft<-s.diff[1:3288,1]

acf(s.diff,lag.max = 100)
pacf(s.diff)

mod<-lm(s.difft~s.diff1)
summary(mod)

mod.mswm.AR1.K2<-MSwM::msmFit(mod, k=2, sw=c(T,T,T))
summary(mod.mswm.AR1.K2)
plotProb(mod.mswm.AR1.K2,1)
AIC(mod.mswm.AR1.K2)

mod.mswm.AR1.K3<-MSwM::msmFit(mod, k=3, sw=c(T,T,T))
summary(mod.mswm.AR1.K3)
plotProb(mod.mswm.AR1.K3,2)
AIC(mod.mswm.AR1.K3)

mod.mswm.AR1.K4<-MSwM::msmFit(mod, k=4, sw=c(T,T,T))
summary(mod.mswm.AR1.K4)
plotProb(mod.mswm.AR1.K4)
AIC(mod.mswm.AR1.K4)

mod.mswm.AR1.K5<-MSwM::msmFit(mod, k=5, sw=c(T,T,T))
summary(mod.mswm.AR1.K5)
AIC(mod.mswm.AR1.K5)

mod2<-lm(s.difft~s.diff1+s.diff2)
summary(mod2)

mod2.mswm.AR2.K2<-MSwM::msmFit(mod2, k=2, sw=c(T,T,T,T))
summary(mod2.mswm.AR2.K2)
AIC(mod2.mswm.AR2.K2)

mod2.mswm.AR2.K3<-MSwM::msmFit(mod2, k=3, sw=c(T,T,T,T))
summary(mod2.mswm.AR2.K3)
AIC(mod2.mswm.AR2.K3)

mod2.mswm.AR2.K4<-MSwM::msmFit(mod2, k=4, sw=c(T,T,T,T))
summary(mod2.mswm.AR2.K4)
AIC(mod2.mswm.AR2.K4)

mod2.mswm.AR2.K5<-MSwM::msmFit(mod2, k=5, sw=c(T,T,T,T))
summary(mod2.mswm.AR2.K5)
AIC(mod2.mswm.AR2.K5)

mod3<-lm(s.difft~s.diff1+s.diff2+s.diff3)
summary(mod3)

mod3.mswm.AR3.K2<-MSwM::msmFit(mod3, k=2, sw=c(T,T,T,T,T))
summary(mod3.mswm.AR3.K2)
AIC(mod3.mswm.AR3.K2)

mod3.mswm.AR3.K3<-MSwM::msmFit(mod3, k=3, sw=c(T,T,T,T,T))
summary(mod3.mswm.AR3.K3)
AIC(mod3.mswm.AR3.K3)

mod3.mswm.AR3.K4<-MSwM::msmFit(mod3, k=4, sw=c(T,T,T,T,T))
summary(mod3.mswm.AR3.K4)
AIC(mod3.mswm.AR3.K4)

mod3.mswm.AR3.K5<-MSwM::msmFit(mod3, k=5, sw=c(T,T,T,T,T))
summary(mod3.mswm.AR3.K5)
AIC(mod3.mswm.AR3.K5)

mod4<-lm(s.difft~s.diff1+s.diff2+s.diff3+s.diff4)
summary(mod4)

mod4.mswm.AR4.K2<-MSwM::msmFit(mod4, k=2, sw=c(T,T,T,T,T,T))
summary(mod4.mswm.AR4.K2)
AIC(mod4.mswm.AR4.K2)

mod4.mswm.AR4.K3<-MSwM::msmFit(mod4, k=3, sw=c(T,T,T,T,T,T))
summary(mod4.mswm.AR4.K3)
AIC(mod4.mswm.AR4.K3)

mod4.mswm.AR4.K4<-MSwM::msmFit(mod4, k=4, sw=c(T,T,T,T,T,T))
summary(mod4.mswm.AR4.K4)
AIC(mod4.mswm.AR4.K4)

mod4.mswm.AR4.K5<-MSwM::msmFit(mod4, k=5, sw=c(T,T,T,T,T,T))
summary(mod4.mswm.AR4.K5)
AIC(mod4.mswm.AR4.K5)

mod4.mswm.AR4.K6<-MSwM::msmFit(mod4, k=6, sw=c(T,T,T,T,T,T))
summary(mod4.mswm.AR4.K6)
AIC(mod4.mswm.AR4.K6)

#      k=2	    k=3	      k=4	      k=5       k=6
#AR1	38134.15	37960.76	37915.05	37959.03
#AR2	38126.98	37956.64	37956.72	37906.32
#AR3	38126.86	37954.67	37959.00	37912.02
#AR4	38122.93	37951.92	37950.23	37871.45  37869.1

label<-data.frame(mod4.mswm.AR4.K6@Fit@filtProb)

#detecting regime per datapoint
regime<-data.frame(regime=double())
for (a in c(1:dim(label)[1]))
{
  subset.label<-label[a,]
  regime.position<-which(subset.label==max(subset.label))
  regime<-rbind(regime,data.frame(regime=regime.position))
}


list.arl <- vector(mode="list", length=max(regime))
for (a in c(1:max(regime)))
{
  
  names(list.arl)[a]<-(a)
  
}

#list.arl[[1]]<-rep(3,5)
#list.arl[[1]]<-c(list.arl[[1]],10)
#list.arl[[4]]<-rep(2,10)


#average run length
counter<-0;regime.switch.count<-0
for (a in c( 2:(dim(regime)[1]) ))
{
  x.curr<-regime[a,1]
  x.prev<-regime[(a-1),1]
  
  if(x.curr==x.prev)
  {
    #print("sama")
    counter<-counter+1
  } else
  {
    #print("tidak sama")
    counter<-counter+1
    print(counter)
    list.arl[[x.curr]]<-c(list.arl[[x.curr]],counter)
    counter<-0
    regime.switch.count<-regime.switch.count+1
  }
  
}

hist

regime[2761,1]
