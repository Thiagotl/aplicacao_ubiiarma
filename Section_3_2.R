library(forecast)
library(dplyr)
library(BTSR)
source("ubxiiarma.fit.r")
######################
## Data preparation ##
######################
data <- readr::read_delim("combined_hourly_data.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(timestamp=as.POSIXct(timestamp, tz="GMT",
                              origin="1970-01-01 00:00:00"),
         hum=hum/100)
#
n<-round(dim(data)[1]*.8)-149
data<-data[150:1870,]

#########################
## Train and test sets ##
#########################
datatrain<-cbind(data[1:n,])  
colnames(datatrain)<-paste0(colnames(datatrain),"_train")

datatest<-cbind(data[(n+1):(dim(data)[1]),])  
colnames(datatest)<-paste0(colnames(datatest),"_test")

View(datatrain)


datatrain$RSSI_03_train[is.na(datatrain$RSSI_03_train)]<-
  mean(na.omit(datatrain$RSSI_03_train))
datatrain$RSSI_04_train[is.na(datatrain$RSSI_04_train)]<-
  mean(na.omit(datatrain$RSSI_04_train))
datatrain$RSSI_05_train[is.na(datatrain$RSSI_05_train)]<-
  mean(na.omit(datatrain$RSSI_05_train))
datatrain$RSSI_06_train[is.na(datatrain$RSSI_06_train)]<-
  mean(na.omit(datatrain$RSSI_06_train))
datatrain$RSSI_08_train[is.na(datatrain$RSSI_08_train)]<-
  mean(na.omit(datatrain$RSSI_08_train))

attach(datatrain)
attach(datatest)
attach(data)
X<-as.matrix(datatrain[,6:13]) 
Xtest<-as.matrix(datatest[,6:13])




######################
## stacionary tests ##
######################
truncation<-round(12*(n/100)^(.25)) # Schwert rule
adf.level<-tseries::adf.test(hum_train,k=truncation) # stacionary
adf.diff<-tseries::adf.test(diff(hum_train),k=truncation) # stacionary
kpss.level<-tseries::kpss.test(hum_train,lshort = F) # non-stacionary
kpss.diff<-tseries::kpss.test(diff(hum_train),lshort = F) # stacionary

table.stationarity<-data.frame(
  Series=c("In Level","1st difference"),
  ADF=c(adf.level$statistic,adf.diff$statistic),
  `p-value ADF`=c(adf.level$p.value,adf.diff$p.value),
  KPSS=c(kpss.level$statistic,kpss.diff$statistic),
  `p-value KPSS`=c(kpss.level$p.value,kpss.diff$p.value)
)

########################
## Fitting the models ##
########################
a01<-auto.arima(hum_train)
new1<-Arima(hum_test,model=a01) #one-step-ahead
a02<-auto.arima(hum_train, xreg = X)
new2<-Arima(hum_test,xreg = Xtest,model=a02) #one-step-ahead


ubxiiarma<-ubxiiarma.fit(ts(hum_train),ar=1,ma=1)

ubxiiarma$forecast
ubxiiarma$residuals
plot(ubxiiarma$residuals)

ubxiiarma<-ubxiiarma.fit(ts(hum_train),ar=1,ma=1,tau=.1)
forecast::accuracy(ubxiiarma$fitted[-c(1:3)], hum_train[-c(1:3)])



quant<-.25
order<-matrix(NA,16,8)
cont<-1
for(i in 0:3){
  for(j in 0:3){
    barma<-summary(BARFIMA.fit(hum_train,p=i,d=F,q=j,info=T,
                       report=F))
    karma<-summary(KARFIMA.fit(hum_train,p=i,d=F,q=j,info=T,
                               report=F))
    uwarma<-summary(UWARFIMA.fit(hum_train,p=i,d=F,q=j,info=T,rho=quant,
                               report=F))
    # ubxiiarma<-ubxiiarma.fit(ts(hum_test),ar=i,ma=i)
    barmax<-summary(BARFIMA.fit(hum_train,p=i,d=F,q=j,info=T,
                                xreg = X,
                                report=F))
    karmax<-summary(KARFIMA.fit(hum_train,p=i,d=F,q=j,info=T,
                                xreg = X,
                                report=F))
    uwarmax<-summary(UWARFIMA.fit(hum_train,p=i,d=F,q=j,info=T,rho=quant,
                                  xreg = X,
                                  report=F))
    order[cont,]<-c(i,j,barma$aic,karma$aic,uwarma$aic,
                    barmax$aic,karmax$aic,uwarmax$aic)
    cont<-cont+1
  }
}
print(order)

orbarma<-order[which(order[,3]==min(order[,3])),c(1:3)]
orkarma<-order[which(order[,4]==min(order[,4])),c(1:2,4)]
oruwarma<-order[which(order[,5]==min(order[,5])),c(1:2,5)]
orbarmax<-order[which(order[,6]==min(order[,6])),c(1:2,6)]
orkarmax<-order[which(order[,7]==min(order[,7])),c(1:2,7)]
oruwarmax<-order[which(order[,8]==min(order[,8])),c(1:2,8)]

barma<-BARFIMA.fit(hum_train,p=orbarma[1],d=F,q=orbarma[2],
                  info=T,report=F)
karma<-KARFIMA.fit(hum_train,p=orkarma[1],d=F,q=orkarma[2],
                  info=T,report=F)
uwarma<-UWARFIMA.fit(hum_train,p=oruwarma[1],d=F,q=oruwarma[2],rho=quant,
                     info=T,report=F)
# ubxiiarma<-ubxiiarma.fit(ts(hum_train),ar=i,ma=i)
barmax<-BARFIMA.fit(hum_train,p=orbarmax[1],d=F,q=orbarmax[2],
                    xreg=X,info=T,report=F)
karmax<-KARFIMA.fit(hum_train,p=orkarmax[1],d=F,q=orkarmax[2],
                    xreg=X,info=T,report=F)
uwarmax<-UWARFIMA.fit(hum_train,p=oruwarmax[1],d=F,q=oruwarmax[2],rho=quant,
                      xreg=X,info=T,report=F)

results_insample<-rbind(
               forecast::accuracy(barmax$fitted.values, hum_train),
               forecast::accuracy(karmax$fitted.values, hum_train),
               forecast::accuracy(uwarmax$fitted.values, hum_train),
               forecast::accuracy(a02$fitted, hum_train),
               forecast::accuracy(barma$fitted.values, hum_train),
               forecast::accuracy(karma$fitted.values, hum_train),
               forecast::accuracy(uwarma$fitted.values, hum_train),
               forecast::accuracy(a01$fitted, hum_train)
)[,c(3,2,5)]

row.names(results_insample)<-c("KARMAX","BARMAX","UWARIMAX","ARIMAX","KARMA",
                      "BARMA","UWARIMA","ARIMA")



# forecast::accuracy(hum_train,barma$fitted.values)
# 
# plot(hum_train,type="l")
# lines(fit1d$fitted[-1],col=2)
# lines(barma$fitted.values,col=3)
# lines(karma$fitted.values[-1],col=4)
# lines(urrarma$fitted, col=6)
# 
ubxiiarma<-ubxiiarma.fit(ts(hum_train),ar=1,ma=1,tau=.1)
forecast::accuracy(ubxiiarma$fitted[-c(1:3)], hum_train[-c(1:3)])

print(results)
