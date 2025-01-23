######################
## Data preparation ##
######################
library(tidyverse)

data <- read_delim("combined_hourly_data.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)  |>  
  mutate(timestamp=as.POSIXct(timestamp, tz="GMT",
                              origin="1970-01-01 00:00:00"),
         hum=hum/100)
#

View(data)
n<-round(dim(data)[1]*.8)-149
data<-data[150:1870,]

#########################
## Descriptive analysis ##
#########################
length(table(strftime(as.POSIXct(data$timestamp, tz="GMT",
                                 origin="1970-01-01 00:00:00"),
                      format="%Y%m%d", tz="GMT"))) # number of days

summary1<-apply(data[,c(3,6:13)],2,summary)
sums<-as.matrix(bind_rows(summary1))
rownames(sums)<-colnames(data[,c(3,6:13)])
sums[is.na(sums)]<-0
descriptive<-(cbind(sums[,c(1,3,4,6)],
                    SD=apply(na.omit(data[,c(3,6:13)]),2,sd),
                    CV=apply(na.omit(data[,c(3,6:13)]),2,sd)/
                      abs(apply(na.omit(data[,c(3,6:13)]),2,mean))*100,
                    Skew=apply(na.omit(data[,c(3,6:13)]),2,moments::skewness),
                    `NA`=sums[,7]))
xtable::xtable(descriptive,digits=2)

###########
## plots ##
###########
library(xts)
RH <- xts(data$hum, order.by=data$timestamp)

RHplot<-{plot(RH, main="",
              yaxis.right=FALSE, grid.col = "white",
              format.labels="%b-%Y", main.timespan = FALSE,
              lwd=0.5,cex.lab=1.3,cex.axis=1.3,
              ylim=c(0.4,.9))
}

w1<-4.5 # width for plots 
h11<-4.5 # height for plots
setEPS()
postscript("RV.eps",width = w1, height = h11,family = "Times")
RHplot
dev.off()

postscript("RV_hist.eps",width = w1, height = h11,family = "Times")
hist(RH,main="",xlab = "",freq = T)
dev.off()
