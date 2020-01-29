load.libraries <- c('plyr', 'dplyr','data.table', 'readxl', 'reshape2', 'stringr', 'stringi', 'ggplot2', 'tidyverse', 'gridExtra','matrixStats','lubridate','corrplot','e1071','xgboost','caret','zoo','factoextra','plotly','DT','forecast','fpp2','prophet','tictoc')
sapply(load.libraries, require, character = TRUE)
library(gtools)
library(MLmetrics)
library(gridExtra)
library(forecast)
remove(wtrain_sample)
train_sample <- setDT(train)[store == 2 & item == 1]
## Create a daily Date object - helps my work on dates
inds <- seq(as.Date(min(train_sample$date)), as.Date(max(train_sample$date)), by = "day")
## Create a time series object
set.seed(25)
myts<-ts(train_sample[,c(sales)],start = c(2013),end=c(2018),frequency = 365)
autoplot(myts)
ggseasonplot(myts,polar = T)
plot(decompose(myts))

##Weekly Sample
weeklyTS<-ts(train_sample[,c(sales)],start = c(2013),end=c(2018),frequency = 52)
ggseasonplot(weeklyTS)

##
ggAcf(myts)
plot(density(train_sample$sales))

####Forecasting
#Naive
Naive<-naive(myts,h=90)
autoplot(Naive)

#Simple Exponential Smoothing
expsm<-ses(myts,h=90)
autoplot(expsm)
checkresiduals(expsm)

####
train1 <- subset(myts, end = length(myts) - 365)
expsm<-ses(train1,h=365)
Naive<-naive(train1,h=365)

#Accuracy
accuracy(expsm,myts)
accuracy(Naive,myts)

##holts method
holt<-holt(myts,h=90)
autoplot(holt)

##winter's method
holtwinter <- hw(myts,seasonal = 'multiplicative')
autoplot(holtwinter)
holtwinter$fitted

###ETS

etsts<-ts(train1,start=c(2013),frequency = 1)
etsmodel<-ets(train1,model = 'AAA')
autoplot(etsmodel)

etsforecast<-forecast(etsmodel,h=365)
etsforecast$fitted

####stlfit
stlfit<-stlf(train1,method='ets')
stlfforecast<-forecast(stlfit,h=365)

str(stlfforecast)
accuracy(stlfforecast)

plot(stlfforecast)

PLOT<-autoplot(stlfforecast)
click(PLOT)

?stlf
summary(stlfit)

plot(decompose(myts))
myts %>% stl(s.window = 5) %>% autoplot


#Comparing
train_sample$index<-1:nrow(train_sample)
testcheck<-cbind(subset(train_sample,index>1461),stlfit$fitted)


###train sample probability
train_prob<-train_sample[,c('date','sales')]
train_prob<-subset(train_prob,date<as.Date('2017-01-01'))
train_prob$ratio<-train_prob$sales/mean(train_prob$sales)

train_prob
library(data.table)
r<-train_prob$sales
e<-data.table(train_prob$sales)
e[,Range := cut(r,breaks = c(5,10,15,20,25,30,35,40,45,50,55,60,65),labels=1:12)]

ratiots<-ts(e[,c('Range')],start = c(2013),end=c(2017),frequency = 365)
grid.arrange(autoplot(sfvf),autoplot(train1))
table(e$Range)

#Markov Chain Analysis Assuming the Markov Chain Property
library(depmixS4)
library(seqHMM)
library(Rsolnp)
library(markovchain)
xchar<-(e$Range)

homcx<-fitHigherOrder(xchar,6)

(homcx)

mcx<-markovchainFit(xchar)$estimate
plot(mcx,edge.arrow.size=0.1)
summary(mcx)
mcx

x <- e$Range
p <- matrix(nrow = 12, ncol = 12, 0)
for (t in 1:(length(x) - 1)) p[x[t], x[t + 1]] <- p[x[t], x[t + 1]] + 1
for (i in 1:12) p[i, ] <- p[i, ] / sum(p[i, ])
p


myseq<-e$Range
mat<-createSequenceMatrix(myseq)
tail(e$Range)
myfit<-markovchainListFit(data=myseq)
per<-(myfit$estimate)
str(per)
plot(per)
per
mat


grid.arrange(autoplot(train1),autoplot(train2))

library(xts)

weeklyall<-as.xts(train_sample,order.by = train_sample$date)
weeklyall <-as.data.table(apply.weekly(weeklyall,sum))
weekrr<-weeklyall$V1
library(data.table)
weeklyall[,Range := cut(weekrr,breaks = c(100,140,180,220,260,300,340))]

weeklyall

str(weeklyall)
rrrrr<-ts(weekrr,start = c(2013),end=c(2017),frequency = 52)

grid.arrange(autoplot(rrrrr),autoplot(decompose(rrrrr)))

x <-( weeklyall$Range)
mcxweekly<-markovchainFit(x)$estimate

plot(mcxweekly,edge.arrow.size=0.3)
summary(mcxweekly)
mcxweekly
steadyStates(mcxweekly)


aa<-createSequenceMatrix(x)
aa


p <- matrix(nrow = 6, ncol = 6, 0)
for (t in 1:(length(x) - 1)) p[x[t], x[t + 1]] <- p[x[t], x[t + 1]] + 1
for (i in 1:6) p[i, ] <- p[i, ] / sum(p[i, ])
p<-as.data.frame(p)
p

write.csv(p,'pq.csv')

####bayesian structural time series analysis
library(bsts)
ss<-AddLocalLinearTrend(list(),train1)
ss<-AddSeasonal(ss,train1,nseasons = 52,season.duration = 7)
bsts.model<-bsts(train1,state.specification = ss,niter=1000)


burn<-SuggestBurn(0.1,bsts.model)

pred<-predict.bsts(bsts.model,horizon = 366,burn=burn)


Predicted<-pred$mean
bsts.model$final.state
test<-window(myts,start=c(2017),end=c(2018))
residual<-Predicted-as.numeric(test)
residual
RMSE<-sqrt(mean(residual)^2)
RMSE

###stlf
accuracy(stlfforecast)
####store
weeks<-seq.Date(from=as.Date('2010-02-05'),to=as.Date('2012-10-26'),by='week')
storets<-ts(storeiitem1[,'Weekly_Sales'],start=c(2010,02,05),frequency = 49)


autoplot(storets)
plot(decompose(storets))
storetest<-subset(storets,end = length(storets)-50)

store_stlf<-stlf(storets,method='ets')
storeforecast<-forecast(store_stlf,h=50)
autoplot(storeforecast)+xlab('Time')+ylab('Sales')
accuracy(storeforecast)

ee<-storeforecast$mean-dd[94:143]

sqrt(mean(ee)^2)

autoplot(stlfforecast)+xlab('Time')+ylab('Sales')
plot(pred,plot.original=1461)

MAPE(storeforecast$mean,dd[94:143])
##store bsts


storetest
storets

ss<-AddLocalLinearTrend(list(),storetest)
ss<-AddSeasonal(ss,storetest,nseasons = 52)
bsts.model<-bsts(storetest,state.specification = ss,niter=1000)
bsts.model$original.series

burn<-SuggestBurn(0.1,bsts.model)

pred<-predict.bsts(bsts.model,horizon = 50,burn=burn)
plot(pred)
dd<-c(storeiitem1$Weekly_Sales)
Predicted<-pred$mean

residuals<-Predicted-dd[94:143]

sqrt(mean(residuals)^2)
MAPE(Predicted,dd[94:143])
