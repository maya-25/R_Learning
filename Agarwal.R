#*********************Assigment 1 Financial econometrics*************
#*********************Mayank Agarwal****************************
#*********************Matrikulation No.1513928***********************************


#preamble
if (!("rstudioapi" %in% installed.packages())) {install.packages("rstudioapi")}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))#set the working directory to the current R Script location 
y = function(){dev.new();x=par(no.readonly=T); dev.off(); x} #'
par(y());options(scipen=0);dev.off();rm(list=ls())

#download the weekly dataest from quandl by hitting the API

Quandl.api_key("###########")

#loading the data downloaded from Quandl by import dataset on right.
#*************************Question 1*************************
data<-Quandl("LBMA/GOLD", collapse="weekly",type="xts")
class(data)
save_data<-data
#import the required libraries
library(Quandl)
library(tsibble)
library(ggplot2)
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(dplyr)
library(tibbletime)
library(urca)
library(forecast)
library(fable)
library("feasts") # feature extraction and statistics
library("gridExtra")

#********************************Question 2***************************
#conventional data frame 
#data1<-data.frame(date=index(data), coredata(data))
#data1
#multiplying by 100 for daily % change and for good number for logreturns
  data<-data[,c(2)]
  data<-na.omit(data)
  day=as.Date(rownames(data.frame(data)))
  price=data$`USD (PM)`
  log_return=c(100*diff(log(data$`USD (PM)`),lag=5)) #considering lag=5 for weekly data
data2<-data.frame(day,price,log_return)
colnames(data2)<-c("day","price","log_return")
data2
#**************************Qusetion 3********************************

#plot(ts(data$price,start = data$day,frequency = 12))
plot1<-ggplot(data2,aes(x=day,y=price))+
  geom_line(color="steelblue")+
  xlab("year")+
  ylab("price")+
  theme_bw()+
  scale_x_date(date_breaks = "4 year",date_labels="%Y")
plot1  #in levels

plot2<-ggplot(data2,aes(x=day,y=log_return))+
  geom_line(color="steelblue")+
  xlab("year")+
  ylab("price")+
  theme_bw()+
  scale_x_date(date_breaks = "4 year",date_labels="%Y")
plot2 #in returns 

#test for normality

logreturn<-na.remove(data2[,"log_return"])
summary(logreturn)

#skewness
skewness(data2$log_return) #right skewed or positively skewed
skewness(data2$price)#also positively skewed
kurtosis(data2$log_return)
kurtosis(data2$price)
jarque.bera.test(na.remove(data2[,2])) #for price
jarque.bera.test(na.remove(data2[,3]))#for log_return
#we are looking for normality 
#or stochastic independent of residuals.So, the null hypothesis is normality
#and is rejected..

#Histogram
h1<-hist(data2$log_return, breaks = 120,col="green",
        xlab="log_Return",main="Histogram of LBMA/GOLD returns",
        xlim=c(-10,10)) #plotted by default

h2<-hist(data2$price, breaks = 120,col="blue",
        xlab="price",main="Histogram of LBMA/GOLD returns",
        xlim=c(-10,500)) #plotted by default


#*********************************Question4*************************
#Locate the dates when both variables have had the highest and the lowest values.

data2$day[[which.max(data2$price)]]
data2$day[[which.min(data2$price)]]
data2$day[[which.max(data2$log_return)]]
data2$day[[which.min(data2$log_return)]]

#******************Qusetion 5*********************************
#Weekly forecasting is quite inadequate due to the deterministic effect 
#of holidays and other events. Weekly data can be severely skewed by 
#when the holiday occurs and activity before and after the holiday
#So, convert the weekly time series into monthly

#but we should be cautious as the accuracy or correctness of your forecast may 
#suffer if months data does not represent the properties of your time series accurately

data<-mutate(data2,day=yearmonth(day))
data_month<-data%>% 
  group_by(day)%>%
  summarise_all(mean)
data_month

typeof(data_month)
class(data_month)
#convert it into modern time series using tsibble
data_timeseries<-as_tsibble(data_month,index = day)

data_timeseries
class(data_timeseries)
#finally  getting rid of the missing value in a dataset
data_timeseries<-na.omit(data_timeseries)
sum(is.na(data_timeseries))

#************************Question 6*******************************
#plot the autocorrelation for log_returns

ACF1<-acf(as.numeric(data_timeseries$log_return),lag.max = 300)
plot(ACF1[1:300],main="") #ACF of log returns

ACF2<-acf(abs(as.numeric(data_timeseries$log_return)),lag.max = 300)
plot(ACF2[1:300],main="") #ACF of absolute log returns 
#ACF is merely a bar chart of the coefficients of correlation 
#between a time series and lags of itself.
# It seems to be an ARMA process.
# An AR process by slowly decaying ACF
# A MA process sS PACF decays more slowly
PACF<-pacf(as.numeric(data_timeseries$log_return),lag.max = 300)
plot(PACF[1:300],main="")


#*****************************Question7******************************
#check the varibale for existence of trend
#performing the dickey fuller test for the existence of a trend
summary(ur.df(data_timeseries$log_return,selectlags="AIC",type="trend"))
#as we can clearly see the trend is not significant as 5% and t-stat is 
#in the rejection area.
#data is stationary/there is no unit root/stochastic trend is not present.
summary(ur.df(data_timeseries$log_return,selectlags="BIC",type="trend"))



#unit root test in log-differences without a trend with a constant
summary(ur.df(data_timeseries$log_return,selectlags="BIC",type="drift"))

#unit root test in log-differences without a trend or a constant
summary(ur.df(data_timeseries$log_return,selectlags="BIC",type="none"))
#reject HO; stationary time series

#****************************Question 8*****************************
#here we are doing analysis bu ARMA(0,0)-ARMA(4,4) with d=0

#as there is no trend present so thats why we are taking no I(0) means difference 
#d=0 for example ARIMA(0,0,0)-ARIMA(4,0,4)

#model choice

modelchoice<-NULL
for(i in 0:4){
  for(j in 0:4){
    modelchoice<-rbind(modelchoice,c(i,j,AIC(arima(data_timeseries[,"log_return"],
                                                   order=c(i,0,j))),
                                     BIC(arima(data_timeseries[,"log_return"],
                                               order =c(i,0,j)))))
  }
}
colnames(modelchoice)<-c("p","q","AIC","BIC")  
class(modelchoice)

#glance(modelchoice)
which.min(modelchoice[,"BIC"])
which.min(modelchoice[,"AIC"])
#so by both the ways model ARIMA(0,0,2) and ARIMA(3,0,4) should be selected 

#Most specifically we have to choose ARIMA(3,0,4)according to AIC as given 
#question

class(data_timeseries)
#alternate method for finding the model
date_forecasting<-data_timeseries%>%
  model(
    arma00 = ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(log_return ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(log_return ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30= ARIMA(log_return ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma40 = ARIMA(log_return ~ 1 + pdq(4, 0, 0) + PDQ(0, 0, 0)),
    arma02 = ARIMA(log_return ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(log_return ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma23 = ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma34= ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0)),
    arma42 = ARIMA(log_return ~ 1 + pdq(4, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(log_return ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(log_return ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33= ARIMA(log_return ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0)),
    arma43 = ARIMA(log_return ~ 1 + pdq(4, 0, 3) + PDQ(0, 0, 0)),
    arma04 = ARIMA(log_return ~ 1 + pdq(0, 0, 4) + PDQ(0, 0, 0)),
    arma14 = ARIMA(log_return ~ 1 + pdq(1, 0, 4) + PDQ(0, 0, 0)),
    arma24 = ARIMA(log_return ~ 1 + pdq(2, 0, 4) + PDQ(0, 0, 0)),
    arma34= ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0)),
    arma44 = ARIMA(log_return ~ 1 + pdq(4, 0, 4) + PDQ(0, 0, 0)),
    
  ) 


glance(date_forecasting) # summary
glance(date_forecasting)[["BIC"]] # show all BIC values
glance(date_forecasting)[which.min(glance(date_forecasting)[["BIC"]]), ] 
glance(date_forecasting)[which.min(glance(date_forecasting)[["AIC"]]), ] 

#***************************Qusetion 9***************************

#Since p value is greater than 5% significance level.
#So, FTR-> no autocorrelation -> we can stick to this model..
a <- date_forecasting %>%
  residuals() %>% # get the residuals from arma23
  group_by(.model) %>% # groups the residuals by model
  features(features = ljung_box, lag = 20)
a[14,] #for ARMA(3,4) model

#**************************Question 10****************************
#Forecast the log-return for the period of May 2020-May 2021 using ARMA(2,3)
# filtering the date for the model before may 2020 
dates<-data_timeseries%>%
  filter(day>=yearmonth("1968 May"))%>%
  filter(day<=yearmonth("2020 Apr"))
dates
forecast34<- dates %>%
  model(ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0))) %>%
  forecast(h = 12, level = 95) # future prediction of a TS from the fitted model, 12periods
#representing 12 months

accuracy(forecast34, data_timeseries)

forecasting_naive <- dates %>%
  model(NAIVE(log_return)) %>%
  forecast(h = 12)

accuracy(forecasting_naive, data_timeseries)

p1 <- autoplot(forecast34, slice(data_timeseries, (n() - 20):n())) + #autoplot wrapper
  xlab("") + ylab("ARIMA(3,0,4)") 
p1
p2 <- autoplot(forecasting_naive, slice(data_timeseries, (n() - 20):n())) + # "slice() the tail()"
  xlab("") + ylab("Naive")
p2
p <- grid.arrange(p1, p2, ncol = 1, nrow = 2) # combine multiple plots in one
plot(p)

#In ARIMA(3,0,4) forecast plot relisations has possible dynamics and 
#naive has straight line which does not indicate much information.We can 
#also check the values from descriptive statistics for example: MAE, RMSE



##########################2 Question#####################################

#********************Mean Estimation using ML*************************
#generating the random numbers
set.seed(1)    # for reproducible example
random_data<-rnorm(5000,mean=5,sd=1)
random_data
# plotting the gernerated random number series fillowing gaussian distribution
data.frame(x=random_data) %>%
  ggplot(aes(x=random_data))+ 
  geom_histogram(bins=30, color="blue",fill="dodgerblue") + 
  theme_bw(base_size = 16) + 
  xlab("Data")
#Create a function loglik normal that returns the negative sum of the 
#log-pdf 
#Gaussian normal distribution given the inputs x (data vector), mu (mean) and sigma
loglik_normal<-function(x,mu,sigma){
  -sum(dnorm(x,mu,sigma,log=T))
}
#Create a vector mean hat in the range of [0; 10]
#using a distance of 1e-2.
mean_hat<-seq(from = 0, to = 10, by = 0.01)
loglik_normal(random_data,mean_hat,2)

output<-sapply(mean_hat,loglik_normal)

#*********************************END************************************
