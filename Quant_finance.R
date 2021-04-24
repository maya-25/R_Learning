library(quantmod)
library(PerformanceAnalytics)
dt <-"2017-2-1"
aapl<-getSymbols.yahoo("AAPL",from = dt, auto.assign=F)
aapl
AAPL_close<-getSymbols.yahoo("AAPL",from = dt,auto.assign=F)[,6]
AAPL_close
AAPL_return<-na.omit(dailyReturn(AAPL_close,type="log"))
AAPL_return
chartSeries(aapl)
#portfolio analysis--------------------
tickers<-c("FB","AAPL","AMZN","NFLX")
weights<-c(.25,.25,.25,.25)
port_prices<-NULL
for (ticker in tickers){
  portfolioprices<-cbind(port_prices,getSymbols.yahoo(ticker, from='2016-01-03',periodicity = 'daily',
                                                      auto.assign=FALSE)[,4])
}
portfolioreturn<-na.omit(ROC(benchmarkprices))
colSums(is.na(portfolioprices))
benchmarkprices<-getSymbols.yahoo( '^GSPC',from='2016-01-03',periodicity = 'daily',
                                  auto.assign=FALSE)[,4]
benchmarkreturn<-na.omit(ROC(benchmarkprices)) #rate of change
colSums(is.na(benchmarkprices))

portfolioReturn<-Return.portfolio(portfolioreturn)
CAPM.beta(portfolioReturn,benchmarkreturn,.035/252)
CAPM.jensenAlpha(portfolioReturn,benchmarkreturn,.035/252)
SharpeRatio(portfolioReturn,.035/252)
table.AnnualizedReturns(portfolioReturn)
table.CalendarReturns(portfolioReturn)

colnames(aapl)<-c("open","High","Low","Close","Volume","Adjusted")
head(aapl)
aapl[,1]
aapl[,c("Low","Volume")]  #Ad and Cl are under quantmod
plot(Cl(aapl),type='l')
plot(Ad(aapl),type='l')
aapl[,"Close"]
class(aapl) #especially when dealing with time series data..

plot(dailyReturn(Ad(aapl),type='arithmetic'),type='l') #returns the daily return
plot(cumprod(1+dailyReturn(Ad(aapl),type='arithmetic')))
plot(diff(log(Cl(aapl))),type='l')
head(aapl[,c("Close","Adjusted")])
diff(log(aapl[,c("Close","Adjusted")])) #to calculate the log returns
apply(aapl[,c("Close","Adjusted")],2,log)
apply(apply(aapl[,c("Close","Adjusted")],2,log),2,diff)
#'log rreturns continuous returns =(1+simple returns )
#'continuos return diff(log(data))
compare_ad_Cl<-data.frame(apply(apply(aapl[,c("Close","Adjusted")],2,log),2,diff))
compare_ad_Cl
plot(compare_ad_Cl[,1],type='l')

cumsum_aapl<-data.frame(apply(compare_ad_Cl,2,cumsum))
plot(cumsum_aapl[,1],type='l')
plot(cumsum_aapl[,2],type='l',col='red')

plot(cumprod(1+dailyReturn(Ad(aapl)),type="arithmetic"))
lines(cumsum(dailyReturn(Ad(aapl)),type="log"),col="red")

library(PerformanceAnalytics) #uses xts and zoo objects..
class(compare_ad_Cl)
class(as.xts(compare_ad_Cl))
data<-as.xts(compare_ad_Cl)
data<-exp(data)-1
charts.PerformanceSummary(data,main="Compare Close to Adjusted cumulative Discrete Return")


#tesla
tsla<-dailyReturn(Ad(getSymbols("TSLA",auto.assign = FALSE)))
charts.PerformanceSummary(tsla,main="Compare Close to Adjusted cumulative Discrete Return for TSLA")

#Microsoft
msft<-dailyReturn(Ad(getSymbols("MSFT",auto.assign = FALSE)))
charts.PerformanceSummary(msft,main="Compare Close to Adjusted cumulative Discrete Return for MSFT")


X<-merge(tsla,msft)
dim(X)
head(X)
tail(X)

Y<-merge(tsla,msft,all = FALSE)
head(Y)
dim(Y)
charts.PerformanceSummary(Y) #mainting the risk and return 
#to measure the risk free return we use sharpe ratio..measue of risj-adjusted return
#higher sharpe ratio better
#252 days of trading
table.AnnualizedReturns(Y,scale=252,Rf=0.005/252) #risk free rate 5%
#standard deviation is  a measure of risk that is used to measure sharpe ratio