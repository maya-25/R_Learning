#probit and Logit models
#'logit model is the cdf of the logistic distribution
#'the probit model isthe cdf of standard normal distribution 
#'the predicted probability is 0 and 1 in both.
#'for the regression model, the predicted probability are not limited between 0
#'and 1.but for logit na dprobit model the predicted prob is limited between 0 and
#'1
insdata<-read.csv("D:/self_learning/probit_insurance.csv")
insdata
attach(insdata)
#define varibales
Y<-cbind(ins)
X<-cbind(retire,age,hstatusg,hhincome,educyear,married,hisp)
#descriptive statistics
summary(Y)
summary(X)


table(Y)
table(Y)/sum(table(Y))

#regression coeffiecients
olsreg<-lm(Y~X)
summary(olsreg)

#logit model coefficients
logit<-glm(Y~X,family = binomial(link = "logit"))
summary(logit)
#we can only interpret more or less likely have haelth insurance forge:
#if peopel is retire they have more likely to have health insurance

#logit model odds ratios
exp(logit$coefficients)
#if the number greater than 1 than they are more likely to have helath insurance than 
#number less than 1 by checking from the model odds ratio

#probit model
probit<-glm(Y~X,family = binomial(link = "probit"))
summary(probit)

#regression marginla effcets
coef(olsreg)

#logit model marginla effects
logitscaler<-mean(dlogis(predict(logit,type="link")))
logitscaler*coef(logit)

probitscaler<-mean(dnorm(predict(probit,type="link")))
probitscaler*coef(probit)
# now we can say by coeffiecients that the 41.9% retire peopel 
#are having the health insurance

#regression predicted probs
polsreg<-predict(olsreg)
summary(polsreg)
#logit model predicted probs
plogit<-predict(logit,type = "response")
summary(plogit)


#probit model predicted probs
pprobit<-predict(probit,type = "response")
summary(pprobit)

#percent corrctly predicted values
table(true=Y,pred=round(fitted(probit)))
table(true=Y,pred=round(fitted(logit)))


#mcfadden pseudo R squared
probit0<-update(probit,formula=Y~1)
Mcfadden<-1-as.vector(logLik(probit)/logLik(probit0))
Mcfadden


#multinomial probit and logit modles in R
library(mlogit)
mlogit_data<-read.csv("D:/self_learning/multinomial_fishing1.csv")
attach(mlogit_data)


#frequnecy of excah class/mode
table(mode)
#reshaping the data from wide to long format

mldata<-mlogit.data(mlogit_data,varying = 4:15,choice="mode",shape="wide")
mldata[1:20,]
mlogit.model1<-mlogit(mode~1|income,data=mldata,reflevel = "charter")
summary(mlogit.model1)
#thelikelihood the choicing beach is not much significant as compare to the 
#the likelihood of choosing pier is go down with the increase in income.and Theophlikelihood
#of choosing private is go up with the increase of income


mlogit.model2<-mlogit(mode~1|income,data=mldata,reflevel = "pier")
summary(mlogit.model2)
#beach,charter and pivate are become more likely

#multinomial logit model odds ratios
expmlogit<-exp(coef(mlogit.model1))
expmlogit
#odds ratio less than less likely..odds ratio more than one more likely


#conditonal logit model
clogit.model1<-mlogit(mode~price+catch|income,data=mldata,reflevel = "charter")
summary(clogit.model1)
#so here we can observing wrt the price and catch..here comes the condition..
#'so we the increase in price it is less likely to be choosen any type of fishing
#'and increse catchrate it is more likely to be choose 
clogit.model2<-mlogit(mode~price+catch|income,data=mldata,reflevel = "pier")
summary(clogit.model2)


#setting mean values for variables to use for marginal effects
m<-mlogit(mode~price+catch|income,data=mldata,reflevel = "charter")
z<-with(mldata,data.frame(price=tapply(price,index(m)$alt,mean),
                          catch=tapply(catch,index(m)$alt,mean),income=mean(income)))


#multinomial logit model marginal effexts
effects(mlogit.model1,covariate = "income",data=z)
#conditional logit model marginal effects
effects(clogit.model1,covariate = "income",data=Z)
effects(clogit.model1,covariate = "price",data=z)
effects(clogit.model1,covariate = "catch",data=z)
#if the price increases the prob of charter is less likely
#if the catch rate incraeses theprob of charter is 
#morelikely as comapre to other modes
#hausman mcfadden test fo rmeasuring this
