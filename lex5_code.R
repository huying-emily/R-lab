########################## Lecture 5 ################################

## Unit root test
data(Mishkin,package="Ecdat")
library(forecast)
library(tseries)
x= as.vector(Mishkin[,1]) 
par(mfrow=c(2,2))
plot(x,main="Inflation rate")
plot(diff(x),main="Difference")
acf(x,main="Inflation rate")
acf(diff(x),main="Difference")
adf.test(x)    # Augmented Dickey-Fuller Test, H0: not stationary =>reject
pp.test(x)     # Phillips-Perron Unit Root Test, H0: not stationary =>reject
kpss.test(x)   # KPSS Test for Level Stationarity, H0:stationary =>reject
#The unit root tests are contradictory
dx = diff(x)
adf.test(dx)  # Augmented Dickey-Fuller Test, H0: not stationary =>reject
pp.test(dx)   # Phillips-Perron Unit Root Test, H0: not stationary =>reject
kpss.test(dx) # KPSS Test for Level Stationarity, H0:stationary =>not reject
#However the 3 tests confirm stationarity of the change of inflation rate.

auto.arima(x) #ARIMA(1,1,1)
auto.arima(x,d=0,ic="bic",stationary=T) #ARIMA(2,0,1) with non-zero mean
fitauto0 = arima(x,order=c(2,0,1))
polyroot( c(1,-fitauto0$coef[1:2]))

## Forecasting Inflation rates
data(Mishkin,package="Ecdat")
library("forecast")
infl = as.vector(Mishkin[,1])
year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)
logn=log(n)

fit=arima(infl,c(0,1,3))
pred.infl = predict(fit, n.ahead = 100, se.fit = TRUE) #predict()进行预测,n.ahead表示预测数量
t1 = 300:491
t2 = 492:(492+49+50)
year = seq(1950 + 1/12,2001+61/12,1/12)
plot(year[t1],infl[t1],ylim=c(-10,18),type="b",xlim=c(1975,1999),
     xlab="year",ylab="Inflation rate",cex.axis=1.15,cex.lab=1.15)
points(year[t2], pred.infl$pred,type="p",pch="*")
lines(year[t2], pred.infl$pred - 2*pred.infl$se)
lines(year[t2], pred.infl$pred + 2*pred.infl$se)
legend(1975,-3,c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=0,pch=c("o","*",NA,NA),lty=c(NA,NA,1,1))

fit_diff=arima(diff(infl),c(0,0,3))
pred.infl_diff = predict(fit_diff, n.ahead = 100, newxreg = NULL,se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+49+50)
plot(year[t1],diff(infl)[t1],xlim=c(1975,1999),ylim=c(-9,15),type="b",
     xlab="year",ylab="Change in inflation rate",cex.axis=1.5,cex.lab=1.5)
points(year[t2], pred.infl_diff$pred,type="p",pch="*")
lines(year[t2], pred.infl_diff$pred - 2*pred.infl_diff$se)
lines(year[t2], pred.infl_diff$pred + 2*pred.infl_diff$se)
legend(1975,14,c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=0,pch=c("o","*",NA,NA),lty=c(NA,NA,1,1))

## Forecast limits of the changes of Inflation rates
data(Mishkin,package="Ecdat")
infl = as.vector(Mishkin[,1])
year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)
logn=log(n)
fit_diff=arima(diff(infl),c(0,0,3))
pred.infl_diff =predict(fit_diff, n.ahead = 100, newxreg = NULL,se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+49+50)
resid = fit_diff$resid[488:490]
coeff = as.vector(fit_diff$coef[1:3])
mu = as.vector(fit_diff$coef[4])
niter = 50000
n.ahead =30
futureobs = matrix(0,nrow=niter,ncol=n.ahead)
future_int = futureobs

set.seed(1234576)
for (i in 1:niter)
{
  errors = sample(fit_diff$resid, n.ahead, replace = TRUE)
  #  errors = rnorm(30,mean=0,sd = sqrt(fit_diff$sigma2))
  errors = c(resid,errors)
  for (j in 1:n.ahead)
  {
    futureobs[i,j] = mu + errors[j+3] + errors[j+2]*coeff[1]+ 
      errors[j+1]*coeff[2] + errors[j]*coeff[3]
    if (j > 1)
    {
      future_int[i,j] = future_int[i,j-1] + futureobs[i,j]
    }
    if (j==1){future_int[i,j] = futureobs[i,j]
    }
  }
}
future_mean = apply(futureobs,2,mean)
ul = 0*(1:n.ahead)
ll =ul
for (k in 1:n.ahead)
{
  ul[k] = quantile(futureobs[,k],.975)
  ll[k] = quantile(futureobs[,k],.025)
}

postscript("inflation_forecasts_sim.ps")   #  Figure 9.21
plot(1:n.ahead,ul,ylim=c(-10,10),type="b",lwd=2,xlab="month ahead",
     ylab="rate",cex.axis=1.5,cex.lab=1.5)
lines(ll,type="b",lwd=2)
lines(1:n.ahead, pred.infl_diff$pred[1:n.ahead] - 
        1.96*pred.infl_diff$se[1:n.ahead],type="b",lty=3)
lines(1:n.ahead, pred.infl_diff$pred[1:n.ahead] + 
        1.96*pred.infl_diff$se[1:n.ahead],type="b",lty=3)
lines(1:n.ahead, future_mean,lwd=2,lty=2)
graphics.off()

## Seasonal differencing for housing starts
data(Hstarts,package="Ecdat")
hst = Hstarts[,1]
x =hst
t = seq(1960.25,2002,.25)
year =t
n = length(t)
fit = arima(hst,c(1,1,1),seasonal = list(order = c(1,1,1), period = 4))
fit2 = arima(hst,c(1,1,1),seasonal = list(order = c(0,1,1), period = 4))
#fit2 => ARIMA{(1,1,1) × (0,1,1)4} model
#Coefficients:
#   ar1         ma1        sma1
#0.6747792  -0.8900838  -0.8220113
#the fitted model: (1-0.675B)Yt*=(1-0.890B)(1-0.822B4)sigama-t
pred =predict(fit2, n.ahead = 16, newxreg = NULL, se.fit = TRUE)
t1 = 130:168
t2 = 169:(169+15)
x = as.matrix(x)

par(mfrow=c(1,3))
plot(t,x,ylab="log(starts)",type="b",xlab="year",main="(a)")
acf(x,main="(b)",xlab="lag")
quart = rep(1,42) %x% (1:4)
boxplot(x~quart,xlab="quarter",ylab="log(starts)",main="(c)")

par(mfrow=c(3,2))
plot(year[2:n],diff(x),xlab="year",type="b", main="(a) nonseasonal differencing")
acf(diff(x),main="(b) nonseasonal differencing",xlab="lag")
plot(year[5:n],diff(x,4),type="b",xlab="year", main="(c) seasonal differencing")
acf(diff(x,4),main="(d) seasonal differencing",xlab="lag")
plot(year[6:n], diff(diff(x,1),4),type="b",xlab="year", main="(e) seasonal & nonseasonal differencing")
acf( diff(diff(x,1),4),main="(f) seasonal & nonseasonal differencing", xlab="lag")


###################### R-lab-NO.5
install.packages("Ecdat")
data(Tbrate,package="Ecdat")
install.packages("tseries")
library(tseries)
# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
plot(Tbrate)  # times series plots
acf(Tbrate)  # ACF plots
adf.test(Tbrate[,1]) # Augmented Dickey-Fuller tests
adf.test(Tbrate[,2])
adf.test(Tbrate[,3])

diff_rate = diff(Tbrate)
adf.test(diff_rate[,1])
adf.test(diff_rate[,2])
adf.test(diff_rate[,3])
pairs(diff_rate) # scatterplot matrix---shows cross-sectional associations
plot(diff_rate) # time series plots---helpful when deciding whether differencing 
# once is enough to induce stationarity.
acf(diff_rate) # auto- and cross-correlations

par(mfrow=c(1,1))
boxplot(diff_rate[,1] ~ cycle(diff_rate)) # cycle() extracts seasonal information
#Quarterly time series=>if mean depends on quarter=>compare boxplots of four quarters.

install.packages("forecast")
library(forecast)
auto.arima(Tbrate[,1],max.P=0,max.Q=0,ic="aic") #注意是大写的P/Q
auto.arima(Tbrate[,1],max.P=0,max.Q=0,ic="bic") 
# max.P=0 and max.Q=0 force the model to be nonseasonal
# max.P and max.Q are the number of seasonal AR and MA components.

fit1 = arima(Tbrate[,1],order=c(0,1,1)) #根据上一步的结果可知用c(0,1,1)
acf(residuals(fit1))  # residual autocorrelation
Box.test(residuals(fit1), lag = 10, type="Ljung") #Box-Ljung test:H0:pho1=...=phom=0

resid2 = residuals(fit1)^2 
#GARCH effects can be detected autocorrelation in mean-centered squared residuals.
#autocorrelations in squared residuals are strong => GARCH effects is noticeable.
Box.test(resid2, lag = 10, type="Ljung") #Box-Ljung test:H0:no autocorrelation.

data(Tbrate,package="Ecdat")
# r = the 91-day Treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
# fit the nonseasonal ARIMA model found by auto.arima
attach(as.list(Tbrate))
auto.arima(pi,max.P=0,max.Q=0,ic="bic")
fit = arima(pi,order=c(0,1,1))
forecasts = predict(fit,36)
plot(pi,xlim=c(1980,2006),ylim=c(-7,12))
lines(seq(from=1997,by=.25,length=36), forecasts$pred,col="red")
lines(seq(from=1997,by=.25,length=36), forecasts$pred+1.96*forecasts$se,col="blue")
lines(seq(from=1997,by=.25,length=36), forecasts$pred-1.96*forecasts$se,col="blue")


