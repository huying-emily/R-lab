########################## Lecture 6 ################################

## A simulated ARCH(1) process
set.seed("8855")
n = 110
e = rnorm(n)
a = e
u = e
sig2= e^2
omega = 1
alpha = .95
phi = .8
mu = .1
for (i in 2:n)
{
  sig2[i+1] = omega + alpha * a[i]^2  #h(t+1)=a0+a1*sigmat^2
  a[i] = sqrt(sig2[i])*e[i]           #sigmat=(ht)^1/2*zt
  u[i] = mu + phi*(u[i-1]-mu) + a[i]  #yt=mu+phi(yt-1-mu)+sigmat
}

par(mfrow=c(2,4))
plot(e[11:n],type="l",xlab="t",ylab=expression(epsilon),main="(a) white noise") #zt
plot(sqrt(sig2[11:n]),type="l",xlab="t",ylab=expression(sigma[t]), 
     main="(b) conditional std dev") #(ht)^1/2 
plot(a[11:n],type="l",xlab="t",ylab="a",main="(c) ARCH") #sigmat=(ht)^1/2*zt
plot(u[11:n],type="l",xlab="t",ylab="u",main="(d) AR/ARCH") #yt=f(yt-1)+sigmat
acf(a[11:n],main="(e) ARCH")   #sigmat_estimated-ACF
acf(a[11:n]^2,main="(f) ARCH squared")  #sigmat^2_estimated-ACF
acf(u[11:n],main="(g) AR/ARCH")  #yt-ACF
acf(u[11:n]^2,main="(h) AR/ARCH squared")  #yt^2-ACF

## A simulated GARCH(1,1) process => ARIMA(p,d,q)/GARCH(p,q) Models
n = 200
#set.seed("8855")
e = rnorm(n)
a=e
u = e
sig2= e^2
omega = 1
alpha = .08
beta = 0.9
phi = .8
mu = .1
for (i in 2:n)
{
  sig2[i+1] = omega + alpha * a[i]^2 + beta*sig2[i]
  a[i] = sqrt(sig2[i])*e[i]
  u[i] = mu + phi*(u[i-1]-mu) + a[i]
}

par(mfrow=c(2,4))
plot(e[11:n],type="l",xlab="t",ylab=expression(epsilon),main="(a) white noise")
plot(sqrt(sig2[101:n]),type="l",xlab="t",ylab=expression(sigma[t]),
     main="(b) conditional std dev")
plot(a[101:n],type="l",xlab="t",ylab="a",main="(c) ARCH")
plot(u[101:n],type="l",xlab="t",ylab="u",main="(d) AR/GARCH")
acf(a[101:n],main="(e) GARCH")
acf(a[101:n]^2,main="(f) GARCH squared")
acf(u[101:n],main="(g) AR/GARCH")
acf(u[101:n]^2,main="(h) AR/GARCH squared")

## AR(1)/GARCH(1,1) fit to BMW returns
install.packages("fGarch")
install.packages("evir")
library(fGarch)
library(evir)
data(bmw,package="evir")

bmw.garch_norm = garchFit(~arma(1,0)+garch(1,1),data=bmw,cond.dist="norm") 
#garchFit() 建立AR()/GARCH()模型
options(digits=3)
summary(bmw.garch_norm) #using Coefficients can get The fitted model.
options(digits=10)

x = bmw.garch_norm@residuals / bmw.garch_norm@sigma.t #zt => standardized residual
n=length(bmw)
grid = (1:n)/(n+1)
library(MASS)
fitdistr(x,"t")
par(mfrow=c(1,2))
qqnorm(x,datax=T,ylab= "Standardized residual quantiles", main="(a) normal plot",
       xlab="normal quantiles")
qqline(x,datax=T)
qqplot(sort(x), qt(grid,df=4), main="(b) t plot, df=4",
       xlab= "Standardized residual quantiles",ylab="t-quantiles")
#QQ plot of standardized residuals from AR(1)/GARCH(1,1) fit to daily BMW logreturns
abline(lm(qt(c(.25,.75),df=4)~quantile(x,c(.25,.75))))
#the reference lines go through the first and third quanrtiles.

## ARMA(1)/GARCH(1,1) fit to BMW returns with Student-t distribution
bmw.garch_t = garchFit(~arma(1,1)+garch(1,1),cond.dist="std",data=bmw) #cond.dist="std"
options(digits=4)
summary(bmw.garch_t)

bmw.aparch_t = garchFit(~arma(1,0)+aparch(1,1),include.delta=T,
                        cond.dist="std",data=bmw)
summary(bmw.aparch_t)
bmw.aparch.std.res.t = bmw.aparch_t@residuals / bmw.aparch_t@sigma.t


###################### R-lab-NO.6 - 1
install.packages("Ecdat")
install.packages("tseries")
install.packages("fGarch")
data(Tbrate,package="Ecdat")
library(tseries)
library(fGarch)
# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
Tbill = Tbrate[,1]
Del.Tbill = diff(Tbill)
del.log.tbill = diff(log(Tbill))
Del.Tbill2 =Del.Tbill^2
par(mfrow=c(1,2))
plot(Tbill)
plot(Del.Tbill)

adf.test(Tbill)
kpss.test(Tbill)
adf.test(Del.Tbill)
kpss.test(Del.Tbill)

par(mfrow=c(2,2))
acf(Del.Tbill)
pacf(Del.Tbill)
acf(Del.Tbill2)
pacf(Del.Tbill2)
#图像可知选择 AR(1)/ARCH(1)
garch.model.Tbill = garchFit(formula= ~arma(1,0) + garch(1,0),Del.Tbill)
summary(garch.model.Tbill)
garch.model.Tbill@fit$matcoef #输出estimates of each of parameters in model
#判断AR(1)/ARCH(1)是否足够
res = residuals(garch.model.Tbill)
res_std = res / garch.model.Tbill@sigma.t
par(mfrow=c(2,3))
plot(res)
acf(res)
acf(res^2)
plot(res_std)
acf(res_std)
acf(res_std^2)

par(mfrow=c(1,1))
plot(abs(res_std))
lines(lowess(abs(res_std)))
#res_std from AR(1)/ARCH(1) fit to Del.Tbill still have heteroscedasticity

#用log(Del.Tbill)建立AR(1)/ARCH(1)
garch.model.log.Tbill = garchFit(formula= ~arma(1,0) + garch(1,0),del.log.tbill)
summary(garch.model.log.Tbill)
garch.model.log.Tbill@fit$matcoef
res = residuals(garch.model.log.Tbill)
res_std = res / garch.model.log.Tbill@sigma.t
par(mfrow=c(2,3))
plot(res)
acf(res)
acf(res^2)
plot(res_std)
acf(res_std)
acf(res_std^2)

#ARCH(1) not adequate =>建立AR(1)/ARCH(1,1)
garch.model.log.Tbill = garchFit(formula= ~arma(1,0) + garch(1,1),del.log.tbill)
summary(garch.model.log.Tbill)
garch.model.log.Tbill@fit$matcoef
res = residuals(garch.model.log.Tbill)
res_std = res / garch.model.log.Tbill@sigma.t
par(mfrow=c(2,3))
plot(res)
acf(res)
acf(res^2)
plot(res_std)
acf(res_std)
acf(res_std^2)

par(mfrow=c(1,1))
plot(abs(res_std))
lines(lowess(abs(res_std)))
#res_std from AR(1)/ARCH(1,1) fit to Del.log.Tbill donot have heteroscedasticity

#Here is the additional R code need to produce the plot. 
garch.model.Tbill_log_11 = garchFit(formula= ~arma(1,0)+ garch(1,1),del.log.tbill) 
summary(garch.model.Tbill_log_11)
res_log_11 = residuals(garch.model.Tbill_log_11) 
res_std_log_11 = res_log_11/ garch.model.Tbill_log_11@sigma.t 
par(mfrow=c(1,2))
plot (Tbill [-1] ,abs(res_std) ,xlab="Tbill" ,ylab="abs std residual", main="no log transform")
lines(lowess (Tbill [-1], abs (res_std)), col="red", h7d=3)
plot (Tbill [-1] , abs (res_std_log_11), xlab="Tbill", ylab="abs std residual", main="log
      transform")
lines(lowess(Tbill[-1] ,abs(res_std_log_11)),col="red",lwd=3)

###################### R-lab-NO.6 - 2
install.packages("Ecdat") 
install.packages("fGarch") 
library(Ecdat)
library(fGarch)
data(SP500,package="Ecdat")
returnBlMon = SP500$r500[1805]
x = SP500$r500[(1804-2*253+1):1804]
plot(c(x,returnBlMon))
results = garchFit(~arma(1,0)+garch(1,1),data=x,cond.dist="std")
dfhat = as.numeric(results@fit$par[6]) #results@fit$par[6] => shape
forecast = predict(results,n.ahead=1)

probBlackMonday = pstd(returnBlMon,mean=forecast$meanForecast,sd=forecast$standardDeviation,nu=dfhat)
#pstd()求概率
round(probBlackMonday,7)

std_res =results@residuals/results@sigma.t 
par(mfrow=c(1,3))
plot(std_res) 
acf(std_res) 
acf(std_res^2)

summary(results)
#比较AR(1)与AR(1)/ARCH()
fitAR1 = arima(x,order=c(1,0,0))
fitAR1 
par(mfrow=c(1,3))
residAR1 = residuals(fitAR1)
plot(residAR1) 
acf(residAR1) 
acf(residAR1^2)

