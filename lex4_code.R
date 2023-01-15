########################## Lecture 4 ################################

## Stationary 
data(Mishkin,package="Ecdat")
x= as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
#  (in percent, annual rate) 
year = seq(1950 + 1/12,1990+11/12,1/12) #seq(from, to, by = )
n=length(year)
logn=log(n)
#Time series plots
postscript("inflation.ps",width=7,height=6)         # Fig 9.1
par(mfrow=c(2,1))
plot(year,x,ylab="inflation rate",type="l",xlab="year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3,main="(a)")
plot(year[2:n],diff(x),ylab="change in rate",type="l",xlab="year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.2,main="(b)")
graphics.off()
#seasonal trend
data(AirPassengers)
x = as.matrix(AirPassengers)
n = length(x)
year = 1949 + (0:(n-1))/12
postscript("airpass.ps")               #    Figure 9.2
plot(year,x,type="b",ylab="passengers",cex.axis=1.5,cex.lab=1.5,cex=1.5,
     lwd=2)
graphics.off()

## ACF(sample autocorrelation function)
data(Mishkin,package="Ecdat")
x= as.vector(Mishkin[,1])  #pai1=one-month inflation rate #(in percent,annual rate) 
year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)
logn=log(n)

postscript("inflation_acf.ps",height=3.5,width=6)     #ACF plots 
par(mfrow=c(1,2))
acf(x,cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="Inflation rate")
acf(diff(x),cex.axis=1.5,cex.lab=1.5,cex.main=1.2,
    main="Change in inflation rate")
graphics.off()
#Box-Pierce test
Box.test(diff(x),lag=10) #Box-Pierce test

## Model Estimation(MLE,LSE,Yule-Walker)
data(Mishkin,package="Ecdat")
x= as.vector(Mishkin[,1]) 
year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)

library(forecast) #每次用auto.arima()之前都需要载入forcast包
#构建ARIMA模型,利用R里面的auto.arima()函数,自动筛选p\d\q
auto.arima(diff(x),max.p=20,max.q=0,ic="aic") 
auto.arima(diff(x),max.p=20,max.q=0,ic="bic")

data(bmw,package="evir")
bmw = as.vector(bmw)
n=length(bmw)
#建立ARIMA(1,0,0)模型
arima(bmw, order = c(1,0, 0)) #arima(data,order=c(p,d,q),method=.)
fitAR1 = arima(bmw, order = c(1,0, 0)) 
Box.test(fitAR1$resid,lag=2,fitdf=1) #Box-Pierce test
Box.test(fitAR1$resid,lag=24,fitdf=1,type=("Ljung")) #Box-Ljung test 

postscript("BMW_resid_acf.ps",height=6,width=6)  #   Figure 9.7
par(mfrow=c(2,2),cex.axis=1.15,cex.lab=1.15)
acf(residuals(fitAR1),lag.max=20 , main="")
qqnorm(residuals(fitAR1),datax=T,main="AR(1) resid")
plot(residuals(fitAR1),ylab="Residual")
graphics.off()

## Inflation rate - Fitting AR/MA models using AIC/BIC
data(Mishkin,package="Ecdat")
x= as.vector(Mishkin[,1]) 
year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)
fit = arima(x,c(1,0,0))
Box.test(fit$resid,lag=24,fitdf=1,type=("Ljung")) #lag=24,type=("Ljung")

postscript("inflation_AR1_acf.ps",width=9,height=4.75) # Fig 9.8
par(mfrow=c(1,2))
acf(x,main="Inflation rate")
acf(fit$resid,main="Residuals from AR(1)")
graphics.off()

# Fitting MA models using AIC/BIC
logn=log(n)
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(diff(x),order=c(0,0,i))
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (logn-2)*i
  resultsdiff[i,1]=i
}
postscript("inflation_diff_mafits.ps")         # Fig 9.10
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion",
     cex.lab=1.35,cex.axis=1.35,
     main="MA fits to changes in inflation rate",
     cex.main=1.35,cex=2,pch="*",ylim=c(2440,2560))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(12,2565,c("AIC","BIC"),pch=c("*","o"),cex=2,box.lty=0)
graphics.off()
#in R's forecast package found that q = 3 is the first local minimum of AIC,
#while the first local minimum of BIC is at q= 2.
#Thus, if an MA model is used, then only 2 or 3 MA parameters are needed. 
#This is a strong contrast with AR models, which require far more parameters.
auto.arima(diff(x),max.p=0,max.q=10,ic="aic") 
auto.arima(diff(x),max.p=0,max.q=10,ic="bic")

arima(diff(x),order=c(0,0,3))

# Fitting AR models using AIC/BIC
logn=log(n)
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
  fit = arima(diff(x),order=c(i,0,0))
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (logn-2)*i
  resultsdiff[i,1]=i
}
postscript("inflation_diff_arfits.ps")         # Fig 9.10
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion",
     cex.lab=1.35,cex.axis=1.35,
     main="AR fits to changes in inflation rate",
     cex.main=1.35,cex=2,pch="*",ylim=c(2440,2560))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(12,2565,c("AIC","BIC"),pch=c("*","o"),cex=2,box.lty=0)
graphics.off()
#in R's forecast package found that p = 8 is the first local minimum of AIC
#while the first local minimum of BIC is at p = 6.
auto.arima(diff(x),max.p=10,max.q=0,ic="aic") 
auto.arima(diff(x),max.p=10,max.q=0,ic="bic")

arima(diff(x),order=c(6,0,0))
fit=arima(x,order=c(7,0,0))
pacf(x)
acf(residuals(fit)) #ACF of resid from AR(7) fit to the inflation rates


