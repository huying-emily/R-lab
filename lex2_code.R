########################## Lecture 2 ################################

##  Weekly_Interest_Rate
dat = read.table(file="WeekInt.txt",header=TRUE)
attach(dat)
aaa_dif = diff(aaa)  #diff()计算相邻两项之差
cm10_dif = diff(cm10) #rate和yield需要用diff()计算
#postscript("cm10aaa.ps",width=6,height=5)  #  Figure 12.2
plot(cm10_dif,aaa_dif,xlab="change in 10YR T rate",
     ylab="change in AAA rate")
#graphics.off()
fit1 = lm(aaa_dif ~ cm10_dif)
summary(fit1)

## Multiple_Linear_Regression
dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff(cm10)
aaa_dif = diff(aaa)
cm30_dif = diff(cm30)
ff_dif = diff(ff)
fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)

summary(fit2)          #  Example 12.4
anova(fit2)            #  Section 12.4.1
summary(fit3)          #  Example 12.4
anova(fit3)            #  Section 12.4.1
postscript("weekint_splotm.ps",width=6,height=5)     #  Figure 12.4
plot(as.data.frame(cbind(aaa_dif,cm10_dif,cm30_dif,ff_dif))) #scatterplot matrix of four time series
graphics.off() #输出图像为ps文件

#model selection: H0: model 1 = model 2,i.e. model 2多余的变量前系数为0.
anova(fit1,fit3)       #  Example 12.5
anova(fit2,fit3)  

##  Weekly_Interest_Rates_Model_Selection
install.packages("leaps")
install.packages("faraway")
library(leaps)
library(faraway)
dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
#全子集回归:regsubsets()
subsets = regsubsets(aaa_dif~.,
                     data=as.data.frame(cbind(cm10_dif,cm30_dif,ff_dif)),nbest=1) #as.datatype()表示将其参数转换为datatype对应的类型 。
b = summary(subsets)                         #  Example 12.7

postscript("WeekInt_model_selection.ps",width=6,height=3)  #  Figure 12.5
par(mfrow=c(1,3),lab=c(2,5,3),pch=19)
plot(1:3,b$bic,type="b",xlab="number of variables",
     ylab="BIC",cex=2.5)          #BIC图
plot(1:3,b$cp,type="b",xlab="number of variables",
     ylab="Cp",cex=2.5)           #Cp图
plot(1:3,b$adjr2,type="b",xlab="number of variables",
     ylab="adjusted R2",cex=2.5)  #adjusted R2图
graphics.off()
#  Example 12.8 
vif(lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)) #计算VIF值

##  Weekly_Interest_Rates_Partial_Residual
dat = read.table(file="WeekInt.txt",header=T)
install.packages("car")
library("car")
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)
fit4 = lm(aaa_dif~cm30_dif)

postscript("weekint_partialresidual.ps",width=6,height=5)  #  Figure 12.7
par(mfrow=c(2,2))
#偏残差图Partial residual plots：crPlot()--y轴是Component+Residual(aaa_dif)
crPlot(fit2,var="cm10_dif",main="(a)",smooth=F,lty=1,lwd=2,col="black")
crPlot(fit2,var="cm30_dif",main="(b)",smooth=F,lty=1,lwd=2,col="black")
plot(cm10_dif,aaa_dif,main="(c)")
regLine(fit1,col="black",lwd=2,lty="dashed")
plot(cm30_dif,aaa_dif,main="(d)")
regLine(fit4,col="black",lwd=2,lty="dashed")
graphics.off()

## Detecting_Nonlinearity
n = 80
set.seed("781235")
e = matrix(runif(12*n),nrow=n) %*% rep(1,12) 
#runif() 生成均匀分布随机数的函数, %*%表示通常意义下的两个矩阵的乘积, rep(1,12)表示1-12循环
e = abs(e)^4
e= e/mean(e)  #(|e|^4)/E(e)
x1 = runif(n)  
x1 = sort(x1) 
x2 = rbeta(n,6,.5) #Beta函数
y =( 8*x2 + x1 + 5*x1^3) + ( 4* x2 + x1 + 7*x1^3) * e 
postscript("ResidEx5.ps",height=4,width=7)  # Figure 13.6
par(mfrow=c(1,2))
plot(x1,y,xlab=expression(x[1]))
plot(x2,y,xlab=expression(x[2]))
graphics.off()

fit = lm(y~x1+x2)
rstudent = rstudent(fit) #rstudent学生化残差
postscript("ResidEx1.ps",height=4,width=7)  # Figure 13.7
par(mfrow=c(1,2))
qqnorm(rstudent,datax=T,main="Normal QQ Plot") # QQ图-Normal QQ Plot
hist(rstudent,12) #rstudent的直方图
graphics.off()

## Residual_Plots_For_Weekly_Interest_Changes
dat = read.table(file="WeekInt.txt",header=T)
install.packages("car")
install.packages("lmtest")
install.packages("forecast")
library("car")
library("lmtest")
library(forecast)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )

fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
durbinWatsonTest(fit2,max.lag=1,rep=10000) # DW检验：H0：rho = 0.
dwtest(fit2,alternative="two.sided",exact=T) #DW检验：H0：true autocorrelation=0.

resid = residuals(fit2) #residuals()求残差
auto.arima(resid,ic="aic") #构建ARIMA模型,利用R里面的auto.arima()函数,自动筛选p\d\q
n = length(resid)
tt = (1:n)/(n+1)
library(forecast)
auto.arima(resid,ic="aic") #auto.arima()函数通过选取AIC和BIC最小来选取模型
auto.arima(resid,ic="bic")

postscript("WeekInt_2var_residuals.ps",height=6,width=6)   #  Figure 13.0
par(mfrow=c(2,2),lwd=1)
qqnorm(resid,datax=T,main="(a) Normal plot",xlab="theoretical quantiles",
       ylab="sample quantiles") 
#(a)Normal plot --- qqnorm是样本与样本期望的正态性对比
qqline(resid,datax=T) #qqline 残差拟合线
qqplot(resid, (-0.00035 +0.04058 * qt(tt,df=3)) ,main="(b) t-plot",
       ylab="theoretical Quantiles") 
#(b)QQ plot of the residuals and the quantiles of the fitted t-distribution 
abline(0,1) #(b)with a 45o reference line
acf(resid,main="(c)",xlab="lag") #ACF图
plot(fitted(fit2),resid,main="(d)",ylab="Residuals",xlab="fitted values")
graphics.off()

##  high-leverage point 高影响点
set.seed("9879")
x = sort(rnorm(100,mean=1,sd=3))
y = x + .3*x + rnorm(100,sd=3)
x[58] = x[58] + 16
fit = lm(y~x)
yhat = fit$fit #fitted values
resid = rstudent(fit)
postscript("exercises2_02.ps",width=6,height=5)
par(mfrow=c(2,3))
plot(x,resid,main="(a)") #plot of resid v.s. x
lines(lowess(x,resid),lwd=2) #lowess/loess 平滑曲线,本质上是(加权)局部回归
plot(yhat,abs(resid),main="(b)") #plot of absolute resid v.s. fitted values
lines(lowess(yhat,abs(resid)),lwd=2) 
qqnorm(resid,datax=T,main="(c)", #normal QQ plot of resid
       ylab="sample quantiles",xlab="theoretical quantiles")
qqline(resid,datax=T)
acf(resid,main="(d)",xlab="lag") #ACF plot of resid
plot(sqrt(cooks.distance(fit)),main="(e)",xlab="index") 
#plot of sqrt root of cook's D v.s. observation num
halfnorm(sqrt(cooks.distance(fit)),ylab="sqrt(CookD)",main="(f)")
#half-norm plot of sqrt root of cook's D
graphics.off()

## heteroskedasticity 异方差
set.seed("9879")
x = sort(rnorm(100,mean=1,sd=3))
y = x + .3*x^2 + rnorm(100,sd=3)
fit = lm(y~x)
yhat = fit$fit
resid = rstudent(fit)
postscript("exercises2_01.ps",width=6,height=5)
par(mfrow=c(2,3))
plot(x,resid,main="(a)")
lines(lowess(x,resid),lwd=2)
plot(yhat,abs(resid),main="(b)")
lines(lowess(yhat,abs(resid)),lwd=2)
qqnorm(resid,datax=T,main="(c)",
       ylab="sample quantiles",xlab="theoretical quantiles")
qqline(resid,datax=T)
acf(resid,main="(d)",xlab="lag")
plot(sqrt(cooks.distance(fit)),main="(e)",xlab="index")
halfnorm(sqrt(cooks.distance(fit)),ylab="sqrt(CookD)",main="(f)")
graphics.off()


###################### R-lab-NO.2
##Rlab2_1
install.packages("AER")
install.packages("sandwich")
install.packages("survival")
library(AER)
data("USMacroG")
MacroDiff= apply(USMacroG,2,diff) #2-对列apply；1-对行apply
consumption = MacroDiff[,2] #选取第2列数据
dpi= MacroDiff[,5] #选取第5列数据
cpi= MacroDiff[,6]
government= MacroDiff[,4]
unemp= MacroDiff[,9]

pairs(cbind(consumption,dpi,cpi,government,unemp))
#pairs()产生X的列之间两两相对的成对散点图阵列(pairwise scatterplot matrix)
#cbind()横向追加；rbind()纵向追加
fitLm1 = lm(consumption~dpi+cpi+government+unemp)
summary(fitLm1) #select useful variables
#confint(fitLm1)

anova(fitLm1) #AOV table
library(MASS)
fitLm2 = stepAIC(fitLm1) #stepAIC()逐步回归筛选变量，默认backward向后
#相当于step(fitLm1, direction="backward")
summary(fitLm2)
AIC(fitLm1)
AIC(fitLm2)
AIC(fitLm1)-AIC(fitLm2) #比较筛选变量前后AIC的变化

install.packages("car")
library(car)
vif(fitLm1)
vif(fitLm2) #用VIF值---判断collinearity共线性是否存在

par(mfrow=c(2,2))
sp = 0.8 #The smoothness of the lowess curves,default is span = 0.5
#crplot()做partial residual plots部分残差图 = component+residual plot
crPlot(fitLm1,dpi,span=sp,col="black") 
crPlot(fitLm1,cpi,span=sp,col="black")
crPlot(fitLm1,government,span=sp,col="black")
crPlot(fitLm1,unemp,span=sp,col="black")

##Rlab2_2
#略

