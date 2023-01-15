########################## Lecture 1 ################################

############Tyler's Introduction to R
# Functions:
arc <- function(x) 2*asin(sqrt(x)) #定义函数function_name <- function() function_body
arc(0.5)

# Reading data from files:
#setwd("xxx")可以设置工作路径，否则需要把数据放入默认路径
myData <- read.table("R_Tutorial_Data.txt", header=TRUE, sep="\t") 
myData
plot(myData) #散点图

# Selecting Subsets of Data:
myData$Learning
myData$Learning[myData$Group=="A"]
attach(myData) #固定数据集
Learning
Learning[Group=="A"]
Condition[Group=="B"&Learning<0.5] #多条件筛选数据

# data normally distributed?
plot(dnorm,-3,3,col="blue",lwd=3, main="The Normal Distribution") 
#数据dnorm，横坐标-3to+3，col线条颜色，lwd线条宽度，main图主标题
par(mfrow=c(1,2)) #表示要画组合图，1行2个图
hist(Learning[Condition=="High"&Group=="A"]) #hist() 直方图
hist(Learning[Condition=="Low"&Group=="A"])
#Shapiro-Wilk normality test，用来检验是否数据符合正态分布
shapiro.test(Learning[Condition=="High"&Group=="A"])

#lm(y ~ x) 拟合回归模型
myModel <- lm(Learning ~ Pre1 + Pre2 + Pre3 + Pre4)
par(mfrow=c(2,2))
plot(myModel) #拟合回归模型的residuals相关图（包括Normal Q_Q图）
summary(myModel) #H0:beta=0 #Residuals/Coefficients/Residual standard error/R-squared/F-statistic/p-value

#逐步回归
step(myModel, direction="backward") #向后逐步回归
step(myModel, direction="forward") #向前逐步回归

#方差分析ANOVA
myANOVA <- aov(Learning~Group*Condition) #方差分析:group/condition/group*condition/residuals
summary(myANOVA) #p-vlaue < 0.05,reject H0, beta!=0, have effect on learning
par(mfrow=c(1,1))
boxplot(Learning~Group*Condition,col=c("#ffdddd","#ddddff")) #boxplot() 直方图,colb分组直方图颜色

myANOVA2 <- aov(Learning~Group*Condition+Gender) #方差分析: “+” -> 仅增加一行gender
summary(myANOVA2) #p > 0.05, accept H0, beta=0, gender have no effect on learning
boxplot(Learning~Group*Condition+Gender,col=c(rep("pink",4),rep("light blue",4)))

## Basic parametric inferential statistics 
# Independent sample t-tests:(独立样本T检验)
t.test(Pre2[Group=="A"], Pre2[Group=="B"], paired=FALSE)
# Independent sample t-tests with equal variance assumed:
t.test(Pre2[Group=="A"], Pre2[Group=="B"], paired=FALSE, var.equal=TRUE)
# Independent sample t-tests with equal variance assumed, one-tailed:(单边)
t.test(Pre2[Group=="A"], Pre2[Group=="B"], paired=FALSE, var.equal=TRUE, alternative='greater')
# Paired sample t-test:(配对样本T检验)
t.test(Pre4[Group=="A"], Pre3[Group=="A"], paired=TRUE)
# Visualize this test:
boxplot(Pre4[Group=="A"],Pre3[Group=="A"],col=c("#ffdddd","#ddddff"),names=c("Pre4","Pre3"),main="Group A")
# One sample t-test:(单样本T检验)
t.test(Learning[Group=="B"], mu=0.5, alternative="greater")
# Visualize this test:
boxplot(Learning[Group=="B"],names="Group B",ylab="Learning")
lines(c(0,2),c(0.5,0.5),col="red")
points(c(rep(1,length(Learning[Group=="B"]))),Learning[Group=="B"],pch=21,col="blue")

# Correlation
cor.test(Pre1,Learning,method="pearson")
plot(Pre1,Learning)


## Basic nonparametric inferential statistics 
# Check for normal distribution
t.test(Learning[Condition=="High"&Group=="A"],Learning[Condition=="Low"&Group=="A"])
plot(dnorm,-3,3,col="blue",lwd=3,main="The Normal Distribution")
par(mfrow=c(1,2))
hist(Learning[Condition=="High"&Group=="A"])
hist(Learning[Condition=="Low"& Group=="A"])
shapiro.test(Learning[Condition=="High"&Group=="A"])
shapiro.test(Learning[Condition=="Low"&Group=="A"])

# Mann-Whitney U / Wilcoxon Signed Rank Test
wilcox.test(Learning[Condition=="High"&Group=="A"],Learning[Condition=="Low"&Group=="A"],exact=FALSE,paired=FALSE)
x <- matrix(c(length(Learning[Group=="A"&Condition=="High"&Gender=="F"]),length(Learning[Group=="A"&Condition=="Low"&Gender=="F"]),length(Learning[Group=="B"&Condition=="High"&Gender=="F"]),length(Learning[Group=="B"&Condition=="Low"&Gender=="F"])),ncol=2)
x
chisq.test(x)
# 后续Linear Models and ANOVA与parametric相同


###################### R-lab-NO.1
dat = read.csv("Stock_FX_bond.csv",header=TRUE) #读取数据

names(dat) 
attach(dat) #固定数据集
par(mfrow=c(1,2)) 
plot(GM_AC) #plot the adjusted closing prices of GM and Ford.
plot(F_AC)

#problem 1
n = dim(dat)[1]   #find the sample size (n)
GMReturn = GM_AC[2:n]/GM_AC[1:(n-1)] - 1 #compute GM returns = GM(n)/GM(n-1) - 1.
FReturn = F_AC[2:n]/F_AC[1:(n-1)] - 1 
par(mfrow=c(1,1))
plot(GMReturn,FReturn) #plot GM returns versus the Ford returns.
#The plot below shows a strong positive correlation, 
#and the outliers in GM returns seem to occur with outlying Ford returns.

#problem 2
GMLogReturn = log(GMReturn+1) #计算LogReturn，注意需要先+1！
cor(GMLogReturn,GMReturn) #计算correlation
plot(GMReturn,GMLogReturn)
#The correlation is almost 1 
#and the plot below shows an almost perfect linear relationship.



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



########################## Lecture 3 ################################
###################### R-lab-NO.3
dat = read.csv("Rlab3_Stock_FX_Bond_2004_to_2006.csv",header=T)
prices = dat[,c(5,7,9,11,13,15,17,24)] #提取dat中需要的几列数据
n = dim(prices)[1] #dim(x)查看变量的维数

dat2 = as.matrix(cbind(dat[(2:n),3]/365, 100*(prices[2:n,]/prices[1:(n-1),] - 1)))
#as.matrix()将数据矿转换成矩阵,dat[(2:n),3]/365是three_month_treasury的daily rate
#100*(prices[2:n,]/prices[1:(n-1),] - 1)是market和7 stocks的daily rate
names(dat2)[1] = "treasury"
risk_free = dat2[,1] #three_month_treasury的daily rate为risk_free_rate
ExRet = dat2[,2:9] - risk_free #excess rate
market = ExRet[,8] #excess rate of market
stockExRet = ExRet[,1:7] #excess rate of 7 stocks

fit_reg = lm(stockExRet~market) #fit model to each stock
summary(fit_reg)
res = residuals(fit_reg) #compute the residuals
pairs(res) #scatter-plot matrix of the residuals
options(digits=3) #保留3位小数

betas=fit_reg$coeff[2,] #extract the estimated betas,提取每个reg模型中market的系数b1
betas*mean(market) #the sample means of the excess returns to estimate the expected excess return
apply(stockExRet,2,mean) #1表示按行计算，2按列计算 #the expected excess return

res = residuals(fit_reg)
options(digits=3)
cor(res) #计算相关系数 correlations

4*betas



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



