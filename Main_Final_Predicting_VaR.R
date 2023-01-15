#Please set your own work path
setwd("C:/Users/Administrator/Desktop/financial econometrics/main_financial econometrics")

library(tseries)
library(forecast)
#install.packages("xts")
library(xts)
#install.packages("MTS")
library(MTS)
#install.packages("rugarch")
#install.packages("rmgarch")
library(rugarch)
library(rmgarch)
#install.packages("mnormt")
library(mnormt)
library("MASS")
#install.packages("fExtremes")
library(fExtremes)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("mvtnorm")
library(mvtnorm)
install.packages("vars")
library(vars)


#Import data
data=read.csv("DATA_MERGE.csv",header=TRUE,col.names = c("DATE","SP500","INDU","NDX"))#,row.names = 1
#date_id=as.POSIXlt(data[,1],tz='UTC')
date_id=as.Date(data[,1])
data_s = subset(data, select = -DATE )
rownames(data_s) <- date_id
data_t <- as.xts(data_s) #data_t is a data set in XTS format

#Standardize each column of data
#data_n=scale(data_t,center=FALSE, scale=TRUE)
data_n = data_t
par(mfrow=c(1,2))
plot(data_t,main = "Original Daily Index level")
title(xlab = "Date", ylab = "Price",line=1)
plot(data_n,main = "Standardized Daily Index Level")
title(xlab = "Date", ylab = "Standardized Price",line=1)
legend("left",c("SP500","INDU","NDX"), col = c("black","red","green"), pch=21, cex=0.5, adj=c(0, 1),lty = c(1,1))


#Stationarity Test
adf.test(data_n[,1])
adf.test(data_n[,2])
adf.test(data_n[,3])

#Difference and standardize each column of data and test stationarity
diff_data = diff(data_n)
diff_data=diff_data[-1,]
adf.test(diff_data[,1])
adf.test(diff_data[,2])
adf.test(diff_data[,3])
#tsdisplay(diff_data[,1])
#tsdisplay(diff_data[,2])
#tsdisplay(diff_data[,3])

#Computed return and stored it in ldata_return
len=dim(diff_data)[1]
ldata_return=as.xts((data.frame(diff_data)/data.frame(data_n[1:len,]))*100) #simple rate of return = (P(t)-P(t-1))/P(t-1)
colnames(ldata_return) <- c("R_SP500(%)","R_INDU(%)","R_NDX(%)")

#Divide the total sample into train data and test data, the proportion is 7:3
prop_train=0.7
len_train=floor(prop_train*len)
data_return_train=ldata_return[1:len_train]
data_return_test=ldata_return[(len_train+1):len]
len_forecast=dim(data_return_test)[1]

#Fit train data using ARIMA model
auto.arima(data_return_train[,1],max.P=0,max.Q=0,ic="aic")
auto.arima(data_return_train[,2],max.P=0,max.Q=0,ic="aic")
auto.arima(data_return_train[,3],max.P=0,max.Q=0,ic="aic")

fit1 = arima(data_return_train[,1],order=c(2,0,0)) #Fit SP500 using ARIMA(2,0)
fit2 = arima(data_return_train[,2],order=c(4,0,2)) #Fit INDU using ARIMA(4,2)
fit3 = arima(data_return_train[,3],order=c(2,0,1)) #Fit NDX using ARIMA(2,1)

acf(residuals(fit1))
Box.test(residuals(fit1), lag=50, type="Ljung-Box")
acf(residuals(fit2))
Box.test(residuals(fit2), lag=50, type="Ljung-Box")
acf(residuals(fit3))
Box.test(residuals(fit3), lag=50, type="Ljung-Box")

#fit_garch model, the parameters in function can be adjusted
#fit_Data = ldata_return[,1], garch_name = 'fGARCH', sub_garch_name = 'TARCH'
#garch_order = c(1,1), arma_order = c(1,1), distribution = 'norm'
myspec <- function(garch_name,sub_garch_name,garch_order,arma_order,distribution){
  myspec = ugarchspec(
    variance.model = list(model = garch_name, 
                          garchOrder = garch_order, 
                          submodel = sub_garch_name, 
                          external.regressors = NULL, 
                          variance.targeting = FALSE), 
    
    mean.model = list(armaOrder = arma_order, 
                      include.mean = TRUE, 
                      archm = FALSE, 
                      archpow = 1, 
                      arfima = FALSE, 
                      external.regressors = NULL, 
                      archex = FALSE), 
    
    distribution.model = distribution)
  
  return(myspec)
}
fit_garch <- function(fit_data,garch_name,sub_garch_name,garch_order,arma_order,distribution){
  myspec = ugarchspec(
    variance.model = list(model = garch_name, 
                          garchOrder = garch_order, 
                          submodel = sub_garch_name, 
                          external.regressors = NULL, 
                          variance.targeting = FALSE), 
    
    mean.model = list(armaOrder = arma_order, 
                      include.mean = TRUE, 
                      archm = FALSE, 
                      archpow = 1, 
                      arfima = FALSE, 
                      external.regressors = NULL, 
                      archex = FALSE), 
    
    distribution.model = distribution)#normal is 'norm', student-t is "std', generalized error distribution is'ged'
  
  myfit=ugarchfit(myspec,data=fit_data,solver="solnp",out.sample = len_forecast)#GARCH??
  
  return(myfit)
}

#9 garch models are respectively fitted for the three indexes as follows---garch(1,1)/egarch(1,1)/tarch(1,1)

#normal distribution + garch
nor_garch_sp500_spec=myspec('fGARCH','GARCH',c(1,1),c(2,0),'norm')
nor_garch_sp500=fit_garch(ldata_return[,1],'fGARCH','GARCH',c(1,1),c(2,0),'norm')

nor_garch_indu_spec=myspec('fGARCH','GARCH',c(1,1),c(4,2),'norm')
nor_garch_indu=fit_garch(ldata_return[,2],'fGARCH','GARCH',c(1,1),c(4,2),'norm')

nor_garch_ndx_spec=myspec('fGARCH','GARCH',c(1,1),c(2,1),'norm')
nor_garch_ndx=fit_garch(ldata_return[,3],'fGARCH','GARCH',c(1,1),c(2,1),'norm')

#normal distribution + egarch
nor_egarch_sp500_spec=myspec('eGARCH',NULL,c(1,1),c(2,0),'norm')
nor_egarch_sp500=fit_garch(ldata_return[,1],'eGARCH',NULL,c(1,1),c(2,0),'norm')
nor_egarch_indu_spec=myspec('eGARCH',NULL,c(1,1),c(4,2),'norm')
nor_egarch_indu=fit_garch(ldata_return[,2],'eGARCH',NULL,c(1,1),c(4,2),'norm')
nor_egarch_ndx_spec=myspec('eGARCH',NULL,c(1,1),c(2,1),'norm')
nor_egarch_ndx=fit_garch(ldata_return[,3],'eGARCH',NULL,c(1,1),c(2,1),'norm')

#normal distribution + TARCH
nor_tarch_sp500_spec=myspec('fGARCH','TGARCH',c(1,1),c(2,0),'norm')
nor_tarch_sp500=fit_garch(ldata_return[,1],'fGARCH','TGARCH',c(1,1),c(2,0),'norm')
nor_tarch_indu_spec=myspec('fGARCH','TGARCH',c(1,1),c(4,2),'norm')
nor_tarch_indu=fit_garch(ldata_return[,2],'fGARCH','TGARCH',c(1,1),c(4,2),'norm')
nor_tarch_ndx_spec=myspec('fGARCH','TGARCH',c(1,1),c(2,1),'norm')
nor_tarch_ndx=fit_garch(ldata_return[,3],'fGARCH','TGARCH',c(1,1),c(2,1),'norm')

#student's-t distribution + garch
std_garch_sp500_spec=myspec('fGARCH','GARCH',c(1,1),c(2,0),'std')
std_garch_sp500=fit_garch(ldata_return[,1],'fGARCH','GARCH',c(1,1),c(2,0),'std')

std_garch_indu_spec=myspec('fGARCH','GARCH',c(1,1),c(4,2),'std')
std_garch_indu=fit_garch(ldata_return[,2],'fGARCH','GARCH',c(1,1),c(4,2),'std')

std_garch_ndx_spec=myspec('fGARCH','GARCH',c(1,1),c(2,1),'std')
std_garch_ndx=fit_garch(ldata_return[,3],'fGARCH','GARCH',c(1,1),c(2,1),'std')

#student's-t distribution + egarch
std_egarch_sp500_spec=myspec('eGARCH',NULL,c(1,1),c(2,0),'std')
std_egarch_sp500=fit_garch(ldata_return[,1],'eGARCH',NULL,c(1,1),c(2,0),'std')
std_egarch_indu_spec=myspec('eGARCH',NULL,c(1,1),c(4,2),'std')
std_egarch_indu=fit_garch(ldata_return[,2],'eGARCH',NULL,c(1,1),c(4,2),'std')
std_egarch_ndx_spec=myspec('eGARCH',NULL,c(1,1),c(2,1),'std')
std_egarch_ndx=fit_garch(ldata_return[,3],'eGARCH',NULL,c(1,1),c(2,1),'std')

#student's-t distribution + TARCH
std_tarch_sp500_spec=myspec('fGARCH','TGARCH',c(1,1),c(2,0),'std')
std_tarch_sp500=fit_garch(ldata_return[,1],'fGARCH','TGARCH',c(1,1),c(2,0),'std')
std_tarch_indu_spec=myspec('fGARCH','TGARCH',c(1,1),c(4,2),'std')
std_tarch_indu=fit_garch(ldata_return[,2],'fGARCH','TGARCH',c(1,1),c(4,2),'std')
std_tarch_ndx_spec=myspec('fGARCH','TGARCH',c(1,1),c(2,1),'std')
std_tarch_ndx=fit_garch(ldata_return[,3],'fGARCH','TGARCH',c(1,1),c(2,1),'std')

#Generalized Error Distribution + garch
ged_garch_sp500_spec=myspec('fGARCH','GARCH',c(1,1),c(2,0),'ged')
ged_garch_sp500=fit_garch(ldata_return[,1],'fGARCH','GARCH',c(1,1),c(2,0),'ged')

ged_garch_indu_spec=myspec('fGARCH','GARCH',c(1,1),c(4,2),'ged')
ged_garch_indu=fit_garch(ldata_return[,2],'fGARCH','GARCH',c(1,1),c(4,2),'ged')

ged_garch_ndx_spec=myspec('fGARCH','GARCH',c(1,1),c(2,1),'ged')
ged_garch_ndx=fit_garch(ldata_return[,3],'fGARCH','GARCH',c(1,1),c(2,1),'ged')

#Generalized Error Distribution + egarch
ged_egarch_sp500_spec=myspec('eGARCH',NULL,c(1,1),c(2,0),'ged')
ged_egarch_sp500=fit_garch(ldata_return[,1],'eGARCH',NULL,c(1,1),c(2,0),'ged')
ged_egarch_indu_spec=myspec('eGARCH',NULL,c(1,1),c(4,2),'ged')
ged_egarch_indu=fit_garch(ldata_return[,2],'eGARCH',NULL,c(1,1),c(4,2),'ged')
ged_egarch_ndx_spec=myspec('eGARCH',NULL,c(1,1),c(2,1),'ged')
ged_egarch_ndx=fit_garch(ldata_return[,3],'eGARCH',NULL,c(1,1),c(2,1),'ged')

#Generalized Error Distribution + TARCH
ged_tarch_sp500_spec=myspec('fGARCH','TGARCH',c(1,1),c(2,0),'ged')
ged_tarch_sp500=fit_garch(ldata_return[,1],'fGARCH','TGARCH',c(1,1),c(2,0),'ged')
ged_tarch_indu_spec=myspec('fGARCH','TGARCH',c(1,1),c(4,2),'ged')
ged_tarch_indu=fit_garch(ldata_return[,2],'fGARCH','TGARCH',c(1,1),c(4,2),'ged')
ged_tarch_ndx_spec=myspec('fGARCH','TGARCH',c(1,1),c(2,1),'ged')
ged_tarch_ndx=fit_garch(ldata_return[,3],'fGARCH','TGARCH',c(1,1),c(2,1),'ged')

#The extraction model results are as follows
# coef(nor_garch_sp500)   #Estimated coefficient
# vcov(nor_garch_sp500) €#Covariance matrix of parameter estimator
# infocriteria(nor_garch_sp500) #List of frequently used information
# newsimpact(nor_garch_sp500) #Calculate the information impact curve
# signbias(nor_garch_sp500) Engle - Ng Sign deviation test
# fitted(nor_garch_sp500)   #Get the fitted data sequence 
# residuals(nor_garch_sp500) €#Obtain the residual 
# uncvariance(nor_garch_sp500) #Unconditional (long-term) variance
# uncmean(nor_garch_sp500)   #Unconditional (long-term) mean

# plot(nor_garch_sp500) 


#Compare the real return rate of the whole period with the estimated VaR; the start period of backtest is len_train, fit is conducted every 50 days
#Set nor_garch_sp500 as example
alpha=c(0.1, 0.05, 0.01) #Tail Probability
nor_garch_roll <- ugarchroll(nor_garch_sp500_spec,  
                             data = ldata_return[,1],  
                             forecast.length = len_forecast,
                             n.start = len_train, 
                             refit.every = 50, 
                             refit.window = "moving",
                             solver = "hybrid", 
                             calculate.VaR = TRUE, 
                             VaR.alpha = alpha,
                             keep.coef = TRUE)

#Check backtest report, VaR.alpha is tail probability, conf.level is the confidence interval(alpha=1%ï¼?5%)
report(nor_garch_roll, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 

#Get plot
intc_var = zoo(nor_garch_roll@forecast$VaR[, 1])#The final VaR value
index(intc_var)=index(data_return_test)

#Draw the final VaR diagram
#The graphic parameters can be set according to the demand
par(mar=c(0.1,0.1,0.1,0.1)+2)
plot(data_return_test[,1], 
     type = "b", main = "99% 1 Month VaR  Backtesting",
     xlab = "Date", ylab = "Return/VaR in percent",
     xaxs="d",yaxs="d",
     ylim=c(-8,8),uniform=TRUE,compress=T)
par(new=TRUE)
plot(intc_var,xlab = "Date",ylab = "Return/VaR in percent",axes = FALSE,ylim=c(-8,8),col="red")
legend("bottomright", inset=.05, c("Intel return","VaR"), col = c("black","red"), lty = c(1,1))

#Forcast the return in test window using the former models
nor_garch_forc=ugarchforecast(nor_garch_sp500,n.ahead=len_forecast)
plot(nor_garch_forc)

#The entire process of single index is completed.

############################################################## 

#The following is process using multivariate garch.
cor(ldata_return) #The correlation of three index

#Scroll parameter setting
rolling_size=10
var_adjust_multiplier <- 10
#rolling_data_1=data_return_train[(len_train-rolling_size+1):len_train]#take out the last 10 of train data to calculate the standard deviation, as the standard deviation of test data on day 1
#rolling_data=rbind(rolling_data_1,pred_go)#merge the above 10 rows with test data

{#the following code is how we construct VAR model to predict future return
  fc <- {}
  for (n in 1:990){
    data_return_train0 <- ldata_return[n:(len_train+n-1)]
    var <- VAR(data_return_train0[,1:3], p=1, type="const")
    pd <- predict(var,n.ahead=1,ci=0.99)
    fc[n] <- pd[["fcst"]][["R_SP500..."]][1]
  }
  
  data_return_train0 <- data_return_train[,1]
  data_return_test0 <- data_return_test[,1][1:990]
  for (i in 1:990){
    data_return_test0[i] <- fc[i]
  }
  data_return_test0 = data_return_test0 * var_adjust_multiplier
  ts <- cbind.xts(data_return_train0,data_return_test0)
  plot(ts)
}

{# {#GO_GARCH
# #Building spec
#   
# #alpha=0.05
# get_VaR <- function(sigma,alpha){
#   len = dim(sigma)[1]
#   for (i in 1:3){
#     for (p in 1:len){
#       sigma_p = sigma[p,i,1]
#       var_unit = qmvnorm(alpha, sigma=sigma_p)$quantile * sigma_p
#       var_column = rbind(0,var_unit)
#     }
#     var_row = cbind(0,var_column)
#     var = var_row[-1,]
#     #var_result = var[,-1]
#     return(var)
#   }
#   
# }
#   
# alpha=0.05    
# length_go_for = 50
# 
# go_spec=gogarchspec(
#   mean.model = list(model = "VAR", robust = FALSE, 
#                     lag = 1, lag.max = NULL, 
#                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), 
#                     external.regressors = NULL), 
#   variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "GARCH", 
#                         variance.targeting = FALSE), 
#   distribution.model = "mvnorm", 
#   ica = c("fastica", "radical"))
# 
# #Fitting model
# go_roll=gogarchroll(spec=go_spec,data=ldata_return,forecast.length = len_forecast,
#                     n.start = len_train, refit.every = 50,refit.window = "moving",
#                     solver = "hybrid",
#                     calculate.VaR = TRUE, 
#                     VaR.alpha = alpha,
#                     keep.coef = TRUE)
# 
# go_fit = gogarchfit(spec=go_spec, data=ldata_return, out.sample = len_forecast, solver = "solnp")
# 
# go_forc = gogarchforecast(go_fit, n.ahead = length_go_for)
# 
# sigma = go_forc@mforecast[["factor.sigmas"]]
# 
# 
# 
}

{#DCC-GARCH
#Building spec
  
length_dcc_for = 50

spec_list <- list(nor_garch_sp500_spec, nor_garch_indu_spec, nor_garch_ndx_spec)
mul_spec = multispec(spec_list)
dcc_spec = dccspec(mul_spec, VAR=TRUE, robust = TRUE, lag = 1, lag.max = NULL, dccOrder = c(1,1), distribution = "mvnorm")

dcc_fit=dccfit(spec=dcc_spec, data=ldata_return, 
               out.sample = len_forecast, solver = "solnp")

dcc_forc=dccforecast(dcc_fit, n.ahead = length_dcc_for, n.roll =0)

plot(dcc_forc)
}


