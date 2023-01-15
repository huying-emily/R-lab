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


