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


