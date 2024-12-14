#QUESTION 1
#Loading the data
oldfaithful<-read.csv("oldfaithful.csv", header = T, sep = ",")
str(oldfaithful)

#1a
plot(oldfaithful$Duration, oldfaithful$Interval, col="red",main = "SCATTER PLOT OF INTERVAL VS DURATION",
     col.main="blue", cex.main=1, pch="o",ylab="INTERVAL",xlab="DURATION",col.lab="blue",las=1,bty="l")
cor.test(oldfaithful$Duration, oldfaithful$Interval, conf.level = 0.95)

#1b
model1=lm(oldfaithful$Interval ~ oldfaithful$Duration)

#Ic
par(mfrow=c(1,2))
plot(model1, which = c(1,2)) #Residual plot and normal Q-Q plot

barplot(hatvalues(model1), main="Leverage plot",xlab = "Observation", ylab = "Leverage") #leverage barplot
abline(h = 4/length(oldfaithful$Duration), col = "red",lwd=2)

par(mfrow=c(1,1))
plot(cooks.distance(model1), main="Cooks distance", type = 'h',las=1)

#1d
summary(model1)  #model summary to get coefficient and t-test

#1e - prediction when duration = 100
durat = 100
pred = model1$coefficients[1] + model1$coefficients[2]*durat

#appropriate interval estimate is prediction interval
n=length(oldfaithful$Interval)
MSE<-sum(residuals(model1)^2)/(n-2)
X=oldfaithful$Duration
X_mean = mean(X)
Sxx = sum((X-X_mean)^2)
s_pred = sqrt(MSE*(1 + 1/n + (durat-X_mean)^2/Sxx))
alpha=0.05
estimated_interval = c(pred, pred-qt(1-alpha/2, n-2)*s_pred, pred+qt(1-alpha/2, n-2)*s_pred)
names(estimated_interval) = c("predicted", "Lower PI", "Upper PI")
estimated_interval


#QUESTION 2
install.packages("alr4")
install.packages("Rcpp")
library('alr4')
library("Rcpp")
data("UBSprices")
colnames(UBSprices)
attach(UBSprices)

#2a scatter plot of Bigmac price
plot(bigmac2003, bigmac2009, main = "Big mac Prices in 2003 vs 2009",las=1,bty="l",
     col="blue", col.lab="blue",col.main="blue")
abline(lm(bigmac2009~bigmac2003),lty=2, lwd=2,col="red")

#2b
UBSprices$MacDiff <- bigmac2003 - bigmac2009
sortedUBSprices <- UBSprices[order(UBSprices$MacDiff),]
head(data.frame(row.names(sortedUBSprices), sortedUBSprices$MacDiff),3) #highest decrease
tail(data.frame(row.names(sortedUBSprices), sortedUBSprices$MacDiff),3)

#2c
plot(log(bigmac2009), log(bigmac2003), main= "Plot of transformed data",las=1,bty="l",
     col="blue", col.lab="blue",col.main="blue")

#2d Linear regression model
model2 = lm(log(bigmac2009) ~ log(bigmac2003))
summary(model2)
model2$coefficients

#2e: ANOVA MODEL
anova(model2)

#2f
par(mfrow=c(1,1))
plot(model2, which = c(1,2)) #Assumption of normal error regression model

par(mfrow=c(2,2))
plot(model2, which=4, labels.id = row.names(UBSprices))