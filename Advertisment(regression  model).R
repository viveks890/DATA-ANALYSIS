library(readr)
z1<-read.csv("D:\\DATA ANALYSIS\\EXCEL SHEETS\\Advertising1.csv")
View(z1)
plot(z1$TV,z1$sales,pch=20,xlab = "tv",ylab = "sales",main="Scatter plot sales~tv",col="orange")
plot(z1$radio,z1$sales)
plot(z1$newspaper,z1$sales)
#############simple linear regression
m1<-lm(sales~TV,data = z1)
m1
m2<-lm(sales~radio,data = z1)
m2
m3<-lm(sales~newspaper,data = z1)
m3
#################multiple regression
m4<-lm(sales~TV+radio+newspaper,data = z1)
m4
m5<-lm(sales~.,data = z1)
m5
summary(m5)
dim(z1)
par(mfrow=c(2,2))
plot(m5)
########################predicting future values
library(caret)
library(psych)
######check for skewness
skew(z1$TV)
skew(z1$radio)
skew(z1$newspaper)
##############data partition
data_index<-createDataPartition(z1$sales,p=0.70,list = F)
View(data_index)
train<-z1[data_index,]
View(train)
test<-z1[-data_index,]
View(test)
dim(test)
###################multiple regression model
model1<-lm(sales~.,data=train)
model1
summary(model1)
###########check for multicollinearity
library(car)
vif(model1)
#############best regression line selection
library(ResourceSelection)
####forward selection
forward<-step(model1,direction = "forward")
forward
summary(forward)
#####backward selection
backward<-step(model1,direction = "backward")
backward
summary(backward)
##########testing
test1<-subset(test,select = -c(sales,newspaper))
prediction<-predict(backward,newdata = test1)
View(prediction)
results<-data.frame(original=test$sales,predicted=prediction)
View(results)
cor(results$original,results$predicted)