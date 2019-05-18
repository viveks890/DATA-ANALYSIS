library(readr)
library(caret)
library(ROCR)
library(ResourceSelection)
library(car)
z1<-read.csv("D:\\DATA ANALYSIS\\EXCEL SHEETS\\bank.csv")
names(z1)
class(z1)
str(z1)
z1$job<-as.integer(z1$job)
View(z1)
z1$marital<-as.integer(z1$marital)
z1$marital[which(z1$marital==3)]<-0
z1$education<-as.integer(z1$education)
z1$education[which(z1$education==1)]<-0
z1$education[which(z1$education==2)]<-1
z1$education[which(z1$education==3)]<-2
z1$education[which(z1$education==4)]<-3
z1$default<-as.integer(z1$default)
z1$default[which(z1$default==1)]<-0
z1$default[which(z1$default==2)]<-1
z1$housing<-as.integer(z1$housing)
z1$housing[which(z1$housing==1)]<-0
z1$housing[which(z1$housing==2)]<-1
z1$loan<-as.integer(z1$loan)
z1$loan[which(z1$loan==1)]<-0
z1$loan[which(z1$loan==2)]<-1
z1$contact<-as.integer(z1$contact)
z1$contact[which(z1$contact==1)]<-0
z1$contact[which(z1$contact==2)]<-1
z1$contact[which(z1$contact==3)]<-2
z1$month<-as.integer(z1$month)
z1$y<-as.integer(z1$y)
z1$y[which(z1$y==1)]<-0
z1$y[which(z1$y==2)]<-1
z1$poutcome<-as.integer(z1$poutcome)
z1$poutcome[which(z1$poutcome==1)]<-0
z1$poutcome[which(z1$poutcome==3)]<-1
z1$poutcome[which(z1$poutcome==4)]<-3
##########modelling
m1<-glm(y~.,data = z1,family="binomial")
m1
summary(m1)
z2<-subset(z1,select = -c(job,default,balance,day,month,pdays))
######modelling
m2<-glm(y~.,data = z2,family="binomial")
m2
summary(m2)
vif(m2)
#######data partition
index<-createDataPartition(z1$y,p=0.70,list=F)
train<-z2[index,]
test<-z2[-index,]
#######best regression line selection
bck<-step(m2,direction="backward")
summary(bck)
vif(bck)
pre<-predict(bck,newdata = test,type="response")
est<-ifelse(pre>0.50,1,0)
res<-data.frame(original=test$y,estimated=est)
#####confusion matrix
x<-table(res$original,res$estimated)
x
(sum(diag(x))/sum(x))*100
#####roc curve
p1<-prediction(pre,test$y)
p2<-performance(p1,"tpr","fpr")
plot(p2,col="red")
#####area under the curve
auc<-performance(p1,measure = "auc")
auc@y.values
