library(readr)
z1<-read.csv("D:\\DATA ANALYSIS\\kaggle\\heart.csv")
View(z1)
table(z1$target)
names(z1)
###########modelling
m1<-glm(target~.,data=z1,family="binomial")
m1
summary(m1)
library(car)
vif(m1)
##########data partition
library(caret)
index<-createDataPartition(z1$target,p=0.70,list = F)
train<-z1[index,]
test<-z1[-index,]
train1<-subset(train,select = -target)
test1<-subset(test,select=-target)
##########modelling
m2<-glm(target~.,data = train,family="binomial")
m2
summary(m2)
vif(m2)
library(ResourceSelection)
bck<-step(m2,direction = "backward")
summary(bck)
vif(bck)
pre<-predict(bck,newdata = test,type="response")
est<-ifelse(pre>0.5,1,0)
res<-data.frame(original=test$target,estimated=est)
View(res)
########confusion matrix
x<-table(res$original,res$estimated)
x
sum(diag(x))/sum(x)*100
#######library(ROCR)
library(ROCR)
p1<-prediction(pre,test$target)
p2<-performance(p1,"tpr","fpr")
plot(p2,col="red")


auc<-performance(p1,measure = "auc")
auc@y.values
