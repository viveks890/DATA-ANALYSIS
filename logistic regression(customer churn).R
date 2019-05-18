library(readr)
library(caret)
z1<-read.csv("D:\\DATA ANALYSIS\\EXCEL SHEETS\\TelcoCustomerChurn.csv")
#View(z1)
names(z1)
z1$gender<-as.integer(z1$gender)
z1$gender<-ifelse(z1$gender==1,0,1)
z1$Partner<-as.integer(z1$Partner)
z1$Partner<-ifelse(z1$Partner==1,0,1)
z1$Dependents<-as.integer(z1$Dependents)
z1$Dependents<-ifelse(z1$Dependents==1,0,1)
z1$PhoneService<-as.integer(z1$PhoneService)
z1$PhoneService<-ifelse(z1$PhoneService==1,0,1)
z1$MultipleLines[which(z1$MultipleLines=="No phone service")]<-2
z1$MultipleLines<-as.integer(z1$MultipleLines)
z1$MultipleLines<-ifelse(z1$MultipleLines==3,1,0)
z1$MultipleLines[which(is.na(z1$MultipleLines))]<-0
z1$InternetService<-as.integer(z1$InternetService)
z1$InternetService[which(z1$InternetService==3)]<-0
z1$OnlineSecurity<-as.integer(z1$OnlineSecurity)
z1$OnlineSecurity[which(z1$OnlineSecurity==1)]<-0
z1$OnlineSecurity[which(z1$OnlineSecurity==3)]<-2
z1$OnlineSecurity[which(z1$OnlineSecurity==2)]<-1
z1$OnlineBackup<-as.integer(z1$OnlineBackup)
z1$OnlineBackup[which(z1$OnlineBackup==1)]<-0
z1$OnlineBackup[which(z1$OnlineBackup==3)]<-1
z1$DeviceProtection<-as.integer(z1$DeviceProtection)
z1$DeviceProtection[which(z1$DeviceProtection==1)]<-0
z1$DeviceProtection[which(z1$DeviceProtection==3)]<-1
z1$TechSupport<-as.integer(z1$TechSupport)
z1$TechSupport[which(z1$TechSupport==1)]<-0
z1$TechSupport[which(z1$TechSupport==3)]<-1
z1$StreamingTV<-as.integer(z1$StreamingTV)
#z1$StreamingTV[which(z1$StreamingTV==3)]<-1
z1$StreamingTV[which(z1$StreamingTV==1)]<-0
z1$StreamingTV[which(z1$StreamingTV==3)]<-1
z1$StreamingMovies<-as.integer(z1$StreamingMovies)
z1$StreamingMovies[which(z1$StreamingMovies==1)]<-0
z1$StreamingMovies[which(z1$StreamingMovies==3)]<-1
z1$Contract<-as.integer(z1$Contract)
z1$Contract[which(z1$Contract==1)]<-0
z1$Contract[which(z1$Contract==2)]<-1
z1$Contract[which(z1$Contract==3)]<-2
z1$PaperlessBilling<-as.integer(z1$PaperlessBilling)
z1$PaperlessBilling<-ifelse(z1$PaperlessBilling==2,1,0)
z1$Churn<-as.integer(z1$Churn)
z1$Churn<-ifelse(z1$Churn==1,0,1)
str(z1)
#########################################
z2<-subset(z1,select = -c(customerID,OnlineBackup,MonthlyCharges,TotalCharges,DeviceProtection,Partner,gender,PaymentMethod))
######data partition
index<-createDataPartition(z2$Churn,p=0.70,list=F)
train<-z2[index,]
test<-z2[-index,]
library(car)
##########modelling
m1<-glm(Churn~.,data=train,family = "binomial")
m1
summary(m1)
vif(m1)
library(ResourceSelection)
bck<-step(m1,direction="backward")
bck
summary(bck)
vif(bck)
pre<-predict(bck,newdata = test,type="response")
#View(pre)
est<-ifelse(pre>0.5,1,0)
View(est)
res<-data.frame(orig=test$Churn,estimated=est)
View(res)
######confusion matrix
x<-table(res$orig,res$estimated)
x
(sum(diag(x))/sum(x))*100
######ROC curve
library(ROCR)
p1<-prediction(pre,test$Churn)
p2<-performance(p1,"tpr","fpr")
plot(p2,col="red")
abline(0,1)

auc<-performance(p1,measure = "auc")
auc@y.values
