library(readxl)
library(cluster)
library(fpc)
library(caret)
library(e1071)
z1<-read_excel("D:\\DATA ANALYSIS\\EXCEL SHEETS\\seeds.xlsx")
str(z1)
names(z1)
View(z1)
######## determining number of clusters
wss<-0
for(i in 1:15){
  wss[i]<-sum(kmeans(z1,centers = i)$withinss)
}
wss[3]-wss[4]
wss[4]-wss[5]
wss[5]-wss[6]
plot(1:15,wss,type="b")
####### clustering with 3 clusters
set.seed(123)
z2<-scale(z1)
clus<-kmeans(z2,3)
clus
clus$size
clus$withinss
clus$betweenss
###### silhouette coeff
diss<-daisy(z2)
diss1<-diss^2
silcof<-silhouette(clus$cluster,diss1)
plot(silcof)
###### plotting
library(factoextra)
fviz_cluster(clus,z2)
clusplot(z2,clus$cluster,color = T)
##### merging
z3<-data.frame(z1,clusters=clus$cluster)
#write.csv(z3,"D:\\DATA ANALYSIS\\Output CSV\\clustersedds.csv")
######################### KNN analysis ###########################
View(z3)
names(z3)
###### data standardization
z4<-subset(z3,select = -clusters)
z4<-scale(z4)
View(z4)
z5<-data.frame(z4,Cluster=z3$clusters)
z5$Cluster<-as.factor(z5$Cluster)
str(z5$Cluster)
View(z5)
###### data partition
set.seed(123)
index<-createDataPartition(z5$Cluster,p=0.70,list=F)
train<-z5[index,]
test<-z5[-index,]
dim(train)
dim(test)
###### modelling
m1<-train(Cluster~.,data=train,method="knn")
m1
plot(m1)
###### testing
pre<-predict(m1,newdata=test,type="prob")
View(pre)
pre$class<-ifelse(pre$`1`>0.5,1,ifelse(pre$`2`>0.5,2,3))
##### confusion matrix
x<-table(test$Cluster,pre$class)
x
sum(diag(x))/sum(x)
