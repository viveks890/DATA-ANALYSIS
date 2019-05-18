library(readr)
x1<-read.csv("D:\\outbreak.csv")
View(x1)
summary(x1)
###########################age
##to convert "seven" into numeric,first covert it as character then replace with 7 then convert back to numeric
#summary(x1$age)
x1$age<-as.character(x1$age)
str(x1$age)
x1$age<-ifelse(x1$age=="seven",7,x1$age)
View(x1)
x1$age<-as.numeric(x1$age)
str(x1$age)
summary(x1)
median(x1$age)
x1$age<-ifelse(x1$age>100,median(x1$age),x1$age)
View(x1$age)
############################sex
median(x1$sex)
boxplot(x1$sex)
table(x1$sex)
x1$sex<-ifelse(x1$sex<0,median(x1$sex),x1$sex)
table(x1$sex)
############################timesupper
table(x1$timesupper)
summary(x1$timesupper)
dim(x1)
############################ill
summary(x1$ill)
table(x1$ill)
############################ill with respect to gender
table(x1$ill)
table(x1$sex)
(table(x1$ill,x1$sex)/sum(table(x1$ill,x1$sex)))*100
############################all the fluids in the party
ifelse(x1$jello & x1$milk & x1$coffee & x1$water & x1$vanilla,1,"-")
############################food more consumed by the people
summary(x1)
###ans==vanilla##
############################rate of illness wrt food items
table(x1$ill,x1$bakedham)
table(x1$ill,x1$spinach)
table(x1$ill,x1$mashedpota)######and so on
############################time preffered for supper
table(x1$timesupper)
#####ans==2200
############################timesupper not recorded
sum(is.na(x1$timesupper))
################BOXPLOT###############
boxplot(x1$ill,x1$age)
####aged person is having more chance of falling ill
