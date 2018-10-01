# load the Library
library("e1071")
library("naivebayes")
library("dplyr")
library("ggplot2")
library("psych")

#Load the dataset
data<- read.csv("D:/Studies/VIII Semester/R/Programs/Ex 8/Spectacle.csv", header=TRUE)

#Structure of Data
View(data)
str(data)
xtabs(~Age+Power_Rate,data = data)

#Visualization
pairs.panels(data[-1])
data %>%

       ggplot(aes(x=Spectacle_Prescription,y=Power_Rate,fill=Spectacle_Prescription))+
       geom_boxplot()+
       ggtitle("Box Plot")


data %>%
  
       ggplot(aes(x=Spectacle_Prescription,fill=Astigmatism))+
       geom_density(alpha=0.8,color='black')+
       ggtitle("Density Plot")

#Data Partition
set.seed(1234)
ind<-sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
train<-data[ind==1,]
test<-data[ind==2,]

#Naive Bayes Model
model<-naive_bayes(Age~.,data=train)
model

train %>%
  
      filter(Power_Rate>="5") %>%
      summarise(mean(Astigmatism),sd(Astigmatism))
plot(model)

#Predict
p<-predict(model,train,type='prob')
head(cbind(p,train))

#Confusion Matrix-train data
p1<-predict(model,train)
(tab1<-table(p1,train$Age))
1-sum(diag(tab1))/sum(tab1)

#Confusion Matrix-test data
p2<-predict(model,test)
(tab2<-table(p2,test$Age))
1-sum(diag(tab2))/sum(tab2)

