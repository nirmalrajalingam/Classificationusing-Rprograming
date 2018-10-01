#DECISION TREE

data<-quakes
View(data)
str(data)
data$NSPF <- factor(data$stations)

#Partion data into Training and Validation Datasets
set.seed(1234)
pd<-sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
train<-data[pd==1,]
validate<-data[pd==2,]

#Decision Tree with Party
library(party)
tree<-ctree(NSPF~lat+long+mag,data=train,controls = ctree_control(mincriterion = 0.99,minsplit = 5))
tree
plot(tree)

#Predict
predict(tree,validate)

#Decision Tree with rPart
library(rpart)
tree1<-rpart(NSPF~lat+long+mag,train)
library(rpart.plot)
rpart.plot(tree1,extra=2)

#Predict
predict(tree1,validate)

#Misclassification Error for 'train' data
tab<-table(predict(tree),train$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)

#Misclassification Error for validate data

testpred<-predict(tree,newdata=validate)
tab<-table(testpred,validate$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)


