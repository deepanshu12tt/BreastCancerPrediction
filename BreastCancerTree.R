BreastCancer<-read.csv("Breast_cancer_data.csv",stringsAsFactors = FALSE)
View(BreastCancer)
str(BreastCancer)
BreastCancer$diagnosis<-factor(BreastCancer$diagnosis)
str(BreastCancer)
set.seed(1234)
pd<-sample(2,nrow(BreastCancer),replace=TRUE,prob=c(0.8,0.2))
train<-BreastCancer[pd==1,]
train
test<-BreastCancer[pd==2,]
test
library(party)
tree<-ctree(diagnosis~.,data = train,controls = ctree_control(mincriterion = 0.90,minbucket = 100))
tree
plot(tree)
predict(tree,test,type="prob")
library(rpart)
tree1<-rpart(diagnosis~.,train)
library(rpart.plot)
rpart.plot(tree1)
predict(tree1,test)
table<-table(predict(tree),train$diagnosis)
table
1-sum(diag(table)/sum(table))
predtest<-predict(tree,test)
table<-table(predtest,test$diagnosis)
table
1-sum(diag(table)/sum(table))
