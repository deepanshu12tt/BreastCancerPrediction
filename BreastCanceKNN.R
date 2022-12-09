BreastCancer<-read.csv("Breast_cancer_data.csv",stringsAsFactors = FALSE)
str(BreastCancer)
View(BreastCancer)
table(BreastCancer$diagnosis)
summary(BreastCancer)
BreastCancer$diagnosis<-factor(BreastCancer$diagnosis, levels=c('0','1'))
round(prop.table(table(BreastCancer$diagnosis))*100,digits=1)
summary(BreastCancer[c("mean_radius","mean_perimeter","mean_smoothness")])
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
BreastCancer_n<-as.data.frame(lapply(BreastCancer[1:5],normalize))
summary(BreastCancer)
summary(BreastCancer_n)
set.seed("123")
Breast_sample<-sample(2,nrow(BreastCancer),replace = TRUE,prob = c(0.80,0.20))
Breast_sample
Breast_train<-BreastCancer_n[Breast_sample==1, 1:5]
Breast_test<-BreastCancer_n[Breast_sample==2, 1:5]
Breast_ltrain<-BreastCancer[Breast_sample==1, 6]
Breast_ltest<-BreastCancer[Breast_sample==2, 6]
library(class)
model<-knn(train = Breast_train,test=Breast_test,cl=Breast_ltrain,k=23)
model
library(gmodels)
CrossTable(x=Breast_ltest, y=model, p.chisq=FALSE)
table<-table(model, Breast_ltest)
sum(diag(table)/sum(table))
1-sum(diag(table)/sum(table))
