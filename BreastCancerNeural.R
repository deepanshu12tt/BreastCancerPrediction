getwd()
bcd = read.csv("Breast_cancer_data.csv")
View(bcd)
str(bcd)
hist(bcd$diagnosis)
normalize <- function(x) { return((x - min(x)) /(max(x) - min(x)))}
bcd_norm <- as.data.frame(lapply(bcd,normalize))
summary(bcd)
summary(bcd_norm)
#install.packages("caTools")
library(caTools) 
set.seed(12)
split=sample.split(bcd_norm$diagnosis, SplitRatio = 0.75)
split
train=subset(bcd_norm, split==TRUE)
test=subset(bcd_norm, split==FALSE)
#install.packages("neuralnet")
library(neuralnet)
bcd_model <- neuralnet(diagnosis ~ ., data = train)
plot(bcd_model)
model_results = compute(bcd_model,test[1:5])
predicted <- model_results$net.result
predicted
cor(predicted,test$diagnosis)
bcd_model2 <- neuralnet(diagnosis ~ .,data =train,hidden =c(2,1))
plot(bcd_model2)
model_results2 <- compute(bcd_model2,test[1:5])
predicted2 <- model_results2$net.result
predicted2
cor(predicted2, test$diagnosis)

