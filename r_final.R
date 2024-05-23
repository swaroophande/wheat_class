install.packages("Metrics")
install.packages("caret")
install.packages("ggplot2")

library(stats)
library(dplyr)
library(randomForest)
library(tidyr)
library(reprtree)
library(e1071)
library(Rgraphviz)
library(caret)
library(ggplot2)
library(Metrics)
library(igraph)

Seed_Data <- read.csv("seeds_new.csv")
View(Seed_Data)

#Randomizing the sample 
Random_seed = Seed_Data[sample(1:nrow(Seed_Data)),]

#Saving random file
write.csv(Random_seed,"seed_out.csv")

#Loading the Randomized file
Seed_Data2 <- read.csv("seed_out.csv")
View(Seed_Data2)

#Visualization of dataset
#1.Bar plot
ggplot(Seed_Data2, aes(x=Type, fill=Type))+
geom_bar()+ 
labs(x="Type of Wheats" ,y="Total count")+ theme_bw()

#2.Heat map
data_long <- Seed_Data %>% 
gather(variable,value, -Type)

ggplot(data_long, aes(x=variable, y=Type, fill=value))+
geom_tile()+scale_fill_gradient(low ="yellow",high="red")+
labs(x="Parameters" ,y="Type of Wheat",fill="Value")+ theme_bw()

#3.Jitter
ggplot(data_long, aes(x=value, y=Type,))+geom_jitter(aes(fill=value),size =0.5)+
scale_shape_manual(values =1:7)+
labs(x="Parameter and value" ,y="Type of wheat",shape="Value")+theme_bw()+
facet_wrap(~variable,scales="free")

#Converting target into factor
Seed_Data2$Type <- as.character(Seed_Data2$Type)
Seed_Data2$Type <- as.factor(Seed_Data2$Type)

#Splitting data set for training and testing
training = Seed_Data2[1:147,]
test=Seed_Data2[148:210,]
View(training)
View(test)

#Visualization of training data
data_long1 <- training%>% 
gather(variable,value, -Type)

#1.jiiter
ggplot(data_long1, aes(x=variable, y=Type,))+
geom_jitter(aes(fill=value),size =0.5)+
scale_shape_manual(values =1:7)+
labs(x="Parameters" ,y="Type of Wheat",shape="Value")+theme_bw()

#2.Heatmap
ggplot(data_long1, aes(x=variable, y=Type, fill=value))+
  geom_tile()+
  scale_fill_gradient(low ="yellow",high="red")+
  labs(x="Parameters" ,y="Type of Wheat",fill="Value")+ theme_bw()+
  facet_wrap(~variable,scales="free")


#Visualization of test dataset
data_long2 <- test%>% 
gather(variable,value, -Type)

#1.Jitter
ggplot(data_long2, aes(x=variable, y=Type,))+
geom_jitter(aes(fill=value),size =0.5)+
scale_shape_manual(values =1:7)+
labs(x="Parameters" ,y="Type of Wheat",shape="Value")+theme_bw()

#2.Heatmap
ggplot(data_long2, aes(x=variable, y=Type, fill=value))+
geom_tile()+
scale_fill_gradient(low ="yellow",high="red")+
labs(x="Parameters" ,y="Type of Wheat",fill="Value")+ theme_bw()+
facet_wrap(~variable,scales="free")

#-------------------------------------------------------------------------------

#Random forest model
rfm<-randomForest(Type~., data=training)
rfm
plot(rfm)
reprtree:::plot.getTree(rfm)

#Evaluating Model Accuary
rfmTargetPred = predict(rfm, test)
test$rfmtarget_pred = rfmTargetPred

table(test$Type, test$rfmtarget_pred)
confusionMatrix(test$Type, test$rfmtarget_pred,mode="prec_recall")

#-------------------------------------------------------------------------------

#Naive bayes model
model =naiveBayes(Type~., data = training)
nbpred = predict(model, test)
test$nbtarget_pred = nbpred
table(test$Type, test$nbtarget_pred)

cpt <- model$tables
print(cpt)
confusionMatrix(test$Type, test$nbtarget_pred, mode="prec_recall")

#-------------------------------------------------------------------------------

#Support vector machine model
svmmodel=svm(Type~., data=training)
svmpred = predict(svmmodel, test)
test$svmtarget_pred=svmpred
table(test$Type, test$svmtarget_pred)

confusionMatrix(test$Type, test$svmtarget_pred, mode="prec_recall")

#-------------------------------------------------------------------------------

#Barplot of accuracy
rfma=accuracy(test$Type, test$rfmtarget_pred)*100
nba=accuracy(test$Type, test$nbtarget_pred)*100
svma=accuracy(test$Type, test$svmtarget_pred)*100
acc=c(rfma,nba,svma)
ynames=c("random forest", "Naive Bayes", "SVM")
barplot(acc, ylab="Accuracy", main="Accuracy graph",names.arg = ynames, ylim=c(90, 95),xpd=FALSE)
box(bty="]")




