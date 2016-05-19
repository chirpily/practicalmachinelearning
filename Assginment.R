#Load and assign the data files
setwd("D:/Document/Program learning/Coursera/Machine learning/Week 4")
pjTrain<-read.csv("training_file.csv", header=T, na.strings=c("NA", "#DIV/0!"))
pjTest<-read.csv("testing_file.csv", header=T, na.string=c("NA", "#DIV/0!"))
#Clean the data
noNApjTrain<-pjTrain[, apply(pjTrain, 2, function(x) !any(is.na(x)))] 
dim(noNApjTrain)
#An variables with user information, time and undefined
cleanpjTrain<-noNApjTrain[,-c(1:8)]
dim(cleanpjTrain)
cleanpjtest<-pjTest[,names(cleanpjTrain[,-52])]
dim(cleanpjtest)
#Start to do the data partition
library(caret)
inTrain<-createDataPartition(y=cleanpjTrain$classe, p=0.75,list=F)
training<-cleanpjTrain[inTrain,] 
test<-cleanpjTrain[-inTrain,] 
#Random forest trees were generated for the training dataset using cross-validation
set.seed(13333)
fitControl2<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
rffit<-train(classe~.,data=training, method="rf", trControl=fitControl2, verbose=F)
predrf<-predict(rffit, newdata=test)
confusionMatrix(predrf, test$classe)
pred20<-predict(rffit, newdata=cleanpmltest)
pred20
fitControl2<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
gmbfit<-train(classe~.,data=training, method="gbm", trControl=fitControl2, verbose=F)
gmbfit$finalModel
class(gmbfit)
predgmb<-predict(gmbfit, newdata=test)
confusionMatrix(predgmb, test$classe)
predtrain<-predict(gmbfit, newdata=training)
confusionMatrix(predtrain, training$classe)
predtrain<-predict(gmbfit, newdata=training)
confusionMatrix(predtrain, training$classe)

getwd()
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(pred20)

