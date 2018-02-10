library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
trainUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainFile <- "C:/Users/quintieri.a/Desktop/R-Course/machineLearning/pml-training.csv"
testFile  <- "C:/Users/quintieri.a/Desktop/R-Course/machineLearning/pml-testing.csv"
if (!file.exists(trainFile)) {
  download.file(trainUrl, destfile=trainFile, method="curl")
}
if (!file.exists(testFile)) {
  download.file(testUrl, destfile=testFile, method="curl")
}
trainRaw <- read.csv("C:/Users/quintieri.a/Desktop/R-Course/machineLearning/pml-training.csv")
testRaw <- read.csv("C:/Users/quintieri.a/Desktop/R-Course/machineLearning/pml-testing.csv")
dim(trainRaw)
sum(complete.cases(trainRaw))
trainRaw <- trainRaw[, colSums(is.na(trainRaw)) == 0] 
testRaw <- testRaw[, colSums(is.na(testRaw)) == 0] 
classe <- trainRaw$classe
trainRemove <- grepl("^X|timestamp|window", names(trainRaw))
trainRaw <- trainRaw[, !trainRemove]
trainCleaned <- trainRaw[, sapply(trainRaw, is.numeric)]
trainCleaned$classe <- classe
testRemove <- grepl("^X|timestamp|window", names(testRaw))
testRaw <- testRaw[, !testRemove]
testCleaned <- testRaw[, sapply(testRaw, is.numeric)]
set.seed(22519) # For reproducibile purpose
inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[inTrain, ]
testData <- trainCleaned[-inTrain, ]
controlRf <- trainControl(method="cv", 5)
modelRf <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=25) # ntree is very small, above 25 make the calculation very long
modelRf
predictRf <- predict(modelRf, testData)
confusionMatrix(testData$classe, predictRf)
accuracy <- postResample(predictRf, testData$classe)
accuracy
oose <-  1 - as.numeric(confusionMatrix(testData$classe, predictRf)$overall[1])
oose
result <- predict(modelRf, testCleaned[, -length(names(testCleaned))])
result
corrPlot <- cor(trainData[, -length(names(trainData))])
corrplot(corrPlot, method="color")
treeModel <- rpart(classe ~ ., data=trainData, method="class")
prp(treeModel) # fast plot

