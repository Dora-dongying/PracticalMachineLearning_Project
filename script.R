library(caret)
## Download the CSV file and then unzip it. Check if the files exist before processing.
trainingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
if (!file.exists("training.csv")){
        download.file(trainingURL, "training.csv", method = "curl")
}
testingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if (!file.exists("testing.csv")){
        download.file(testingURL, "testing.csv", method = "curl")
}

## Read data into R
training <- read.csv("training.csv", na.strings=c("NA","#DIV/0!", ""))
testing <- read.csv("testing.csv", na.strings=c("NA","#DIV/0!", ""))

## Delete the NA cols and irrelavent cols
training <- training[,colSums(is.na(training)) == 0]
testing <- testing[,colSums(is.na(testing)) == 0]
training   <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]
training$classe <- as.factor(training$classe)
testing$classe <- as.factor(testing$classe)
## Create validation data from the training data
set.seed(123)
inValid <- createDataPartition(training$classe, p = 0.3, list = FALSE)
validation <- training[inValid,]
training_train <- training[-inValid,]
dim(validation)
dim(training_train)

## Training the data using "classe" as the outcome
modFit_rf <- randomForest(classe ~., data = training_train, method = "class")
pred_rf <- predict(modFit_rf, newdata = validation)
confusionMatrix(pred_rf, validation$classe)

library(rpart)
library(rattle)
modFit_rpart <- rpart(classe ~., data = training_train, method = "class")
fancyRpartPlot(modFit_rpart)
pred_rpart <- predict(modFit_rpart, newdata = validation, type = "class")
confusionMatrix(pred_rpart, validation$classe)

pred_testing <- predict(modFit_rf, newdata = testing)
pred_testing
