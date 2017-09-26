setwd("C:/Users/Administrator/Desktop/statistical computing/titanic")
trainingData = read.csv(file = 'train.csv',stringsAsFactors = FALSE,header = T)
testingData = read.csv(file = 'test.csv',stringsAsFactors = FALSE,header = T)

# in order to distinguish training data and testing data after combined them toghther.
trainingData$IsTrainSet <- TRUE
testingData$IsTrainSet <- FALSE
testingData$Survived <- NA


# bind training data and testing data
allData <- rbind(trainingData,testingData)

# use regressing to predict missing Age
ageUpperOutlier <- boxplot.stats(allData$Age)$stats[5]
ageFilter <- allData$Age < ageUpperOutlier
ageEquation <- "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
ageModel <- lm(
  formula = ageEquation,
  data = allData
)
missingAgeRow <- allData[
  is.na(allData$Age),
  c("Pclass","Sex","Fare","SibSp","Parch","Embarked")
  ]
agePrediction <- predict(ageModel,missingAgeRow)
allData[is.na(allData$Age),"Age"] <- agePrediction

########### fill missing Fare ############
upper.whisker <- boxplot.stats(allData$Fare)$stats[5]
outlier.filter <- allData$Fare < upper.whisker
fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = allData[outlier.filter,]
)

missingFare.row <- allData[
  is.na(allData$Fare),
  c("Pclass","Sex","Age","SibSp","Parch","Embarked")
  ]
fare.predictions <- predict(fare.model,missingFare.row)
allData[is.na(allData$Fare),"Fare"] <- fare.predictions
##########################################



########### fill missing Embarked ############
allData[allData$Embarked=='','Embarked'] = NA


# categorical casting
allData$Sex <- as.factor(allData$Sex)
allData$Cabin <- as.factor(allData$Cabin)
allData$Embarked <- as.factor(allData$Embarked)

# read back training data and testing data from full data
embarkedTrainingData = allData[!is.na(allData$Embarked),]
embarkedTestingData = allData[is.na(allData$Embarked),]

embarkedTrainingData$Embarked <- as.factor(embarkedTrainingData$Embarked)

embarkedEquation <- "Embarked ~ Pclass + Sex + Age + Fare + SibSp"
embarkedFormula <- as.formula(embarkedEquation)

# construct prediction model
embarkedModel <- randomForest(formula = embarkedFormula,data=embarkedTrainingData, ntree = 500, mtry = 3, proximity=TRUE)
embarked <- predict(embarkedModel,newdata = embarkedTestingData)
allData[is.na(allData$Embarked),'Embarked'] <- embarked
######################################



# categorical casting
allData$Sex <- as.factor(allData$Sex)
allData$Cabin <- as.factor(allData$Cabin)
allData$Embarked <- as.factor(allData$Embarked)

# read back training data and testing data from full data
titanic.train <- allData[allData$IsTrainSet==TRUE,]
titanic.test <- allData[allData$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)

# construct prediction model
titanic.model <- randomForest(formula = survived.formula,data=titanic.train, ntree = 500, mtry = 3, nodesize = 0.01*nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Fare + Embarked"

Survived <- predict(titanic.model,newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- data.frame(PassengerId,Survived)
write.csv(output.df, file = "kaggle_submission2.csv", row.names = FALSE)
