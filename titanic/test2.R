setwd("C:/Users/Administrator/Desktop/statistical computing/titanic")
trainingData = read.csv(file = 'train.csv',stringsAsFactors = FALSE,header = T)
testingData = read.csv(file = 'test.csv',stringsAsFactors = FALSE,header = T)

# in order to distinguish training data and testing data after combined them toghther.
trainingData$IsTrainSet <- TRUE
testingData$IsTrainSet <- FALSE
testingData$Survived <- NA

# bind training data and testing data
allData <- rbind(trainingData,testingData)


# fill missing Embarked
allData[allData$Embarked=='','Embarked'] = NA


# categorical casting
allData$Sex <- as.factor(allData$Sex)
allData$Cabin <- as.factor(allData$Cabin)
allData$Embarked <- as.factor(allData$Embarked)

# read back training data and testing data from full data
trainingData = allData[!is.na(allData$Embarked),]
testingData = allData[is.na(allData$Embarked),]

trainingData$Embarked <- as.factor(trainingData$Embarked)



embarkedEquation <- "Embarked ~ Pclass + Sex + SibSp"
embarkedFormula <- as.formula(embarkedEquation)


# construct prediction model
embarkedModel <- randomForest(formula = embarkedFormula,data=trainingData, ntree = 500, mtry = 3, proximity=TRUE)

featuresEmbark <- "Pclass + Sex + SibSp"

embarked <- predict(embarkedModel,newdata = testingData)
