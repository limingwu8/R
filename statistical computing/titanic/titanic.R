setwd("C:/Users/Administrator/Desktop/statistical computing/titanic")
titanic.train = read.csv(file = 'train.csv',stringsAsFactors = FALSE,header = T)
titanic.test = read.csv(file = 'test.csv',stringsAsFactors = FALSE,header = T)

# in order to distinguish training data and testing data after combined them toghther.
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE
titanic.test$Survived <- NA

# bind training data and testing data
titanic.full <- rbind(titanic.train,titanic.test)

# fill missing values
titanic.full[titanic.full$Embarked=='',"Embarked"] <- 'S'
titanic.full[is.na(titanic.full$Age),'Age'] <- median(titanic.full$Age,na.rm = TRUE)
table(is.na(titanic.full$Fare))
titanic.full[is.na(titanic.full$Fare),'Fare'] <- median(titanic.full$Fare,na.rm = TRUE)

# categorical casting
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Cabin <- as.factor(titanic.full$Cabin)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

# read back training data and testing data from full data
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

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
write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)
