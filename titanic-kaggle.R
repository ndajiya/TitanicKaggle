setwd("C:/Users/Nda-Jiya/Desktop/The Titanic")

titanic.train <- read.csv(file ="train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file ="test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train, titanic.test)

titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

age.median <- median(titanic.full$Age, na.rm = TRUE)

titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

table(is.na(titanic.full$Fare))

#Clean missing values of Fare
fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median


#categorical casting (must convert Pclass to an ordinal category(.order) 
#must consider SibSp and Parch to be Ordinal and then assign it back in as below)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

# split dataset back out into train and test
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]

#Allows for binary classification not regression or multi-class
titanic.train$Survived <- as.factor(titanic.train$Survived)

#Formula to define what to use
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)

#Must do 70/30 split and cross-validation
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

#Defining features
features.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

