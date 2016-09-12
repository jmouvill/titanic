library(magrittr)
library(ggplot2)
library(GGally)
library(dplyr)
library(plyr)
library(data.table)  
library(caret)
library(randomForest)
library(rpart)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)


train <- fread("/Users/Jean/Documents/workspace/R/titanic/train.csv")
test <- fread("/Users/Jean/Documents/workspace/R/titanic/test.csv")
test$Survived <- NA
test %>% head
train %>% str

combi <- rbind(train,test)
combi$Title <- (sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}))
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Embarked[which(combi$Embarked == '')] = "S"
combi$Embarked <- factor(combi$Embarked)
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm=TRUE)


train <- combi[combi$PassengerId %in% train$PassengerId,]
test <- combi[combi$PassengerId %in% test$PassengerId,]

smp_size <- floor(0.80 * nrow(train))
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

trainSet <- train[train_ind, ]
crossSet <- train[-train_ind, ]

set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize ,
                    data=trainSet, 
                    importance=TRUE, 
                    ntree=2000)

