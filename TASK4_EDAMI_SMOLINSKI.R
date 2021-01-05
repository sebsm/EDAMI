# Sebastian Smoliński
# 269056
# 20Z
# EDAMI
# Laboratory: Thursday 10-12
# CLassification task

library(gmodels) #results analysis
library(Hmisc) #results analysis
library(caret)
library(rpart) # rpart() - decision tree classifier
library(rpart.plot) 
library(e1071)
library(C50) # C5 classifer
library(randomForest)

# Aim of the experiments 

# The experiments would focus on discovering rules which append objects to their corresponding classes. 

# Downloading data - i would use red wine dataset

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');

red_wine = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")

red_wine

# Some information about dataset

summary(red_wine)
View(red_wine)
set.seed(2137)

# Creating training and test datasets 
nrow(red_wine)
?sample
sam <- sample(3, nrow(red_wine), replace=TRUE, prob=c(0.7, 0.3))
sam
red_wine_train <- red_wine[sam==1,]
red_wine_test <- red_wine[sam==2,]

# Class distribution in sets
prop.table(table(red_wine_train$quality))
prop.table(table(red_wine_test$quality))

# Creating training and test datasets 
?createDataPartition
red_wine_quality <- unlist(createDataPartition(red_wine$quality,p=0.7))
str(red_wine_quality)

red_wine_train_qual <-red_wine[red_wine_quality,]
red_wine_test_qual <-red_wine[-red_wine_quality,]

# Class distribution in sets
prop.table(table(red_wine_train_qual$category))
prop.table(table(red_wine_test_qual$category))

table(red_wine_train_qual$category)
table(red_wine_test_qual$category)

