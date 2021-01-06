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

#red_wine

# Some information about dataset

summary(red_wine)
View(red_wine)
set.seed(2137)


# Creating a new attribute with 3 values(classses) based on 
# the orignal class atribute - quality

red_wine$new = lapply(red_wine[,12], function (x)
{
  if(x >6)  { "A"}
  else if(x >4)  {"B"}
  else { "C"} 
}
)
red_wine$new = unlist(red_wine$new)
red_wine$new = as.factor(red_wine$new)

# Show how does the table with content looks like
View(red_wine)

?unlist
?as.factor

# Basic check of our dataset's parameters and their distribution #

# Creating data partitions (test and training) for our basic dataset and the quality attribute
nrow(red_wine)
?sample
sam <- sample(2, nrow(red_wine), replace=TRUE, prob=c(0.7, 0.3))
sam
red_wine_train <- red_wine[sam==1,]
red_wine_test <- red_wine[sam==2,]

# Class distribution in sets
prop.table(table(red_wine_train$quality))
prop.table(table(red_wine_test$quality))

# Creating data partitions (test and training) for our modified dataset and the category attribute 
?createDataPartition
red_wine_category <- unlist(createDataPartition(red_wine$new,p=0.7))
str(red_wine_category)

red_wine_train_cat <-red_wine[red_wine_category,]
red_wine_test_cat <-red_wine[-red_wine_category,]

# Class distribution in sets
prop.table(table(red_wine_train_cat$new))
prop.table(table(red_wine_test_cat$new))

table(red_wine_train_qual$new)
table(red_wine_test_qual$new)


##################################################
# Experiments with C5.0 classifier               #
##################################################













