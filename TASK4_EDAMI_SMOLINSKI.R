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
# Main questions which should be answered based on experiments:
# - what atrribute decides to which class certain object has been attached to
# - what are the criteria for measurement of classifier's quality?
# - potential practical application of whole idea of classifying objects?

# Downloading data - i would use red wine dataset

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');

red_wine = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")

#red_wine

# Some information about dataset

summary(red_wine)
View(red_wine)
set.seed(2137)


# Creating a cat attribute with 3 values(classses) based on 
# the orignal class atribute - quality

red_wine$cat = lapply(red_wine[,12], function (x)
{
  if(x >6)  { "A"}
  else if(x >4)  {"B"}
  else { "C"} 
})

red_wine$cat = unlist(red_wine$cat)
red_wine$cat = as.factor(red_wine$cat)

# Show how does the table with content looks like
View(red_wine)

?unlist
?as.factor

# Remove quality column

red_wine <- red_wine[,-12]


# Basic check of our dataset's parameters and their distribution #

# Creating data partitions (test and training) for our basic dataset and the quality attribute
nrow(red_wine)
?sample
# sam <- sample(2, nrow(red_wine), replace=TRUE, prob=c(0.7, 0.3))
# sam
# red_wine_train <- red_wine[sam==1,]
# red_wine_test <- red_wine[sam==2,]

# Class distribution in sets
# prop.table(table(red_wine_train$quality))
# prop.table(table(red_wine_test$quality))

# Creating data partitions (test and training) for our modified dataset and the category attribute 
?createDataPartition
red_wine_category <- unlist(createDataPartition(red_wine$cat,p=0.7))
#str(red_wine_category)

red_wine_train_cat <-red_wine[red_wine_category,]
red_wine_test_cat <-red_wine[-red_wine_category,]

# Class distribution in sets
prop.table(table(red_wine_train_cat$cat))
prop.table(table(red_wine_test_cat$cat))

table(red_wine_train_cat$cat)
table(red_wine_test_cat$cat)


##################################################
# Experiments with C5.0 classifier               #
##################################################

View(red_wine)
?C5.0
c5_red_wine<-C5.0(red_wine_train_cat[,-12], red_wine_train_cat$cat)
summary(c5_red_wine)
plot(c5_red_wine)

# Data summary

# Decision tree:

# Train

# A (152)
# B (924)
# C (45)


# Test

# A (65)
# B (395)
# C (18)

##### Training data - no rules #####

# Quality of classification for training data
c5_red_wine_trainPredi <- predict(c5_red_wine, red_wine_train_cat)

?CrossTable
CrossTable(c5_red_wine_trainPredi, red_wine_train_cat$cat, prop.chisq = FALSE,prop.c = FALSE,
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))
# 
# 
# # Total Observations in Table:  1121 
# # 
# # 
# #                 | actual class 
# # predicted class |         A |         B |         C | Row Total | 
# # ----------------|-----------|-----------|-----------|-----------|
# #               A |        95 |         9 |         0 |       104 | 
# #                 |     0.085 |     0.008 |     0.000 |           | 
# # ----------------|-----------|-----------|-----------|-----------|
# #               B |        57 |       915 |        29 |      1001 | 
# #                 |     0.051 |     0.816 |     0.026 |           | 
# # ----------------|-----------|-----------|-----------|-----------|
# #               C |         0 |         0 |        16 |        16 | 
# #                 |     0.000 |     0.000 |     0.014 |           | 
# # ----------------|-----------|-----------|-----------|-----------|
# #    Column Total |       152 |       924 |        45 |      1121 | 
# # ----------------|-----------|-----------|-----------|-----------|
#  
# 
?confusionMatrix
confusionMatrix(c5_red_wine_trainPredi, red_wine_train_cat$cat, mode="everything")
# 
#  #              Reference
#  # Prediction   A   B   C
#  #          A  95   9   0
#  #          B  57 915  29
#  #          C   0   0  16
# 
#               Accuracy : 0.9153          
#                 95% CI : (0.8974, 0.9309)
#    No Information Rate : 0.8243          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                  Kappa : 0.6621  
# 
#  Statistics by Class:
#    
#                      Class: A Class: B Class: C
#  
#  
# Precision             0.91346   0.9141  1.00000
# Recall                0.62500   0.9903  0.35556
# F1                    0.74219   0.9506  0.52459
# 

##### Test data - no rules #####

# Quality of classification for test data

c5_red_wine_testPredi <- predict(c5_red_wine, red_wine_test_cat)

CrossTable(c5_red_wine_testPredi, red_wine_test_cat$cat, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))


# Total Observations in Table:  478 
# 
# 
#                 | actual class 
#   predicted class |         A |         B |         C | Row Total | 
#   ----------------|-----------|-----------|-----------|-----------|
#                 A |        25 |        16 |         1 |        42 | 
#                   |     0.052 |     0.033 |     0.002 |           | 
#   ----------------|-----------|-----------|-----------|-----------|
#                 B |        40 |       371 |        16 |       427 | 
#                   |     0.084 |     0.776 |     0.033 |           | 
#   ----------------|-----------|-----------|-----------|-----------|
#                 C |         0 |         8 |         1 |         9 | 
#                   |     0.000 |     0.017 |     0.002 |           | 
#   ----------------|-----------|-----------|-----------|-----------|
#      Column Total |        65 |       395 |        18 |       478 | 
#   ----------------|-----------|-----------|-----------|-----------|



confusionMatrix(c5_red_wine_testPredi, red_wine_test_cat$cat, mode="everything")


#               Reference
# Prediction   A   B   C
#          A  25  16   1
#          B  40 371  16
#          C   0   8   1

#             Accuracy : 0.8305            
#               95% CI : (0.7938, 0.8631)
#  No Information Rate : 0.8264     
#  P-Value [Acc > NIR] : 0.433160
#                Kappa : 0.3199

# Statistics by Class:
#   
#                       Class: A Class: B Class: C
# Precision             0.59524   0.8689 0.111111
# Recall                0.38462   0.9392 0.055556
# F1                    0.46729   0.9027 0.074074


######################################################

##### Rules #####
red_wine_train_cat
# Model building - rules
c5_red_wine_rules <- C5.0(red_wine_train_cat[,-12], red_wine_train_cat$cat,  rules = TRUE) 
summary(c5_red_wine_rules)

c5_red_wine_testPredi <- predict(c5_red_wine_rules, red_wine_test_cat)

CrossTable(c5_red_wine_testPredi, red_wine_test_cat$cat, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

confusionMatrix(c5_red_wine_testPredi, red_wine_test_cat$cat, mode="everything")

# Discovered rules

# 20 rules have been detected 

# There have been 114 errors, which stands for 10.2% of certain dataset 

# Alcohol level seems to be the most used attribute to determine the class of an object and it's usage was 90.72%
# Second on the list was volatile.acidity with 79.48% usage and third place belongs to free.sulfur.dioxide with 72.35%


# The least used attribute was pH and density, having 2.68% and 3.39% respectively

  #                 | actual class 
  # predicted class |         A |         B |         C | Row Total | 
  # ----------------|-----------|-----------|-----------|-----------|
  #               A |        27 |        21 |         1 |        49 | 
  #                 |     0.056 |     0.044 |     0.002 |           | 
  # ----------------|-----------|-----------|-----------|-----------|
  #               B |        38 |       371 |        17 |       426 | 
  #                 |     0.079 |     0.776 |     0.036 |           | 
  # ----------------|-----------|-----------|-----------|-----------|
  #               C |         0 |         3 |         0 |         3 | 
  #                 |     0.000 |     0.006 |     0.000 |           | 
  # ----------------|-----------|-----------|-----------|-----------|
  #    Column Total |        65 |       395 |        18 |       478 | 
  # ----------------|-----------|-----------|-----------|-----------|

#             Reference
# Prediction   A   B   C
#          A  27  21   1
#          B  38 371  17
#          C   0   3   0

# Overall Statistics
# 
#            Accuracy : 0.8326         
#              95% CI : (0.7961, 0.865)
# No Information Rate : 0.8264         
# P-Value [Acc > NIR] : 0.385978       
#               Kappa : 0.3288


# Statistics by Class:

# Precision             0.55102   0.8709 0.000000
# Recall                0.41538   0.9392 0.000000
# F1                    0.47368   0.9038      NaN


##################################################
# Recursive partitioning trees              #
##################################################

# Setting the class attribute from the rest of dataset's attributes
red_wine_class <-  cat ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol

# View(red_wine)
# red_wine
?rpart
print(red_wine_rpart)
summary(red_wine_rpart)

# plot(red_wine_rpart, main="Classification for red wine")
# text(red_wine_rpart, use.n=TRUE, all=TRUE, cex=.7)
# rpart.plot(red_wine_rpart, main="Classification for red wine")

red_wine_rpart <- rpart(red_wine_class, method="class", data = red_wine_train_cat)
print(red_wine_rpart)


# Training data classification - confusion matrix 
red_wine_rpart_trainPredi = predict(red_wine_rpart,red_wine_train_cat,type = "class")
table(red_wine_rpart_trainPredi, red_wine_train_cat$cat)

# Test data classification - confusion matrix 
red_wine_rpart_testPredi = predict(red_wine_rpart,red_wine_test_cat,type = "class")
table(red_wine_rpart_testPredi, red_wine_test_cat$cat)

confusionMatrix(red_wine_rpart_testPredi, red_wine_test_cat$cat, mode="everything")


#              Reference
# Prediction   A   B   C
#          A  35  22   0
#          B  30 368  17
#          C   0   5   1

# Overall Statistics

# Accuracy : 0.8452          
# 95% CI : (0.8096, 0.8764)
# No Information Rate : 0.8264          
# P-Value [Acc > NIR] : 0.1521          
# 
# Kappa : 0.4177 

# Statistics by Class:

# Precision             0.61404   0.8867 0.166667
# Recall                0.53846   0.9316 0.055556
# F1                    0.57377   0.9086 0.083333


##### LOSS MATRIX #####

# Loss matrix - a row the actual class, a column - predicted class 
# Example:
#              Reference
# Prediction   A   B   C
#          A  27  21   1
#          B  38 371  17
#          C   0   3   0
#
# Our loss matrix:
#       [,1] [,2] [,3]
# [1,]    0    1    1
# [2,]    3    0    1
# [3,]    4    3    0
#
# How to read: classifying A-rated wine as lower quality, for example C, is 4 times more unwelocme than classyfing
# low quality wine C as an A-rated wine. 
# In this case we can assume that this model (or quite simillar to it) can be helpful for distributors or shops selling wine 
# - it's better to have low quality wine classified better than it is in reality than having better wine classifed as a low quality product

lossM=matrix(c(0,1,1,3,0,1,4,3,0), byrow=TRUE, nrow=3)
lossM
red_wine_rpartLM <-  rpart(red_wine_class,  method="class", data=red_wine_train_cat, parms = list(loss = lossM ))

# # Training data classification - confusion matrix 
# red_wine_rpatLM_trainPred = predict(red_wine_rpartLM,red_wine_train_cat,type = "class")
# table(red_wine_rpatLM_trainPred, red_wine_train_cat$cat)

# Test data classification - confusion matrix 
red_wine_rpatLM_testPred = predict(red_wine_rpartLM,red_wine_test_cat,type = "class")
table(red_wine_rpatLM_testPred, red_wine_test_cat$cat)
confusionMatrix(red_wine_rpatLM_testPred, red_wine_test_cat$cat, mode="everything")

#               Reference
# Prediction   A   B   C
#              A   8   5   0
#              B  57 369  15
#              C   0  21   3


# Overall Statistics
# 
#            Accuracy : 0.795
#              95% CI : (0.756, 0.8303)
# No Information Rate : 0.8264
# P-Value [Acc > NIR] : 0.9672
# 
#               Kappa : 0.1164


# Statistics by Class:

# Precision             0.61538   0.8367 0.125000
# Recall                0.12308   0.9342 0.166667
# F1                    0.20513   0.8828 0.142857




##### DIFFERENT PARAMETERS #####

# Changing the values of parameters
rpControl = rpart.control(minbucket =30, maxDepth = 1);
rpTree <- rpart(red_wine_class,  method="class", data=red_wine_train_cat,
                control =rpControl,
                parms = list(split = "information" ))
rpart.plot(rpTree, main="Classification for red wine")

red_wine_rpartS = predict(rpTree,red_wine_test_cat,type = "class")
table(red_wine_rpartS, red_wine_test_cat$cat)
confusionMatrix(red_wine_rpartS, red_wine_test_cat$cat, mode="everything")

#              Reference
# Prediction       A   B   C
#              A  19  15   0
#              B  46 380  18
#              C   0   0   0

# Overall Statistics
# 
#                 Accuracy : 0.8347          
#                   95% CI : (0.7983, 0.8669)
#      No Information Rate : 0.8264          
#      P-Value [Acc > NIR] : 0.3403          
# 
#                    Kappa : 0.258   

# Statistics by Class:
#   
#                       Class: A Class: B Class: C
# Precision             0.55882   0.8559       NA
# Recall                0.29231   0.9620  0.00000
# F1                    0.38384   0.9058       NA
##### TREE PRUNING #####

# The minimal  cross-validation error 
min(red_wine_rpartLM$cptable[,"xerror"])
which.min(red_wine_rpartLM$cptable[,"xerror"])
rpTree.cp=red_wine_rpartLM$cptable[3,"CP"]
rpTree.cp
?prune
red_wine_rpartLM_Pruned<- prune(red_wine_rpartLM, cp = rpTree.cp)


#red_wine_rpartLM_Pruned <- prune(red_wine_rpartLM, cp = rpTree$cptable[which.min(rpTree$cptable[,"xerror"]),"CP"])

red_wine_rpart_pruned = predict(red_wine_rpartLM_Pruned,red_wine_test_cat,type = "class")
table(red_wine_rpart_pruned, red_wine_test_cat$cat)
confusionMatrix(red_wine_rpart_pruned, red_wine_test_cat$cat, mode="everything")

#              Reference
# Prediction   A   B   C
#              A  11   9   0
#              B  54 381  17
#              C   0   5   1

# Overall Statistics
# 
#               Accuracy : 0.8222          
#                 95% CI : (0.7849, 0.8554)
#    No Information Rate : 0.8264          
#    P-Value [Acc > NIR] : 0.6231          
# 
#                  Kappa : 0.1629

# Statistics by Class:
#                      Class: A Class: B Class: C
# Precision             0.55000   0.8429 0.166667
# Recall                0.16923   0.9646 0.055556
# F1                    0.25882   0.8996 0.083333




rpart.plot(red_wine_rpartLM, main="Classification for red wine")
rpart.plot(red_wine_rpartLM_Pruned, main="Classification for red wine - pruned")

##################################################
# RANDOM FOREST                                  #
##################################################

red_wine_Forest = randomForest(cat~., data = red_wine_train_cat, importance = TRUE, nodesize = 10, mtry = 4, ntree = 100 )

print(red_wine_Forest)
plot(red_wine_Forest)

?importance
round(importance(red_wine_Forest, type = 1),2)

red_wine_Forest_testPred = predict (red_wine_Forest, newdata = red_wine_test_cat[,-12])
confusionMatrix(red_wine_Forest_testPred, red_wine_test_cat$cat, mode = "everything")

# Take a look at the stats
# Type of random forest: classification
# Number of trees: 100
# No. of variables tried at each split: 4
# OOB estimate of  error rate: 14.54%

# Confusion matrix:
#   A   B C class.error
# A 66  86 0  0.56578947
# B 31 888 5  0.03896104
# C  0  41 4  0.91111111


#             Reference
# Prediction   A   B   C
#              A  28  14   0
#              B  37 379  18
#              C   0   2   0

# Overall Statistics
# 
#              Accuracy : 0.8515          
#                95% CI : (0.8164, 0.8821)
#   No Information Rate : 0.8264          
#   P-Value [Acc > NIR] : 0.08046         
# 
#                 Kappa : 0.3749   

# Statistics by Class:
#                       Class: A Class: B Class: C
# Precision             0.66667   0.8733 0.000000
# Recall                0.43077   0.9595 0.000000
# F1                    0.52336   0.9144      NaN


# It seems there is some problem with classification objects to class C



##### TUNNING #####

# Looking for the best values of parameters by means of K-fold validation
?trainControl
trControl <- trainControl(method = "cv", number = 10, search = "grid")


?train
tuneGrid <- expand.grid(mtry = c(1:6))
tuneGrid
red_wine_Frestores_mtry <- train(cat~.,  data = red_wine_train_cat,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tuneGrid,
                            trControl = trControl,
                            importance = TRUE,    # randomForest function parameter
                            nodesize = 10,        # randomForest function parameter
                            ntree = 250)          ## randomForest function parameter
print(red_wine_Frestores_mtry)



treesModels <- list()
for (nbTree in c(5,10,25, 50, 100, 250, 500)) 
{
  red_wine_F_maxtrees <- train(cat~.,  data = red_wine_train_cat,
                          method = "rf",
                          metric = "Accuracy",
                          tuneGrid = tuneGrid,
                          trControl = trControl,
                          importance = TRUE,
                          nodesize = 10,
                          ntree = nbTree)
  key <- toString(nbTree)
  treesModels[[key]] <- red_wine_F_maxtrees
}

?resamples
results_tree <- resamples(treesModels)
summary(results_tree)

red_wine_Forest_tunned = randomForest(cat~., data = red_wine_train_cat, importance = TRUE, nodesize = 10, mtry = 4, ntree = 100 )

?importance
round(importance(red_wine_Forest_tunned, type = 1),2)

red_wine_Forest_tunned_testPred = predict (red_wine_Forest_tunned, newdata = red_wine_test_cat[,-12])
confusionMatrix(red_wine_Forest_tunned_testPred, red_wine_test_cat$cat, mode = "everything")

#              Reference
# Prediction   A   B   C
#          A  29  10   0
#          B  36 383  18
#          C   0   2   0

# Overall Statistics
# 
#                   Accuracy : 0.8619          
#                     95% CI : (0.8277, 0.8916)
#        No Information Rate : 0.8264          
#        P-Value [Acc > NIR] : 0.02088         
# 
#                      Kappa : 0.4081

# Statistics by Class:
#                      Class: A Class: B Class: C
# Precision             0.74359   0.8764 0.000000
# Recall                0.44615   0.9696 0.000000
# F1                    0.55769   0.9207      NaN



# Results
# Resampling results across tuning parameters:
  
#   mtry  Accuracy   Kappa    
# 1     0.8491970  0.2530727
# 2     0.8536773  0.3303764
# 3     0.8598876  0.3860472
# 4     0.8474192  0.3538253
# 5     0.8500898  0.3655154
# 6     0.8483041  0.3730153

  
#   Models: 5, 10, 25, 50, 100, 250, 500 
# Number of resamples: 10 
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# 5   0.7927928 0.8236607 0.8355721 0.8376367 0.8542813 0.8750000    0
# 10  0.8070175 0.8333533 0.8603604 0.8554944 0.8816964 0.8938053    0
# 25  0.8392857 0.8415179 0.8571429 0.8563685 0.8647282 0.8839286    0
# 50  0.8392857 0.8485501 0.8577750 0.8617553 0.8719233 0.8928571    0
# 100 0.8318584 0.8434282 0.8616071 0.8608917 0.8722196 0.9017857    0
# 250 0.8407080 0.8471887 0.8526786 0.8545859 0.8580910 0.8839286    0
# 500 0.8141593 0.8404117 0.8738739 0.8617908 0.8755481 0.8938053    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# 5   0.1132338 0.2454433 0.3613355 0.3372815 0.4315146 0.5261408    0
# 10  0.2517695 0.3299101 0.4128866 0.4203921 0.5047894 0.5803157    0
# 25  0.2460733 0.3420344 0.4264916 0.4011925 0.4557729 0.5030717    0
# 50  0.1663748 0.3289337 0.3749987 0.3713308 0.4198688 0.5549669    0
# 100 0.2329403 0.3464994 0.4212175 0.4120265 0.5046656 0.5541079    0
# 250 0.3075229 0.3673107 0.3862009 0.3934671 0.4285023 0.4709302    0
# 500 0.2682256 0.3569804 0.4526883 0.4223163 0.4935438 0.5298197    0

##### FINAL MODEL #####

red_wine_Forest2 = randomForest(cat~., data = red_wine_train_cat, importance = TRUE, mtry = 6, ntree = 250, nodesize = 10)

print(red_wine_Forest2)
plot(red_wine_Forest2)


red_wine_Forest2_testPred = predict (red_wine_Forest, newdata = red_wine_test_cat[,-12])
confusionMatrix(red_wine_Forest2_testPred, red_wine_test_cat$cat, mode = "everything")

varImpPlot(red_wine_Forest2)

# OOB estimate of  error rate: 14.81%
# Confusion matrix:
#   A   B C class.error
# A 66  86 0  0.56578947
# B 35 885 4  0.04220779
# C  0  41 4  0.91111111

#               Reference
# Prediction   A   B   C
#          A  29  14   0
#          B  36 379  18
#          C   0   2   0

# Overall Statistics
# 
#             Accuracy : 0.8536         
#               95% CI : (0.8186, 0.884)
#  No Information Rate : 0.8264         
#  P-Value [Acc > NIR] : 0.06328        
# 
#                Kappa : 0.3874     

# Statistics by Class:
#                       Class: A Class: B Class: C
# Precision             0.67442   0.8753 0.000000
# Recall                0.44615   0.9595 0.000000
# F1                    0.53704   0.9155      NaN




##################################################
# NAIVE BAYES CLASSIFICATOR                      #
##################################################


red_wine_naivebayesClass <- naiveBayes(red_wine_class, data=red_wine_train_cat, laplace = 0)

print(red_wine_naivebayesClass)


red_wine_test_naive_pred = predict(red_wine_naivebayesClass,red_wine_train_cat)
table(red_wine_train_cat$cat,red_wine_test_naive_pred)
#     A   B   C
# A  40  27   0
# B  54 321  15
# C   1  17   4
length(is.na(red_wine_test_naive_pred))
confusionMatrix(red_wine_train_cat$cat, red_wine_test_naive_pred,mode="everything")
anyNA(red_wine_test_naive_pred)

# A-priori probabilities:
#   Y
# A          B          C 
# 0.13559322 0.82426405 0.04014273 


#              Reference
# Prediction   A   B   C
#          A 100  51   1
#          B 124 765  35
#          C   1  33  11

# Overall Statistics
# 
#                 Accuracy : 0.7814          
#                   95% CI : (0.7561, 0.8053)
#      No Information Rate : 0.7574          
#      P-Value [Acc > NIR] : 0.03133         
# 
#                    Kappa : 0.3699

# Statistics by Class:

#                       Class: A Class: B Class: C
# Precision             0.65789   0.8279 0.244444
# Recall                0.44444   0.9011 0.234043
# F1                    0.53050   0.8629 0.239130


##### COMPARISION #####

# red_wine_rpart <- rpart(cat~., data=red_wine_train_cat)
# red_wine_rpart_testPred = predict(red_wine_rpart, red_wine_test_cat, type = "class")

klasyfikator = c('C50', 'rpart',  'rForest no tunning', 'Naive Bayes', 'rpart with loss matrix', 'rpart with changed parameters', 'rForest tunned', 'rForest full')
dokladnosc = c( mean(c5_red_wine_testPredi == red_wine_test_cat$cat), 
                mean(red_wine_rpart_testPredi == red_wine_test_cat$cat),
                mean(red_wine_Forest_testPred == red_wine_test_cat$cat),
                mean(red_wine_test_naive_pred == red_wine_test_cat$cat),
                mean(red_wine_rpatLM_testPred == red_wine_test_cat$cat),
                mean(red_wine_rpartS == red_wine_test_cat$cat),
                mean(red_wine_Forest_tunned_testPred == red_wine_test_cat$cat),
                mean(red_wine_Forest2_testPred == red_wine_test_cat$cat))

res <- data.frame(klasyfikator, dokladnosc)
View(res)

red_wine_test_naive_pred
red_wine_test_cat$cat
red_wine_rpatLM_testPred
red_wine_rpartS
c5_red_wine_testPredi
##### CONCLUSIONS #####

# 1 C50	0.8326360
# 2	rpart	0.8451883
# 3	rForest no tunning	0.8514644
# 4	Naive Bayes	0.6663693
# 5	rpart with loss matrix	0.7949791
# 6	rpart with changed parameters	0.8347280
# 7	rForest tunned	0.8640167
# 8	rForest full	0.8514644

