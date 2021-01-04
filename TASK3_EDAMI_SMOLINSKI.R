# Sebastian Smoliński 
# 269056
# Laboratories: Thursday 10-12
# 20Z
# Clustering task

# loading necessary libraries
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)
library(fossil)
# Aim of the laboratory is to discover to which group belongs certain dataset observation. 
# I would try to discover the best method (and parameters) to group data provided in the red wine quality dataset

# Downloading the dataset 

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
red_wine = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")

# Description of dataset
# Input variables (based on physicochemical tests):
# 1 - fixed acidity
# 2 - volatile acidity
# 3 - citric acid
# 4 - residual sugar
# 5 - chlorides
# 6 - free sulfur dioxide
# 7 - total sulfur dioxide
# 8 - density
# 9 - pH
# 10 - sulphates
# 11 - alcohol

# Output variable (based on sensory data):
# 12 - quality (score between 0 and 10)

# So basically we deal with 11 numerical variables which look like example below:

# fixed.acidity volatile.acidity citric.acid residual.sugar chlorides free.sulfur.dioxide total.sulfur.dioxide density   pH   sulphates alcohol quality
# 7.4           0.70             0.00        1.9            0.076     11                  34                   0.9978    3.51 0.56      9.4     5

# The 12th variable is the quality of the wine - the number representing quality of the wine would be a result of our clustering experiments

# Calculating the accuracy

accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}

# Checking the ranges of values for each column
red_wine
summary(red_wine)
str(red_wine)
head(red_wine)
View(red_wine)
# We can observe that in the referenced quality data column we can see only numbers from 3 to 8 - this  gives 6 possible values.

# Setting seed value for RNG 
?set.seed
set.seed(123)

# Firstly, we will check how does Kmeans divide our observations
red_wine_no_quality <- red_wine[,-12]

####################################
# KMEANS                           #
####################################

?kmeans
wine_kmeans = kmeans(red_wine_no_quality,6)
result = table(red_wine$quality,wine_kmeans$cluster )
result
accuracyCalc(result,1)
print(wine_kmeans$iter)
print(wine_kmeans)
# Basic view
#     1   2   3   4   5   6
# 3   0   3   6   0   0   1
# 4   1   8  21   4   7  12
# 5  60 120 133 116 114 138
# 6   6 148 154  46 105 179
# 7   2  42  77  11  15  52
# 8   0   3   9   2   1   3

# Within cluster sum of squares by cluster:
# [1] 62670.46 27052.50 11111.19 36706.47 39366.67 16513.46
# (between_SS / total_SS =  89.9 %)

# Since there is problem with correct numbering of clusters, we would use rand.index instead of cluster.stats and corrected.rand

rand.index(red_wine$quality, wine_kmeans$cluster)

# Rand index value is about 0.58 so we can assume that there is some agreement in similarity between those two partitions

# We can see that our starting accuracy is about 0.4821 using deafult number of quality value from referenced data + 3 iterations

# Kmeans with other parameters

wine_kmeans_2 = kmeans(red_wine_no_quality,6, iter.max = 100,nstart = 50,algorithm = c("MacQueen"))
result_2 = table(red_wine$quality,wine_kmeans_2$cluster )
result_2
accuracyCalc(result_2,1)
print(wine_kmeans_2$iter)
print(wine_kmeans_2)
rand.index(red_wine$quality, wine_kmeans_2$cluster)
#     1   2   3   4   5   6
# 3   6   0   2   2   0   0
# 4  28   1  10  10   4   0
# 5 181  71 143 169 117   0
# 6 230   7 132 215  54   0
# 7 102   0  24  59  12   2
# 8  11   0   3   2   2   0

# Accuracy: 0.4865541
# Iterations: 24
# Rand index: 0.5684099

# The same result, but with much more iterations - about 10-25, depending on case

wine_kmeans_3 = kmeans(red_wine_no_quality,4, iter.max = 100,nstart = 50,algorithm = c("Hartigan-Wong"))
result_3 = table(red_wine$quality,wine_kmeans_3$cluster )
result_3
accuracyCalc(result_3,1)
print(wine_kmeans_3$iter)
print(wine_kmeans_3)
rand.index(red_wine$quality, wine_kmeans_3$cluster)

#     1   2   3   4
# 3   3   7   0   0
# 4  14  31   6   2
# 5 195 247 151  88
# 6 242 300  86  10
# 7  54 125  18   2
# 8   3  12   3   0

# Accuracy: 0.4890557
# Iterations: 3
# Rand index: 0.5448806

wine_kmeans_4 = kmeans(red_wine_no_quality,6, iter.max = 100,nstart = 50,algorithm = c("Forgy"))
result_fin = table(red_wine$quality,wine_kmeans_4$cluster )
result_fin
accuracyCalc(result_fin,1)
print(wine_kmeans_4$iter)
print(wine_kmeans_4)
rand.index(red_wine$quality, wine_kmeans_4$cluster)

#     1   2   3   4   5   6
# 3   0   0   2   6   0   2
# 4   1   4  10  28   0  10
# 5  71 117 169 181   0 143
# 6   7  54 215 230   0 132
# 7   0  12  59 102   2  24
# 8   0   2   2  11   0   3

# Accuracy: 0.4865541
# Iterations: 42
# Rand index: 0.5684099

# Drawing clusters
plot(red_wine_no_quality[,c(7,11)], col = wine_kmeans$cluster)

# Adding centers for selected clusters
?points
points(wine_kmeans$centers[,c(7,11)], col = c(7,11), pch = c(7,10), cex=5)


# Increasing parameters keeps giving accuracy of 0.4884303 and doesn't seem to increase
# After reducing the number of clusters we acquired a little bit better accuracy, but this increase is not worth reduced number of clusters
# Also reducing number of clusters reduced the value of Rand index



# We try with distance functions and we want to find what result do they give

# Reference:
# http://girke.bioinformatics.ucr.edu/GEN242/pages/mydoc/Rclustering.html
# https://rpubs.com/mrkm_a/ClusteringMethodsWithR

####################################
# DISTANCE                         #
####################################


# EUCLIDEAN
?dist
wine_distance_euc = dist(red_wine_no_quality,method = "euclidean")

wkmeans_euc = kmeans(wine_distance_euc,6)
result_4 = table(red_wine$quality,wkmeans_euc$cluster )
result_4

accuracyCalc(result_4,1)
print(wkmeans_euc$iter)
rand.index(red_wine$quality, wkmeans_euc$cluster)

#     1   2   3   4   5   6
# 3   0   3   0   6   0   1
# 4   5   8   0  21   7  12
# 5 118 109  59 136 113 146
# 6  41 147   6 158 103 183
# 7  13  42   2  77  12  53
# 8   2   2   0   9   1   4

# Accuracy:0.4865541
# Iterations: 2
# Rand index: 0.5869062

# MANHATTAN

wine_distance_euc = dist(red_wine_no_quality,method = "manhattan")

wkmeans_man = kmeans(wine_distance_euc,6)
result_5 = table(red_wine$quality,wkmeans_man$cluster )
result_5

accuracyCalc(result_5,1)
print(wkmeans_man$iter)
rand.index(red_wine$quality, wkmeans_man$cluster)

#     1   2   3   4   5   6
# 3   0   3   6   0   1   0
# 4   6  10  24   3  10   0
# 5 108 122 158  97 147  49
# 6  95 142 178  29 186   8
# 7  10  38  86  11  52   2
# 8   1   2   9   2   4   0

# Accuracy:0.4752971
# Iterations: 3
# Rand index: 0.5810116

# MINKOWSKI
wine_distance_euc = dist(red_wine_no_quality,method = "minkowski")

wkmeans_min = kmeans(wine_distance_euc,6)
result_6 = table(red_wine$quality,wkmeans_min$cluster )
result_6

accuracyCalc(result_6,1)
print(wkmeans_min$iter)
rand.index(red_wine$quality, wkmeans_min$cluster)

#     1   2   3   4   5   6
# 3   0   0   6   1   3   0
# 4   7   5  21  12   8   0
# 5 113 118 136 146 109  59
# 6 103  41 158 183 147   6
# 7  12  13  77  53  42   2
# 8   1   2   9   4   2   0

# Accuracy:0.4865541
# Iterations: 4
# Rand index: 0.5869062


print(wkmeans_euc)
print(wkmeans_man)
print(wkmeans_min)

# Based on experiments with distances we can see, that apllying different distance methods gives us a little bit different results
# Manhattan distance proved to give the smallest value of accuracy and rand index so generally it is not the best solution for this dataset.



####################################
# DATA SCALING                     #
####################################

?scale
wineScale <- scale(red_wine_no_quality, center = FALSE)
str(wineScale)
wineScale_kmeans = kmeans(wineScale,6, iter.max = 20)
str(wineScale_kmeans)
score = table(red_wine$quality,wineScale_kmeans$cluster)
score
accuracyCalc(score,1)

# Drawing clusters
plot(wineScale[,c(3,6)], col = wineScale_kmeans$cluster)

# Adding centers for selected clusters
?points
points(wineScale_kmeans$centers[,c(3,6)], col = c(3,6), pch = c(7,10), cex=5)


#     1   2   3   4   5   6
# 3   1   0   0   0   2   7
# 4   6   1   6   1   4  35
# 5 237  29  93  16  58 248
# 6 143  11 153   6 123 202
# 7  26   6  63   1  68  35
# 8   3   0   6   0   8   1

# So, after scaling we acquired a little bit better accuracy - about 0.5 - but now it is difficult to find good way to show clusters on plot in comparision to previous experiments
# Difference in accuracy is only about 0.12, so the improvement of accuracy is arguable


####################################
# ELBOW METHOD                     #
####################################


# In this method we want to evaluate what is the optimal number of clusters for analyzed dataset
# Here we initiate vectors for storing values of kmeans iterations

wss <- vector(mode = "integer" ,length = 12)
acc <- vector(mode = "numeric" ,length = 12)
rand <- vector(mode = "numeric" ,length = 12)
ite <- vector(mode = "integer" ,length = 12)

#  1 to 12 clusters
for (i in 1:12) {
  kmeans.group <- kmeans(wineScale, centers = i, nstart=20, iter.max = 80)
  # total within-cluster sum of squares
  wss[i] <- kmeans.group$tot.withinss
  result_elbow <-table(red_wine$quality,kmeans.group$cluster)
  acc[i] <- accuracyCalc(result_elbow,1)
  rand[i] <- rand.index(red_wine$quality,kmeans.group$cluster)
  ite[i] <- kmeans.group$iter
}

kmeans.group
print(kmeans.group)

# total within-cluster sum of squares per number of groups
plot(1:12, wss, type = "b", 
     xlab = "number of groups", 
     ylab = "Total within-cluster sum of squares")

plot(1:12, acc, type = "b", 
     xlab = "number of groups", 
     ylab = "ACCURACY")

plot(1:12, rand, type = "b", 
     xlab = "number of groups", 
     ylab = "RAND_INDEX")

plot(1:12, ite, type = "b", 
     xlab = "number of groups", 
     ylab = "NUMBER OF ITERATIONS")

# Conclusions #
# - based on plot of total within-cluster sum of squares it is quite difficult to find the correct location of our elbow
# - on accuracy plot we can see that accuracy of about 0.52 is the highest and it is related to 9 groups. 
# Actually, on our previous plots we can observe that in fact there are some dense groups of points, but there are also a lot of points
# which are separated from each other with significant distances, but still are put in the same clusters. 
# Based on this plot i can say that those 3 additional clusters are made of loose points
# Rand index surprisingly is rushing for first 3 iterations to the value of 0.55 and then increases slithly to about 0.6. 
# I assume level of similarity between partitions  for our dataset is from the range 0.55 - 0.6 and we can't get anything better
# Iterations plot seems to find optimal location at 9 number of groups - somehow the iterations number here is a local mininmum
# and for next iterations it increases significantly in comparision to the rest of plot

####################################
# ECLUST                           #
####################################


##### 6 groups #####

wine_alt<-eclust(red_wine_no_quality, "kmeans", k=6, graph=FALSE)
fviz_silhouette(wine_alt, palette="jco")

# 1       1   69          0.40
# 2       2  324          0.31
# 3       3  400          0.54
# 4       4  179          0.40
# 5       5  242          0.28
# 6       6  385          0.36

silinfo<-wine_alt$silinfo
names(silinfo)

# Silhouette length for each cluster
silinfo$clus.avg.widths

# Avg. widths: 0.3980696 0.3122152 0.5437184 0.4018474 0.2828045 0.3629244

# Average silhouette length
silinfo$avg.width

# Average: 0.3916242


kmeans.tab = table(red_wine$quality, wine_alt$cluster)
accurate<- accuracyCalc(kmeans.tab,1)
accurate

# Accuracy: 0.4821764

##### 9 groups #####

wine_alt<-eclust(red_wine_no_quality, "kmeans", k=9, graph=FALSE)
fviz_silhouette(wine_alt, palette="jco")

# 1       1   68          0.38
# 2       2  156          0.33
# 3       3  272          0.30
# 4       4   76          0.26
# 5       5  147          0.33
# 6       6  228          0.29
# 7       7  157          0.38
# 8       8  248          0.29
# 9       9  247          0.51

silinfo<-wine_alt$silinfo
names(silinfo)

# Silhouette length for each cluster
silinfo$clus.avg.widths

# Avg. widths: 0.3783020 0.3284442 0.2963426 0.2609361 0.3295793 0.2850698 0.3819362 0.2888504 0.5079171

# Average silhouette length
silinfo$avg.width

# Average: 0.3426495


kmeans.tab = table(red_wine$quality, wine_alt$cluster)
accurate<- accuracyCalc(kmeans.tab,1)
accurate

# Accuracy: 0.4903064

##### 3 groups #####

wine_alt<-eclust(red_wine_no_quality, "kmeans", k=3, graph=FALSE)
fviz_silhouette(wine_alt, palette="jco")

# 1       1  241          0.41
# 2       2  544          0.40
# 3       3  814          0.63

silinfo<-wine_alt$silinfo
names(silinfo)

# Silhouette length for each cluster
silinfo$clus.avg.widths

# Avg. widths: 0.4139542 0.3962430 0.6337602

# Average silhouette length
silinfo$avg.width

# Average: 0.5198249


kmeans.tab = table(red_wine$quality, wine_alt$cluster)
accurate<- accuracyCalc(kmeans.tab,1)
accurate

# Accuracy: 0.4803002

# We can observe that adding more clusters doesn't increase average silhouette width so it's not the best idea according to this method.
# Only one cluster has avg width value above 0.5 so it is not the best idea to add more clusters
# Experiments with lower value of clusters prove to give better avg silhouette width, but the accuracy isn't changing a lot and is the best in case with 9 groups.
# We can also observe that the average witdh of one cluster seems to always be bigger than the others - he also has the biggest number of elements.

####################################
# PAM - partitioning around medoids#
####################################

# Deciding on the optimal number of clusters
fviz_nbclust(red_wine_no_quality, pam, method = "silhouette")+theme_classic()


# Based on result of function above, i decided to choose 2 clusters
# Division into 2 clusters
pam.score <- pam(red_wine_no_quality, 2)

# Clustering results together with information on objects being cluster centers
print(pam.score)

pam.score$silinfo

# Average width: 0.5784708

# Clusters avg. width: 0.6948670 0.3502049


# Adding information on cluster assignment
red_wine_clus<-cbind(red_wine_no_quality, pam.score$cluster)
head(red_wine_clus)

# Cluster centers
print(pam.score$medoids)

# Cluster assignment
pam.score$clustering


# Accuracy

pam.result = table(red_wine$quality, pam.score$cluster)
accuracyCalc(pam.result, 1)

# Accuracy value: 0.4859287


# Rand index

rand.index(red_wine$quality, pam.score$cluster)


# Rand index value: 0.487412


# Clustering visualization - it calculates quite long
# ?fviz_cluster
# fviz_cluster(pam.score,
#              palette = c("#00AFBB", "#FC4E07"), # color palette
#              ellipse.type = "t", # ellipse of concentration
#              repel = TRUE, # avoid overlapping (slows down)
#              ggtheme = theme_light() #background color
# )

# This method of clustering gives a little bit better result in terms of accuracy and rand index than ECLUST in majority of simulations.

####################################
# HIERARCHICAL CLUSTERING          #
####################################

?hclust
# Calculation of a distance matrix
?dist
distM = dist(red_wine_no_quality)
distT = as.matrix(distM)

dim(distT)

# Hierarchical clustering for different linkage methods
red_wine.hc_complete <- hclust(distM, method="complete")
red_wine.hc_single <- hclust(distM, method="single")
red_wine.hc <- hclust(distM, method="average")
red_wine.hc_centroid <- hclust(distM, method="centroid")

# Generating clusters
?cutree

##### AVG #####

red_wine.avg <- cutree(red_wine.hc, k=6)
red_wine.avg

plot(red_wine.hc, hang = -1, labels=red_wine$quality)
rect.hclust(red_wine.hc, k=6, border=1:4)

# Compare clusters with original class labels
res_avg<-table(red_wine$quality, red_wine.avg)

# Accuracy
accuracyCalc(res_avg, 1)

# Accuracy: 0.4796748

# Rand index
rand.index(red_wine$quality, red_wine.avg)

# Rand index: 0.4312645

# Silhouette width
sill_avg<-silhouette(red_wine.avg, distM)
summary(sill_avg)

# Avg sil: 0.5540

##### COMPLETE #####

red_wine.com <- cutree(red_wine.hc_complete, k=6)
red_wine.com

plot(red_wine.hc_complete, hang = -1, labels=red_wine$quality)
rect.hclust(red_wine.hc_complete, k=6, border=1:4)

# Compare clusters with original class labels
res_com<-table(red_wine$quality, red_wine.com)

# Accuracy
accuracyCalc(res_com, 1)

# Accuracy: 0.4696685

# Rand index
rand.index(red_wine$quality, red_wine.com)

# Rand index: 0.5277501

# Silhouette width
sill_avg<-silhouette(red_wine.com, distM)
summary(sill_avg)

# Avg sil: 0.4388


##### SINGLE #####

red_wine.sin <- cutree(red_wine.hc_single, k=6)
red_wine.sin

plot(red_wine.hc_single, hang = -1, labels=red_wine$quality)
rect.hclust(red_wine.hc_single, k=6, border=1:4)

# Compare clusters with original class labels
res_sin<-table(red_wine$quality, red_wine.sin)

# Accuracy
accuracyCalc(res_sin, 1)

# Accuracy: 0.4290181

# Rand index
rand.index(red_wine$quality, red_wine.sin)

# Rand index: 0.3610407

# Silhouette width
sill_avg<-silhouette(red_wine.sin, distM)
summary(sill_avg)

# Avg sil: 0.4044

##### CENTROID #####

red_wine.cen <- cutree(red_wine.hc_centroid, k=6)
red_wine.cen

plot(red_wine.hc_centroid, hang = -1, labels=red_wine$quality)
rect.hclust(red_wine.hc_centroid, k=6, border=1:4)

# Compare clusters with original class labels
res_cen<-table(red_wine$quality, red_wine.cen)

# Accuracy
accuracyCalc(res_cen, 1)

# Accuracy: 0.4828018

# Rand index
rand.index(red_wine$quality, red_wine.cen)

# Rand index: 0.4399018

# Silhouette width
sill_avg<-silhouette(red_wine.cen, distM)
summary(sill_avg)

# Avg sil: 0.5383

# SUMMARY

#             ACCURACY     RAND INDEX   SILHOUETTE

# AVG         0.4796748    0.4312645    0.5540

# COMPLETE    0.4696685    0.5277501    0.4388

# SINGLE      0.4290181    0.3610407    0.4044

# CENTROID    0.4828018    0.4399018    0.5383

# We can see that the best accuracy we obtained with Centroid method, but the rand index was the best in Complete method.
# On the other hand, silhouteet avg width was the best with Average method.
# We can assume, based on results, that Single method gives generally the worst result with this dataset

####################################
# DBSCAN                           #
####################################
?dbscan
?dbscan::kNNdistplot
?abline
dbscan::kNNdistplot(red_wine_no_quality, k=6)

abline(h=6, lty="solid")


# Dbscan alg. execution

red_wine_no_quality.dbscan <- dbscan(red_wine_no_quality, eps=3, MinPts=13)

# Compare clusters with original class labels
# Cluster 0 means noise
res_dbscan<-table(red_wine$quality, red_wine_no_quality.dbscan$cluster)
#     0   1   2   3   4   5   6
# 3   2   7   0   1   0   0   0
# 4  15  37   0   0   0   0   1
# 5 337 316   9   6   6   5   2
# 6 227 370   4  12   6   9  10
# 7  61 130   0   3   1   4   0
# 8   6  12   0   0   0   0   0
# Plot clusters
plot(red_wine_no_quality.dbscan, red_wine_no_quality[c(6,11)])

# Accuracy
accuracyCalc(res_dbscan,1)

# Accuracy: 0.4709193

rand.index(red_wine$quality, red_wine_no_quality.dbscan$cluster)
# Rand index: 0.5125771

# The best possible result i could get from using dbscan are 6 clusters, but it looks like too much elements has been attached to only two big clusters
# and other clusters got only few elements
# It's quite difficult to find good parameters for given dataset - there is a problem with clusters which are not dense adn their points are loose

####################################
# CONCLUSIONS                      #
####################################


# Main conclusions has been made under each experiment
# Generally, number of iterations doesn't really improve the result of experiment
# Single method of hierarchical clustering seems to be the worst for this dataset
# Scaling data  before experiments improves results slithly
# Higher number of clusters improves the accuracy, but rand index or silhouette avg witdh doesn't seem to increase really
# DBSCAN gave one of the worst accuracy value due to problems with setting good parameters - in given dataset there are something like 4 dense clusters and 2 loose
# The accuracy of certain methods depends highly on the possibility of method to distinguish dense clusters from loose ones and correctly append loose points to correct cluster
# PAM method proved to be one of the best in terms of accuracy for 6 clusters - only Eclust for 9 clusters achieved better value