# Sebastian Smoliński

# EDAMI Thursday 10:15
# 20Z

# Association rules task
# Aim of the experiments in which answers to the following questions should be included:
# • what a rule is interesting (definition based on available rule parameters);
# • how to choose the best rule(s);
# • what is potential practical application of the discovered rules.

# 1) Downloading the dataset
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/supermarket.csv','supermarket.csv')
# We save the dataset under 'marketSet' name
original = read.csv('supermarket.csv',sep=';')
summary(original)
#View(original)

# So basically we can observe than input data is in a form of 0 or 1 in a cell of dataframe
marketSet = read.csv('supermarket.csv',sep=';')
??marketSet

# Some basic information about given dataset
colnames(marketSet)
summary(marketSet)
head(marketSet,5)
dim(marketSet)
View(marketSet)

# We convert our dataset to dataframe 
marketSet= as.data.frame(sapply(marketSet, function(x) as.logical(x)))
# We check if anything has changed

colnames(marketSet)
summary(marketSet)
head(marketSet,5)
dim(marketSet)
View(marketSet)
str(marketSet)

# Loading libraries for association rules and visualisation

library(arulesViz)
library(arules) 

# We are checking if there are any NA's in our dataset
is.na(marketSet)

delOneValued <- function(inputData11)
{
  res <- c(which(sapply(inputData11, function(x) {length(unique(x))}) == 1));
  if(length(res) > 0)         
  {
    data11 <- inputData11[,-res];
  }   
  else
  {
    data11 <-inputData11;
  }
  data11 
}

str(marketSet)

# There are no columns with only one value like one column with 1s in all cells

# We are converting this dataset to new "transactions" format
marketSet2 <- as(marketSet, "transactions")
str(marketSet)
View(marketSet)
str(marketSet2)

summary(marketSet2)
length(which(is.na(marketSet)==TRUE))

# Take a look at the results

# 4627 rows (elements/itemsets/transactions) and
# 122 columns (items) and a density of 0.1519272 
# 
# most frequent items:
#   bread.and.cake          fruit     vegetables     milk.cream   baking.needs 
# 3330           2962           2961           2939           2795 
# (Other) 
# 70775 
# 
# element (itemset/transaction) length distribution:
#   sizes
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21 
# 36  19  22  22  26  46  58  83  97 148 181 198 228 274 271 299 249 260 238 205 172 
# 22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42 
# 194 177 162 143 124 118 106  95  68  65  57  39  30  28  21  17  10  18   6   3   4 
# 43  44  45  48 
# 3   4   2   1 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   13.00   18.00   18.54   23.00   48.00 


# As we can see, the most frequent items are:

# 1) bread and cake
# 2) fruit
# 3) vegetables
# 4) milk cream
# 5) baking needs

# As we can see, the length 16 of element is the most frequent among those

# Let's take a look on frequency of items there

marketTable1  = itemFrequency(marketSet2, type = "relative")
View(marketTable1)
marketTable2  = itemFrequency(marketSet2, type = "absolute")
View(marketTable2)
str(marketTable1)
class(marketTable1)
summary(marketTable1)
View(marketTable1)
print(marketTable1)


marketTable1  = sort(marketTable1, decreasing= TRUE)
View(marketTable1)

# Let's check if there are any elements with support higher than 50%

print(marketTable1[marketTable1 > 0.5])

# #   bread.and.cake             fruit        vegetables        milk.cream 
# 0.7196888         0.6401556         0.6399395         0.6351848 
# baking.needs      frozen.foods          biscuits juice.sat.cord.ms 
# 0.6040631         0.5872055         0.5629998         0.5323104 
# party.snack.foods 
# 0.5035660

# Out of 122 columns only 9 have support higher than 50%
length(marketTable1[marketTable1>=0.5])
length(marketTable1[marketTable1>=0.05])
length(marketTable1[marketTable1>=0.1])
length(marketTable1[marketTable1>=0.2])
itemFrequencyPlot(marketSet2, type ="relative", support= 0.1)

# As we can observe, only 69 out of 122 have support higher than 5%, so we can conclude that columns with value '1' are not frequent in a scope of all attributes



##### Frequent itemsets ######

# Firstly, we have to set parameters for our Apriori algorithm

aParam  = new("APparameter", "confidence" = 0.5, "support" = 0.3, "minlen"= 1)
print(aParam)
View(marketSet2)

aParam@target ="frequent itemsets"

#aprioriMarket = sort(apriori(marketSet2, aParam), by = 'support')

aprioriMarket = apriori(marketSet2, aParam)
str(aprioriMarket)
length(aprioriMarket)
summary(aprioriMarket)
inspect(aprioriMarket)

# As we can see, there are about 105 when minlen = 1 and 82 when minlen = 2 rules after apllying apriori algorithm


# Most frequent are the same as most frequent items shown previosusly, but the order is different

inspect(head(sort(aprioriMarket, by="support"),10))

#Top 10 most popular rules
#      items                         support   transIdenticalToItemsets count
# [1]  {bread.and.cake,milk.cream}   0.5050789 0.0006483683             2337 
# [2]  {bread.and.cake,fruit}        0.5024854 0.0000000000             2325 
# [3]  {bread.and.cake,vegetables}   0.4966501 0.0000000000             2298 
# [4]  {fruit,vegetables}            0.4769829 0.0002161228             2207 
# [5]  {bread.and.cake,baking.needs} 0.4735250 0.0000000000             2191 
# [6]  {bread.and.cake,frozen.foods} 0.4601254 0.0000000000             2129 
# [7]  {bread.and.cake,biscuits}     0.4501837 0.0000000000             2083 
# [8]  {milk.cream,fruit}            0.4404582 0.0000000000             2038 
# [9]  {milk.cream,vegetables}       0.4376486 0.0000000000             2025 
# [10] {baking.needs,vegetables}     0.4212233 0.0000000000             1949

inspect(aprioriMarket[size(aprioriMarket)==3])
inspect(aprioriMarket[size(aprioriMarket)==2])
inspect(aprioriMarket[size(aprioriMarket)==1])
# As we can see, the highest support in group of rules of length 2 combination of bread and cake and milk cream has the highest support of 50% when in group of lentgh equal to 3 the highest support is 38%

# Some charts

plot(aprioriMarket[size(aprioriMarket)==3], method = "graph")
plot(aprioriMarket[size(aprioriMarket)==2], method = "graph")
plot(aprioriMarket[size(aprioriMarket)==1], method = "graph")

# As we can see on plots, especially this with rules length equal to 2, soft drinks, department137, cheese, pet foods and breakfast food happen only in combination with bread and cake

setsmilk <- subset(aprioriMarket, subset = items %in% "milk.cream")
inspect(setsmilk)
setsmilk <- subset(aprioriMarket, subset = items %pin% "bread.and.cake")
inspect(setsmilk)
setsmilk <- subset(aprioriMarket, subset = items %in% "fruit")
inspect(setsmilk)

summary(aprioriMarket)
is.closed(aprioriMarket)

# Discovering frequent itemsets

# Try with Eclat algorithm

ecParam  = new("ECparameter", "confidence" = 0.5, "support" = 0.3) 
print(ecParam)

fsets <- eclat(marketSet2,ecParam)
length(fsets)


inspect(fsets[which(is.na(fsets %in% aprioriMarket))])

inspect(fsets[which((fsets %in% aprioriMarket)==FALSE)])
length(fsets[which((fsets %in% aprioriMarket)==FALSE)])

# We can see that with the same parameters Eclat didn't find any other rules which haven't been found by apriori


# We want to find rules with highest lift, support and confidence within our rules
assoRule = new("APparameter", "confidence"=0.5, "support"=0.1, "minlen"=2, target = "rules")
print(assoRule)
assoRules <- apriori(marketSet2,assoRule)
redundants<-is.redundant(assoRules)
assoRules<-assoRules[!redundants]
summary(assoRules)
length(assoRules)
str(assoRules)
max(size(assoRules))
min(size(assoRules))

# This set of commands give us better overview - we can see that:
# - absolute minimum support in this case is 462 which is  about 1/7 of the highest support
# - there are almost 25 000 rules discovered
# - the rule length of 4 is the highest
# - the mean is 4.23 so we can conclude that ruless seem to be longer than shorter chains of elements -> that means that more than less elements are connected with each other
# - the minimum lift is almost one and the highest is 1.72 so we can assume that our rules are essentially interdependent|

# rule length distribution (lhs + rhs):sizes
# 2     3     4     5     6     7 
# 494  4365 10224  7599  1613    68 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   4.000   4.000   4.233   5.000   7.000 
# 
# summary of quality measures:
#   support         confidence        coverage           lift            count       
# Min.   :0.1001   Min.   :0.5000   Min.   :0.1091   Min.   :0.9402   Min.   : 463.0  
# 1st Qu.:0.1076   1st Qu.:0.6555   1st Qu.:0.1463   1st Qu.:1.1969   1st Qu.: 498.0  
# Median :0.1189   Median :0.7307   Median :0.1701   Median :1.2675   Median : 550.0  
# Mean   :0.1310   Mean   :0.7182   Mean   :0.1864   Mean   :1.2723   Mean   : 606.1  
# 3rd Qu.:0.1400   3rd Qu.:0.7848   3rd Qu.:0.2055   3rd Qu.:1.3390   3rd Qu.: 648.0  
# Max.   :0.5051   Max.   :0.9205   Max.   :0.7197   Max.   :1.7263   Max.   :2337.0  



assoRule = new("APparameter", "confidence"=0.5, "support"=0.05, "minlen"=2, target = "rules")
print(assoRule)
assoRules <- apriori(marketSet2,assoRule)
redundants<-is.redundant(assoRules)
assoRules<-assoRules[!redundants]
summary(assoRules)
length(assoRules)
str(assoRules)
max(size(assoRules))
min(size(assoRules))

# Dividing support by 2 resulted in almost double the number of disvocered rules so we can assume that there are a lot of rules with very little support

assoRule = new("APparameter", "confidence"=0.5, "support"=0.2, "minlen"=2, target = "rules")
print(assoRule)
assoRules <- apriori(marketSet2,assoRule)
redundants<-is.redundant(assoRules)
assoRules<-assoRules[!redundants]
summary(assoRules)
length(assoRules)
str(assoRules)
max(size(assoRules))
min(size(assoRules))

# With support equal to 0.2, the number of discovered rules is almost 1/12 of number for 0.1.

assoRule = new("APparameter", "confidence"=0.5, "support"=0.3, "minlen"=2, target = "rules")
print(assoRule)
assoRules <- apriori(marketSet2,assoRule)
redundants<-is.redundant(assoRules)
assoRules<-assoRules[!redundants]
summary(assoRules)
length(assoRules)
str(assoRules)
max(size(assoRules))
min(size(assoRules))

# There are only 179 rules when support is 0.3

# General conclusion: rules in our set seem to have much lower support and at the point of 0.3 support our longest rule contains only 3-element chain. It is quite big difference in comparision to max length of 9 in 0.05 support


# discovering rules with a given consequent
# 1) bread and cake
# 2) fruit
# 3) vegetables
# 4) milk cream
# 5) baking needs

# We want to check if there are any rules where consequent is any/our most frequent item
rulesWithBread <- apriori(marketSet2, parameter = list(support=0.1, confidence = 0.5, minlen = 2), 
                        appearance = list(rhs = c("bread.and.cake"), default="lhs"))
inspect(rulesWithBread[1:10])

# 2965 rules
rulesWithFruit <- apriori(marketSet2, parameter = list(support=0.1, confidence = 0.5, minlen = 2), 
                          appearance = list(rhs = c("fruit"), default="lhs"))
inspect(rulesWithFruit[1:10])

# 2258 rules
rulesWithVegetables <- apriori(marketSet2, parameter = list(support=0.1, confidence = 0.5, minlen = 2), 
                          appearance = list(rhs = c("vegetables"), default="lhs"))
inspect(rulesWithVegetables[1:10])

# 2318 rules
rulesWithMilkCream <- apriori(marketSet2, parameter = list(support=0.1, confidence = 0.5, minlen = 2), 
                          appearance = list(rhs = c("milk.cream"), default="lhs"))
inspect(rulesWithMilkCream[1:10])

# 1873 rules
rulesWithBakingNeeds <- apriori(marketSet2, parameter = list(support=0.1, confidence = 0.5, minlen = 2), 
                          appearance = list(rhs = c("baking.needs"), default="lhs"))
inspect(rulesWithBakingNeeds[1:10])

# 2266 rules

rulesWithAll <- apriori(marketSet2, parameter = list(support=0.1, confidence = 0.5, minlen = 2), 
                                appearance = list(rhs = c("bread.and.cake","fruit","vegetables","milk.cream","baking.needs"), default="lhs"))
inspect(rulesWithAll[1:10])

# We can see that rules where our 5 most frequent items are consequents make up 11 680 rules out of 25 000 rules so we can say that 
# almost in half of all rules people are buying one of those elements as some next item

# Which elements are independent of each other?
inspect(head(sort(assoRules, by="lift",decreasing = F),10))

# It seems that cigs and tobacco and bread and cake rule has the lowest lift so we can assume that they are independent form each other

inspect(head(sort(assoRules, by="support",decreasing = F),10))
inspect(head(sort(assoRules, by="support",decreasing = T),10))

# We can see that hair care and milk cream almost always ends with buying bread and cake
# Also tea, biscuits lead to fruit, so  we can assume that people buying tea and biscuits for party or family meeting really often also buy fruits (maybe for quests?)

inspect(head(sort(assoRules, by="confidence",decreasing = F),10))

ruleLift <- subset(assoRules, subset = lift > 1.5)
inspect(head(sort(ruleLift, by="lift")))
# There are only 6 rules with lift over 1.5

ruleLift1 <- subset(assoRules, subset = lift < 1)
inspect(head(sort(ruleLift1, by="lift")))

# And there is only with lift below 1

# Comparision of length and average confidence

summary(sort((assoRules[size(assoRules)==7]), by="confidence"))
# Mean confidence 0.82
summary(sort((assoRules[size(assoRules)==6]), by="confidence"))
# Mean confidence 0.78
summary(sort((assoRules[size(assoRules)==5]), by="confidence"))
# Mean confidence 0.74
summary(sort((assoRules[size(assoRules)==4]), by="confidence"))
# Mean confidence 0.70
summary(sort((assoRules[size(assoRules)==3]), by="confidence"))
# Mean confidence 0.67
summary(sort((assoRules[size(assoRules)==2]), by="confidence"))
# Mean confidence 0.64

# We can see that the confidence is lower the lower is the length of the whole rule
# This means that confidence of buying frequent itemsets is higher

# General conclusions

# As we saw, we can assume that rules with lower confidence and support seem to have higher lift - it is quite reasonable, because we are dividing by lower number.

# Longer itemsets happen to have greater confidence - it's also quite obvious, because the general pattern are easier to see (for example - if we buy tea we buy everything which could be connected to party or quests who plan to visit us. These situations are not common, but when they happen they seem to have more elements and are easy to catch. Those sets with lower number of elements seem to be more random.



