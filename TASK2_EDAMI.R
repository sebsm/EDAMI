# Sebastian Smoliński

# EDAMI Thursday 10:15
# 20Z

# Sequential rules task
library(arules)
library(arulesSequences)

# Downloading file
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')

# Reading data and saving it into dataframe
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
diabetes<-diab.df
View(diabetes)
summary(diabetes)

# We check if there are any NA cells
diabetes<-na.omit(diabetes)
View(diabetes)
summary(diabetes)

# We detected 8 lines with NA value so we decided to remove them

# Removing header
write.table(diabetes, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )
?read_baskets

# Saving file in transactional form
diabetes <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))
diabetesSequence <- diabetes

# Displaying result
View(as(diabetesSequence,"data.frame"))
summary(diabetesSequence)

################################################################################################

#information about data concerning times 
?timeFrequency
timeSeq  = as(diabetesSequence,"timedsequences")
freqT = timeFrequency(timeSeq, "times")
freqT
?timeFrequency
spanT= timeFrequency(timeSeq, "span")
spanT

freqItem = itemFrequency(diabetesSequence)
#str(freqItem)
freqItem = sort(freqItem, decreasing = TRUE )

head(freqItem,20)
str(diabetesSequence)


#transaction for in a given sequence
trans109 <- diabetesSequence[diabetesSequence@itemsetInfo$sequenceID == 109]
inspect(trans109)

#frequent sequences discovery
#parameters of Spade algorithm
?cspade
seqParam = new ("SPparameter",support = 0.003, maxsize = 5, mingap=1, maxgap = 3, maxlen = 5)
print(seqParam)

#execution of the algorithm

patSeq= cspade(diabetesSequence,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

#information about discoverd sequences
summary(patSeq)
#length(patSeq)

inspect(head(patSeq,100))
#patterns with more than one element
seq2elem <- patSeq[size(patSeq)>1]
length(seq2elem)
inspect(seq2elem)


#patters supported by a given sequence of transactions(without time constraints))
inspect(patSeq[which(support(patSeq,trans109,type='absolute') >0)])

#searching for patterns with given items
?subset
?match

seqI = subset(patSeq, x %ein% c('design','art'))
inspect(seqI)
View(as(seqI,"data.frame"))

#execution the algorithm with different parameters
seqParam1 = new ("SPparameter",support = 0.001, maxsize = 5, mingap=1, maxgap = 3, maxlen = 5)
patSeq1= cspade(diabetesSequence,seqParam1, control = list(verbose = TRUE, tidLists = TRUE, summary= TRUE))

#selecting new discovered sequences

seqdiff = patSeq1[which(!(patSeq1 %in% patSeq))]
length(patSeq1)
length(seqdiff)

#discovery sequential rules
?ruleInduction
seqRules = ruleInduction(patSeq1,confidence = 0.8)

length(seqRules)
#summary of set of rules
summary(seqRules)
#view of rules and sequneces
inspect(seqRules)

?arulesSequences::size

size(lhs(seqRules))
size(lhs(seqRules), 'length')
size(lhs(seqRules), 'itemsets')
size(lhs(seqRules), 'items')

#sequences in the left part of rules
inspect(lhs(seqRules))
#sequences in the right part of rules
inspect(rhs(seqRules))

#items in rules
seqRules@elements@items@itemInfo

#all patterns included in rules (from left and right parts of rules)
allSeq <- c(rhs(seqRules),lhs(seqRules))
allSeq <- unique(allSeq)
inspect(allSeq)
str(allSeq)

#selecting interesting rules
rulesI = subset(seqRules, rhs(seqRules) %in% c('design','webdesign'))
inspect(rulesI)
View(as(rulesI,"data.frame"))

rulesI = subset(seqRules, lhs(seqRules) %ein% c('design','webdesign'))
inspect(rulesI)

rulesI = subset(seqRules, lhs(seqRules) %ein% c('design','webdesign') & rhs(seqRules) %in% c('webdesign'))
inspect(rulesI)
