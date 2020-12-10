# Sebastian Smoliński

# EDAMI Thursday 10:15
# 20Z

# Sequential rules task

# Our data table has 5 columns, where first one is ID and 4 remaining are:
# patient_id - indicates which patient's data are we looking at
# time_sek - this could be a timestamp or logical value like "breakfast" which was converted to 8:00 for example
# code - this value can be deciphered as one of the following things:

# 33 = Regular insulin dose
# 34 = NPH insulin dose
# 35 = UltraLente insulin dose

# Those above are types of insulin dose
# For more information one can check following sources:
#                   Onset  Peak   Duration
# Regular insulin   0,5-1h 2-4h   6-8h
# NPH               1–4h   6–10h  10-16h
# Ultralente        4-6h   14-24h 28-36h

# For more information check those links:
# https://dtc.ucsf.edu/types-of-diabetes/type2/treatment-of-type-2-diabetes/medications-and-therapies/type-2-insulin-rx/types-of-insulin/
# https://en.wikipedia.org/wiki/Ultralente_insulin
# https://en.wikipedia.org/wiki/NPH_insulin


# 48 = Unspecified blood glucose measurement
# 57 = Unspecified blood glucose measurement

# 58 = Pre-breakfast blood glucose measurement
# 59 = Post-breakfast blood glucose measurement

# 60 = Pre-lunch blood glucose measurement
# 61 = Post-lunch blood glucose measurement

# 62 = Pre-supper blood glucose measurement
# 63 = Post-supper blood glucose measurement

# 64 = Pre-snack blood glucose measurement
# 65 = Hypoglycemic symptoms
# 66 = Typical meal ingestion

# 67 = More-than-usual meal ingestion
# 68 = Less-than-usual meal ingestion

# 69 = Typical exercise activity
# 70 = More-than-usual exercise activity
# 71 = Less-than-usual exercise activity

# 72 = Unspecified special event
#
# Example record from dataset
# patient_id    time_sek    code      value 
# 1	            96772141	  id_58	    100


########################################################
# Icluding association and sequential libraries into project
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


############### CODES ###############

sort(unique(diabetes$code))
plot(table(diabetes$code),type = "h",xlab = "Type of code",ylab = "Frequency of codes")
diab<-subset(diabetes, !(code %in% c("id_36","id_56","id_72")))
#diab<-subset(diabetes, !(code %in% c("id_36","id_56")))
sort(unique(diabetes$code))

# It seems there are more code id's than we can find in description of our data set, so we decided to remove those two id's
# because we don't possess any info about them so for purpose of our task
# they are useless

############### VALUES ###############

# Now, we want to check the level of measurement for our patients
plot(table(diabetes$value),type = "h",xlab = "Value of measurement",ylab = "Frequency of values")

# Basing on scope of values i assume, that value is a measure of serum insulin pmol/L
# which is indicated by chart in this link https://en.wikipedia.org/wiki/Blood_sugar_level#/media/File:Suckale08_fig3_glucose_insulin_day.png
# Anyway, we can see that a lot of records contain value of measurement below 50 (if serum insulin) or 72 (if glucose blood level)
# Those values below 72 indicate that a lot of people had symptoms of hypoglycemia https://en.wikipedia.org/wiki/Hypoglycemia

quantile(diabetes$value)
#0%  25%  50%  75% 100% 
#0    6   20  139  501 

# As we can see, 50% values of all measurements are below 72 (for glucose blood level) or 50 (for serum insulin level)
# It shows that almost half of all measurements were made in a state of hyperglikemia

############### PATIENTS ###############

plot(table(diabetes$patient_id),type = "h",xlab = "Patient's id",ylab = "Number of records for given patient")
length(unique(diabetes$patient_id))

# It seems that there are no records for 4 patients (chart shows 70 places for lines, but 4 are empty)
# Length function confirms my suspicion - there are only records for 66 different patients

############### TIMESTAMPS ###############

plot(table(diabetes$time_sek),type = "h",xlab = "Value of seconds since the beginning of measurement",ylab = "Number of records for given value of seconds")


# We can see that there are 4 main peaks on the chart - this might be connected to "logical time" events mentioned in the description
# Those fixed times were assigned to breakfast (08:00), lunch (12:00), dinner (18:00), and bedtime (22:00)

############### DISCRETIZATION OF VALUES ###############

# Basing on https://en.wikipedia.org/wiki/Blood_sugar_level#Normal_values
# We set certain ranges
#  * - 69 - hypoglycemia - "hypo"
# 70 - 100 ((70+130)/2 = 100) - Normal level of sugar in blood during fastening and no meal - "normal"
# 100 - 130 - before meal and during bed - "bm_normal_b"
# 130 - 140 - after meal and during bed - "am_normal_b"
# 140 - 180 - after meal - "am_normal"
# 180 - *   - hyperglycemia - "hyper"

# As i mentioned earlier - i'm not sure if this is a glucose blood level or insulin blood levels - in the first case, the values received are too high, but on the other hand those extremely high numbers may be just wrong measurements

diabetes[["value"]] <- ordered(cut(diabetes[[ "value"]], c(-1,69,100,130,140,180,1000)), labels = c("hypo", "normal", "bm_normal_b","am_normal_b","am_normal","hyper"))
head(diabetes)

# Transfering data to transactional form so we would be able to read our sequences
write.table(diabetes, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )
diabetesSequence <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))
View(diabetesSequence)
frameSequence = as(diabetesSequence,"data.frame")
summary(frameSequence)
str(frameSequence)
View(frameSequence)

# We check if there are any NA's now

frameSequence<-is.na(frameSequence)

# Nothing has been printed so we assume that there are no NA's left

############### TIME AND ITEM FREQUENCY ###############

timeSeq  = as(diabetesSequence,"timedsequences")
freqT = timeFrequency(timeSeq, "times")
sort(freqT, decreasing = TRUE )

# Mingap between timestamps

mingap = timeFrequency(timeSeq, "mingap")
sort(mingap, decreasing = TRUE )

# Maxgap between timestamps

maxgap = timeFrequency(timeSeq, "maxgap")
sort(maxgap, decreasing = TRUE )

# Interesting things - some gaps suggest taking measurements after full hours like 118800 divided by 3600 are 33 hours

freqItem = itemFrequency(diabetesSequence)
str(freqItem)
freqItem = sort(freqItem, decreasing = TRUE )

head(freqItem,20)
print(freqItem)

# As we can see, based on our assumptions, we can make certain annotations:
# - the situation of hypoglycemia was the most frequent state
# - regular insulin was the most popular among patients - second was NPH and the least frequent was UltraLente insulin
# - hyperglycemia was also frequent among those patients - we can assume that appending every measure over 180 to hyperglycemia makes it quite popular
# - the most frequent type of measurement was the pre-breakfast one - it also affects other type of measurements where usually measurements were taken before meals
# - the conditions of a lot of measurement were unspecified

############### DISCOVERY OF SEQUENCES IN DATASET ###############


# Parameters have been chosen basing on reality - only one activity at the time so size max 2
seqParam = new ("SPparameter",support = 0.01, maxsize = 2, mingap=600, maxgap = 43200, maxlen = 4 )
# I think that gaps of 1 minute and 0.5 day are reasonable limits

# These parameters has been chosen because RStudio had problems with executing cspade, threw errors and executing this on desktop RStudio took almost 20 minutes...
patSeq = cspade(diabetesSequence,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

# We want to check if there is any connection between Hypoglycemic symptoms indicated by id 65 and value of glucose blood level
# and if the meal or exercise activity has any influence on occurance of hypoglycemia
seqRules = ruleInduction(patSeq,confidence = 0.6)
seqRules<- subset(seqRules, rhs(seqRules) %in% c('"id_65"','"hypo"'))


rulesI = subset(seqRules, !(lhs(seqRules) %in% c('"id_65"','"hypo"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')) & lhs(seqRules) %in% c('"id_66"','"id_67"','"id_68"','"id_69"','"id_70"','"id_71"','"id_33"','"id_34"','"id_35"') )

rulesI <- subset(rulesI, subset = lift > 1.05)
inspect(head(sort(rulesI, by="support"),20))
#str(rulesI)
summary(rulesI)

# lhs                  rhs                 support confidence     lift 
# 1 <{"id_33"}>       => <{"hypo",         0.9242424  1.0000000 1.081967 
#   "id_33"}>          
#   2 <{"id_33"},                               
# {"hyper"}>       => <{"hypo",         0.9090909  0.9836066 1.064230 
#   "id_33"}>          
#   3 <{"hyper"},                               
# {"id_33"}>       => <{"hypo",         0.9090909  0.9836066 1.064230 
#   "id_33"}>          
#   4 <{"id_33"},                               
# {"normal"}>      => <{"hypo",         0.9090909  0.9836066 1.064230 
#   "id_33"}>          
#   5 <{"id_33"},                               
# {"id_33"},                               
# {"hyper"}>       => <{"hypo",         0.8939394  0.9833333 1.063934 
#   "id_33"}>          

# Based on our calculations we can assume that:
# - almost always taking regular  insulin dose was connected with measurement value indicating hypoglycemia
# - the support is uncommonly high, almost 90% - it seems that taking insulin is a must-be activity when in hypoglycemia state
# - at the same time it's quite strange that the lift is only about 1.06 which says that there is no real relation between antecedant and consequent
# - the level of lift among those rules with highest support is almost the same - there is no real relation between lhs and rhs rules
# - taking NPH insulin dose has significantly lower support than taking regular insulin dose
# - further rules indicate that there is strong connection between NPH insulin dose and regular insulin dose
# it may be indicating a rule that patients must take different types of insulin/mix them with different frequency
# General observation: patients must take different types of insulin everyday and keep taking certain types of insulin on exact time





# Check of meal ingestion impact on hypoglycemia

seqRules = ruleInduction(patSeq,confidence = 0.6)
seqRules<- subset(seqRules, rhs(seqRules) %in% c('"id_65"','"hypo"') & !(rhs(seqRules) %in% c('"id_33"','"id_34"','"id_35"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')))

rulesI = subset(seqRules, !(lhs(seqRules) %in% c('"id_65"','"hypo"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')) & lhs(seqRules) %in% c('"id_66"','"id_67"','"id_68"') )

rulesI <- subset(rulesI, subset = lift > 1.05)
inspect(head(sort(rulesI, by="support"),20))

# lhs                  rhs                  support confidence     lift 
# 1 <{"id_67"},                               
# {"id_33"}>       => <{"id_67",        0.27272727  0.6000000 1.164706 
#                         "hypo"}>           
#   2 <{"id_34"},                               
# {"normal"},                              
# {"id_67"}>       => <{"id_67",        0.10606061  0.6363636 1.235294 
#                       "hypo"}>           
#   3 <{"am_normal"},                           
# {"id_67"},                               
# {"id_34"}>       => <{"id_65"}>       0.09090909  0.6666667 1.189189 
# 4 <{"am_normal"},                           
# {"id_67"},                               
# {"id_34"}>       => <{"id_65",        0.09090909  0.6666667 1.189189 
#                       "hypo"}>           
#   5 <{"id_67"},                               
# {"id_71"},                               
# {"id_33"}>       => <{"id_67",        0.07575758  0.7142857 1.386555 
#                         "hypo"}>    


# - Quite logical but highlighted in people with diabetes.
# - This rule isn't too strong in confidence and lift compared to the others but it has a practical value.
# - Patients shouldn't eat irregularly or have some strange eating habits
# - Eating less or nothing brings in a few hours hypoglycemia.
# - The support is quite low, confidence and lift are reasonable so we can assume that 
# - The results are comparable to experiments above
# Here we check if there is any connection between hypoglycemia occurance and taking certain insulin type doses

seqRules = ruleInduction(patSeq,confidence = 0.6)
seqRules<- subset(seqRules, rhs(seqRules) %in% c('"id_65"','"hypo"') & !(rhs(seqRules) %in% c('"id_33"','"id_34"','"id_35"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')))

rulesI = subset(seqRules, !(lhs(seqRules) %in% c('"id_65"','"hypo"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')) & lhs(seqRules) %in% c('"id_33"','"id_34"','"id_35"') )

rulesI <- subset(rulesI, subset = lift > 1.05)
inspect(head(sort(rulesI, by="support"),20))
summary(rulesI)

#   lhs                  rhs                  support confidence     lift 
# 1 <{"id_67"},                               
# {"id_33"}>       => <{"id_67",        0.27272727  0.6000000 1.164706 
#   "hypo"}>           
#   2 <{"id_67"},                               
# {"id_33"},                               
# {"hyper"}>       => <{"id_67",        0.22727273  0.5769231 1.119910 
#   "hypo"}>           
#   3 <{"id_34"},                               
# {"id_34"},                               
# {"id_34"}>       => <{"id_65"}>       0.19696970  0.6842105 1.220484 
# 4 <{"id_34"},                               
# {"id_34"},                               
# {"id_34"}>       => <{"id_65",        0.19696970  0.6842105 1.220484 
#   "hypo"}>           
#   5 <{"id_35"},                               
# {"id_33"}>       => <{"id_67",        0.18181818  0.6315789 1.226006 
#   "hypo"}>           

# - As we can see, there is a connection between more than usual meal ingestion and taking different types of insulin
# - This can indicate that patients have bad diet - they take "stronger" types of insulin - NPH and UltraLente and then they are eating too much to fight back hypoglycemia
# - In this case the support of rules is quite low - we can treat the overeating to fight back hypoglycemia as a patologic behavior
# - The confidence of rules varies from 0.55 to 0.85 - so the overeating is a rule and a signal for doctor to reduce insulin doese, prepare better diet for patient
# - We can see also that exercise activity and unspecified special events can lead to hypoglycemia - the support of those events is really low, but they exist
# - Maybe it would be a good idea to change insulin type or reduce their doses - almost always taking an insulin leads to hypoglycemia and i think patients take it too often

# We want to check the impact of physical activity on occurance of hypoglycemia 

seqRules = ruleInduction(patSeq,confidence = 0.6)
seqRules<- subset(seqRules, rhs(seqRules) %in% c('"id_65"','"hypo"') & !(rhs(seqRules) %in% c('"id_33"','"id_34"','"id_35"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')))

rulesI = subset(seqRules, !(lhs(seqRules) %in% c('"id_65"','"hypo"','"id_48"','"id_57"','"id_58"','"id_59"','"id_60"','"id_61"','"id_62"','"id_63"','"id_64"')) & lhs(seqRules) %in% c('"id_69"','"id_70"','"id_71"') )

rulesI <- subset(rulesI, subset = lift > 1.05)
inspect(head(sort(rulesI, by="support"),20))

# lhs                  rhs                  support confidence     lift 
# 1 <{"id_33"},                               
# {"id_70"},                               
# {"normal"}>      => <{"id_70",        0.09090909  0.8571429 2.357143 
#   "hypo"}>           
#   2 <{"id_70"},                               
# {"normal"},                              
# {"normal"}>      => <{"id_70",        0.09090909  0.6000000 1.650000 
#   "hypo"}>           
#   3 <{"id_71"},                               
# {"id_34"},                               
# {"normal"}>      => <{"id_71",        0.07575758  0.6250000 1.650000 
#   "hypo"}>           
#   4 <{"normal"},                              
# {"id_70"},                               
# {"id_33"}>       => <{"id_70",        0.07575758  0.6250000 1.718750 
#   "hypo"}>           
#   5 <{"id_67"},                               
# {"id_71"},                               
# {"id_33"}>       => <{"id_67",        0.07575758  0.7142857 1.386555 
#   "hypo"}>           

# - We can see that support of physical activity leading to hypoglycemia is rather low, but they have really high confidence so it's quite possible to happen
# - We cna also observe that more than usual exercise activity has the highest support, has quite high confidence of 85% and lift value over 2,35!
# - I think that quite high values of lift - in comparision to previous experiments - prove that physical activity at all leads to hypoglycemia and drop of glucose blood level
# - Doctors should warn their patients about it and remind them to always carry some sugars when they plan to exercise or play a game like football
# - Especially it is unadvisable to go exercise when one is fastening or didn't eat recently - this also have impact on occurance of hypoglycemia

############### GENERAL CONCLUSIONS ###############

# Comments are integral part of conclusions
#
# Whole report has been made on the purpose of showing some helpful information for medical staff
# Few things were able to be predicted even without medical education - that meals, sports, frequency of meals, sport activity are
# connected with low levels of glucose in blood and for diabetes it is really important to carry some 
# simple sugars with them when they plan to workout
# Also, it's quite important for patients to keep meals in set periods of time
#
# As we could see, the two most popular insulin doses had support and confidence higher than other rules.
# The lift differs from 1.1 to 1.9 so we can make a general conclusion that usually the presence of antecedant of a rule in this transaction increases propability of occurancee of the consequent of rule in given transaction 
# And this applies to whole set
# 
# We can assume that most of hypoglycemia cases happen before meals and highly depend on type of meal - if this consisted of simple sugars which lead to high levels and drops of glucose level in blood
# or the melas were starch-rich
#
# Physical exercises can be beneficial to patients, but they should be preapared for it and have in mind that 
# their glucose level in blood can drop significantly and this applies in particular to diabetes.
# Nutritious meals can be good for patients, but they can't forget about other meals - little or no meals can lead to hypoglycemia
# General consideration is that patients should care about frequent meals, those meals should be nutrious and they should consist aas much complex sugars as it's possible 
