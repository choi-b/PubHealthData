#LDA practice

#library(MASS)
#data(iris)
#head(iris,5)
#r = lda(formula= Species ~ .,
#        data = iris,
#        prior = c(1,1,1)/3)

###ASA's Health Data Challenge begins here###

#save the data
#save(expanded, file="Expanded.RData")
load("Expanded.RData")

#Using ExpandRows
#install.packages("splitstackshape")
library(splitstackshape)

#DATA IMPORT & PREPROCESSING

data1 = read.delim("death1999.txt")
sum(is.na(data1$Deaths))
sum(is.na(data2$Deaths))
data2 = read.delim("death2008.txt")
data2 = data2[!is.na(data2$Deaths),]

data = rbind(data1,data2)
summary(data)

expanded = expandRows(data,"Deaths")
expanded = expanded[-1]

summary(expanded)
#test = as.numeric(expanded$Single.Year.Ages.Code)-1

#histogram
#hist(test,main="Histogram of Deaths by Age", xlab="Age")

test2 = as.character(expanded$Single.Year.Ages.Code)
test2 = as.numeric(test2)

compare = data.frame(expanded$Single.Year.Ages.Code,test2)
View(compare)

summary(expanded$Single.Year.Ages.Code %in% test2)


#Work with a modified version of the dataset
dataset = expanded[c(-3,-5,-8)]
str(dataset)
unique(dataset$UCD...Drug.Alcohol.Induced.Cause)
unique(dataset$Gender.Code)

#count the frequency of each cause.
table(dataset$UCD...Drug.Alcohol.Induced.Cause)
#1. All other non-drug and non-alcohol causes
#2. Drug poisonings (overdose) Unintentional 
#3. All other alcohol-induced causes

#Handling missing values
table(dataset$Single.Year.Ages.Code) #3779 with "NS" - not specified age.
dataset = dataset[dataset$Single.Year.Ages.Code != "NS", ]
sum(dataset$Single.Year.Ages.Code == "NS") #3779 rows with "NS" age removed. Now 0.

#table(dataset$Single.Year.Ages.Code) #min=0, max=100
dataset$Single.Year.Ages.Code = as.numeric(dataset$Single.Year.Ages.Code)-1 #change age to numeric
hist(dataset$Single.Year.Ages.Code)
sum(dataset$Single.Year.Ages.Code == 0)


#Spinning 3D Scatterplot
install.packages("rgl")
library(rgl)
