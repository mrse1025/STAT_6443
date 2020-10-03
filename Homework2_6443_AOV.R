#Victoria Espinola
#STAT 6443 Homework #2
# 2 October 2020

library(DescTools); library(MASS)

heartbpchol <- read.csv("~/Desktop/STAT_6443/heartbpchol.csv")
bupa <- read.csv("~/Desktop/STAT_6443/bupa.csv")
psych <- read.csv("~/Desktop/STAT_6443/psych.csv")
cars_new<- read.csv("~/Desktop/STAT_6443/cars_new.csv")

#Question 1
aov.hrt=aov(Cholesterol~BP_Status, data=heartbpchol)
summary(aov.hrt)
LeveneTest(aov.hrt)
ScheffeTest(aov.hrt)

#a) The null hypothesis is that BP status does not effect the Cholesterol. 
#Since the p-value is smaller than .05, we reject the null hypothesis, and conclude there Cholesterol effects BP Status.
#Applying the Levene's Test we can assume that the variance assumption is valid since the p-value is greater than .05.
#b) Applying Scheffe's Test we see that when a HIGH BP Status has the larger mean of Cholesterol than a NORMAL BP Status, which is larger than the mean of an OPTIMAL BP Status. 


#Question 2 
bupa$drinkgroup <- as.factor(bupa$drinkgroup)
aov.bupa=aov(mcv~drinkgroup, data=bupa)
summary(aov.bupa)
LeveneTest(aov.bupa)

#The null hypothesis is no interaction between the mcv amount and drink group. 
#a) Since the p-value is less than .05, we reject the null hypothesis and conclude that there is an interaction between the mcv and drink group. 
#After applying the Levene's Test, the p-value is greater than .05, therefore the equal variance assumption can be trusted. 

aov.bupa2=aov(alkphos~drinkgroup, data=bupa)
summary(aov.bupa2)
LeveneTest(aov.bupa2)

#The null hypothesis is there is no interaction between alkphos and drink group. 
#b) Since the p-value is less than .05, we reject the null hypothesis and conclude there is an interaction between alkphos and drink group.
#After Levene's Test, the p-value is greater than .05, therefore the equal variance assumption can be trusted. 

ScheffeTest(aov.bupa)
ScheffeTest(aov.bupa2)

#c) After both post-hoc test, both models show that group 5 has the largest mean of mcv and alkphos. 

#Question 3


#Questions 4



