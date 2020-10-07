library(car)
library(DescTools)
library (MASS)

setwd("~/Desktop/STAT_6443")

tooth <- read.csv("ToothGrowthCSV.csv")
tooth$Dose= as.factor(tooth$Dose_in_milligrams)

summary(aov(Tooth_length~Supplement_type+Dose, data=tooth))
summary(aov(Tooth_length~Dose+Supplement_type, data=tooth))

ozkid <- read.csv("ozkidsCSV.csv", header=TRUE)
str(ozkid)
table(ozkid$origin)
table(ozkid$sex)
###Data is unbalanced

summary(aov(days~origin+grade, data=ozkid))
summary(aov(days~grade+origin, data=ozkid))

anova(lm(days~origin+grade, data=ozkid))
anova(lm(days~grade+origin, data=ozkid))

#boxplot of days by origin
#dev.new(width=100, height=100, unit ="px")
#par(mfrow=c(2,2))
boxplot(days~ origin, data=ozkid, main="Days by Origin", 
        xlab="Origin", ylab="Days")

boxplot(days~ sex, data=ozkid, main="Days by Sex", 
        xlab="Sex", ylab="Days")

boxplot(days~ grade, data=ozkid, main="Days by grade", 
        xlab="Grade", ylab="Days")

boxplot(days~ type, data=ozkid, main="Days by Type", 
        xlab="Type", ylab="Days")
#Type 1 
aov.ozkid1= aov(days~grade+origin, data=ozkid)
aov.ozkid2= aov(days~origin+grade, data=ozkid)
#Type 3
Anova(aov.ozkid1, type=3)
Anova(aov.ozkid2, type=3)
#We reject the null for the grade, and for the origin. There exists a significant effect from grade and from origin. 

TukeyHSD(aov.ozkid1)

#The only group with different mean values is F2 and F1; F2 has a higher mean since difference is positive. 
#A is greater than N 

#LeveneTest(aov.ozkid1)--Test failed: check for equal variance. 

par(mfrow=c(2,2))
plot(aov.ozkid1)
