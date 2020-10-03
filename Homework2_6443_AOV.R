#Victoria Espinola
#STAT 6443 Homework #2
# 2 October 2020

library(DescTools); library(MASS); library(car)

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
#Since the p-value is smaller than .05, we reject the null hypothesis, and conclude there is interaction between Cholesterol and BP Status.The amount of variation described by the model is largely from the Residual.
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

#The null hypothesis is there is no interaction between alkphos and drink group. The amount of variation in the model is largely from the residual.   
#b) Since the p-value is less than .05, we reject the null hypothesis and conclude there is an interaction between alkphos and drink group.
#After Levene's Test, the p-value is greater than .05, therefore the equal variance assumption can be trusted. 

ScheffeTest(aov.bupa)
ScheffeTest(aov.bupa2)

#c) After both post-hoc test, both models show that group 5 has the largest mean of mcv and alkphos. 

#Question 3

#a) This includes the interaction effect 
#type 1 table
aov.psych_a= (aov(salary ~ sex + rank + sex*rank , data=psych))
summary(aov.psych_a)
#type 3 table
Anova(aov.psych, type=3)
#a) The type I test has p-values less than .05 for both sex and rank; so there exists a significant effect on the Salary based on the sex and rank. 
#The interaction between sex and rank is not significant because p-value is greater than .05. 
#The type III test has p-values less than .05 for both sex and rank; so there exists a significant effect on the Salary based on sex and rank.
#The interaction between the interaction effect is not significant in the type III model. 

#b) This does NOT included the interaction effect.
#type 1 table
aov.psych_b= (aov(salary ~ sex + rank , data=psych))
summary(aov.psych_b)
#type 3 table
Anova(aov.psych_b, type=3)
#b) Refitting the data without the interaction term
#The type I test has p-values less than .05 for both sex and rank; so there exists a significant effect on the Salary based on the sex and rank. 
#The interaction between sex and rank is not significant because p-value is greater than .05. 
#The type III test has has p-values less than .05 for both sex and rank; so there exists a significant effect on the Salary based on sex and rank. 

#c) Normality Assumption
dev.new(width = 100, height = 100, unit = "px")
par(mfrow=c(2,2))
boxplot(salary ~ sex,data=psych, main=" Salary by Sex ",
        xlab="Sex", ylab="Salary")
boxplot(salary ~ rank ,data=psych, main=" Salary by rank ",
        xlab="Rank", ylab="days")
#c) To validate normality a box plot was created for salary with sex and salary with rank. The box plots show that the data is normal. 

#d) Post-HOC Test
TukeyHSD(aov.psych_a)
TukeyHSD(aov.psych_b)

#Elimination Method
summary(aov(salary ~ sex + rank+ sex*rank, data=psych))
#Eliminate sex*rank (the interaction)
summary(aov(salary ~ sex + rank, data=psych))
#Both values are less than .05, so the final model should have only sex and rank without their interaction. 
#d From the post-hoc test, the Male Associate is greater than the Male Assistant and the Male Associate is greater than the female Assistant. 
#The final model only includes sex and rank, but not there interaction effect. 
#From the Tukey results, male salary is greater than female salary and associate rank salary is greater than assistant rank salary.


#Questions 4
cars_new$type <- as.factor(cars_new$type)
cars_new$origin <- as.factor(cars_new$origin)
cars_new$cylinders <- as.factor(cars_new$cylinders)
aov.mpg = (aov(mpg_highway ~ cylinders + origin + type, data= cars_new))
Anova(aov.mpg, type=3)
summary(aov.mpg)

aov.mpgk = (aov(mpg_highway ~ cylinders + type, data= cars_new))
summary(aov.mpgk)
#a) Based on the results from the first ANOVA model, cylinders and type should be kept, and origin should be taken out of the model because it has the largest p-value. 
#Based on the model that keeps only cylinders and type, cylinders accounts for more of the variation in the model when compared to the type. 

aov.mpgi = (aov(mpg_highway ~ cylinders + type + cylinders * type, data=cars_new))
summary(aov.mpgi)

#b) Based on the results from the ANOVA with cylinders, type, and the interaction effect of cylinders and type; all p-values are small, therefore there is a significant effect between
#cylinders, type and their interaction effect. Cylinders explains more of the variations in the model. 

ScheffeTest(aov.mpg)

#c) The Scheffe Test shows that cars with 4 cylinders have better fuel efficiency than 6 cylinder cars, Sedans have better fuel efficiency than Sports cars. 
#The origin has  a large p-value, and thus does not show that there is an effect on fuel efficiency. 