#Victoria Espinola
#STAT 6443 Homework #2
#20 September 2020

library(DescTools); library(MASS)

heartbpchol <- read.csv("~/Desktop/STAT_6443/heartbpchol.csv")

aov.res=aov(Cholesterol~BP_Status, data=heartbpchol)
summary(aov.res)
