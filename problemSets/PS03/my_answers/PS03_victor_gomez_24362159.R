# Script for assignment 3
# Author: Victor Gomez
# Date : 05/11/2024

## Initialization 

# Set wd for loading data
setwd("C:/data/ecole/___TCD/Michaelmas_Term/stats_I/TCD-StatsI_Fall2024/datasets")

# Load df
df <- read.csv("incumbents_subset.csv")
View(df)

# Set wd for proper work
setwd("C:/data/ecole/___TCD/Michaelmas_Term/stats_I/TCD-StatsI_Fall2024/problemSets/PS03/my_answers")
###########################################
#         Question 1                      #
########################################### 

# Q1 : Linear regression
lm1<-lm(formula=voteshare~difflog, data=df)

# Q2 : plotting
pdf("lm1.pdf")
plot(df$difflog,df$voteshare, 
     xlab="difflog", ylab="voteshare")
abline(lm1$coefficients, col=2)
dev.off()

#Q3 :  save residuals
lm1_residuals<- lm1$residuals
pdf("res1.pdf")
plot(df$difflog,lm1_residuals, 
     xlab="difflog", ylab="residuals model 1")
abline(a=0,b=0)
dev.off()

# Q4 : cf latex file

###########################################
#         Question 2                      #
###########################################

# Q1 : Linear regression
lm2<-lm(formula=presvote~difflog, data=df)

# Q2 : plotting
pdf("lm2.pdf")
plot(df$difflog,df$presvote, 
     xlab="difflog", ylab="presvote")
abline(lm2$coefficients, col=2)
dev.off()

#Q3 :  save residuals
lm2_residuals<- lm2$residuals
pdf("res2.pdf")
plot(df$difflog,lm2_residuals, 
     xlab="difflog", ylab="residuals model 2")
abline(a=0,b=0)
dev.off()

# Q4 : cf latex file

###########################################
#         Question 3                      #
###########################################

# Q1 : Linear regression
lm3<-lm(formula=voteshare~presvote, data=df)

# Q2 : plotting
pdf("lm3.pdf")
plot(df$presvote,df$voteshare, 
     xlab="presvote", ylab="voteshare")
abline(lm3$coefficients, col=2)
dev.off()

#Q3 :  save residuals
lm3_residuals<- lm3$residuals
pdf("res3.pdf")
plot(df$difflog,lm3_residuals, 
     xlab="presvote", ylab="residuals model 3")
abline(a=0,b=0)
dev.off()

# Q4 : cf latex file

###########################################
#         Question 4                      #
###########################################

# Q1 : Linear regression
lm4<-lm(formula=lm1_residuals~lm2_residuals, data=df)

# Q2 : plotting
pdf("lm4.pdf")
plot(lm2_residuals,lm1_residuals, 
     xlab="residuals model 2", ylab="residuals model 1")
abline(lm4$coefficients, col=2)
dev.off()

#save residuals
lm4_residuals<- lm4$residuals
pdf("res4.pdf")
plot(lm2_residuals,lm4_residuals,
     xlab="residuals model 2", ylab="residuals model 1")
abline(a=0,b=0)
dev.off()

# Q4 : cf latex file

###########################################
#         Question 5                      #
###########################################

# Q1 : Linear regression
lm5<-lm(formula=voteshare~difflog +  presvote, data=df)

# plotting
# load library car and tidyverse
library(car)
library(tidyverse)

pdf("lm5.pdf")
avPlots(lm5, layout= c(1,2))
dev.off()

# Q2 : cf latex file

# Q3 : 
# lm5_residuals<- lm5$residuals
# pdf("res5.pdf")
# plot(lm2_residuals,lm4_residuals,
#      xlab="residuals model 2", ylab="residuals model 1")
# abline(a=0,b=0)
# dev.off()

