# PS04
# Author: Victor Gomez
# Date : 18/11/2024
setwd("C:/data/ecole/___TCD/Michaelmas_Term/stats_I/TCD-StatsI_Fall2024/problemSets/PS04/my_answers")
# Question 1
# Q a
#install.packages(car)
library(car)
data(Prestige )
df<-Prestige
professional <- ifelse(df$type=="prof", 1, 
                       ifelse(df$type=="bc",0,
                       ifelse(df$type=="wc",0,NA)))
professional

# Q b
lm1 <- lm(formula = df$prestige ~ df$income +professional + df$income:professional)
summary(lm1)

lm2 <- lm(formula =  ifelse(professional==1,df$prestige,NA) ~ ifelse(professional==1,df$income,NA))
summary(lm2)

pdf("lm1.pdf")
plot(y=df$prestige,x=df$income,col = factor(professional), xlab="Income ($)", ylab="Prestige (Pineo-Porter)")
legend("bottomright",
       legend = levels(factor(professional)),
       pch = 19,
       col = factor(levels(factor(professional))))
abline(lm1)
dev.off()

pdf("lm2.pdf")
plot(df$income, df$prestige,
     col = factor(professional),
     xlab="Income ($)", ylab="Prestige (Pineo-Porter)")
legend("bottomright",
       legend = levels(factor(professional)),
       pch = 19,
       col = factor(levels(factor(professional))))
abline(lm2)
dev.off()

###########################################
#         Question 2                      #
###########################################

#Q c
f_test <- 6.6
n<-131
k<- 2
f_pvalue <- df (f_test  , k-1 , n )
