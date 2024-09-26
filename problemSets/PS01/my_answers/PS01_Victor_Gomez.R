#####################
# load libraries
PATH<-"C:/data/ecole/___TCD/Michaelmas_Term/stats_I/TCD-StatsI_Fall2024/problemSets/PS01/my_answers"
setwd(PATH)
# clear global .environment
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("ggplot2", "stargazer"),  pkgTest)

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data as vector
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
summary(y)

## Question 1


n <- length(y) # capture the number of observations
sgm <-sd(y) # Calculate empirical standard deviation
mu <- mean(y) # calculate empirical mean 

# Assuming y values follow a student law, we obtain the following confidence interval CI at 90% of confidence:

alpha <-0.1
CI_standard <- c(qt(alpha/2,df=n-1),qt(1-alpha/2,df=n-1))
CI <- CI_standard*sgm/sqrt(n) + mu
CI

## Question 2

# Hypothesis H0 is: "Average student IQ is lower or equal than the average IQ score among all the schools int he country."
# The test is done for an alpha-risk of 5%
# I.e. H0 := {mean_general >= mean_students}
# The test statistic is the mean.

alpha<-0.05
test<-t.test(y, mu=100, alternative = 'greater',conf.level = 1-alpha)
test
test$p.value

#H0 is not rejected.
#CI95 <- c(qnorm(0.025,mean=mu,sd=sgm),qnorm(0.975,mean=mu,sd=sgm))


#####################
# Problem 2
#####################

# read in expenditure data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
summary(expenditure)

#Question 1
#Create pairs plot and save it in a pdf file.
pdf("pairs.pdf")
pairs(expenditure[,2:5])
dev.off()

#Question 2

#Create basic graph
pdf("regions.pdf")
plot(expenditure$Region,expenditure$Y,xlab="Region", ylab="Y")
dev.off()

#Create box plot graph with values        
pdf("regions_boxplot.pdf")
boxplot(expenditure$Y ~ expenditure$Region,xlab="Region", ylab="Y",xaxt = "n")
axis(1, at=c(1,2,3,4), labels=c("Northeast","North Central","South","West"), las=0)
# Add data points with jitter for limiting points overlap.
mylevels <- as.numeric(levels(factor(expenditure$Region)))
levelProportions <- summary(expenditure$Region)/nrow(expenditure)
for(i in 1:length(mylevels)){
  thislevel <- mylevels[i]
  thisvalues <- expenditure[expenditure$Region==thislevel, "Y"]
  
  # take the x-axis indices and add a jitter, proportional to the N in each level
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=1, col=rgb(0,0,0,.9)) 
  
}
dev.off()
####################################################


# Question3 

# create scatterplot of Y and X1
pdf("X1.pdf")
plot(expenditure$X1, expenditure$Y,xlab="X1", ylab="Y")
dev.off()

pdf("YX1Region.pdf")
plot(expenditure$X1,
     expenditure$Y,
     col=expenditure$Region,
     pch=expenditure$Region,
     xlab="per capita personal income in state ($/pers)",
     ylab="per capita expenditure on shelters/housing assistance in state ($/pers)",
     main="Relationship between shelters/housing assistance and personal income")
# Add legend
legend(1000, 130, # x and y position of legend
       legend=c("Northeast", "North Central", "South", "West"),
       col= as.integer(names(table(expenditure$Region))),
       pch= as.integer(names(table(expenditure$Region))))
dev.off()


# run an example regression, to show how to save table
regression1 <- lm(Y~X1, data=expenditure)
# now save that output to a file that you can read in later to your answers
# make it easier for when we need to do this again, let's create a function
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("regression_output1.tex", regression1)
