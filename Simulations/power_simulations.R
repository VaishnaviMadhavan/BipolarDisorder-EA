
#
# Script: power_simulations.R
# Project: Bipolar Disorder and Education:
#              Deciphering a Complex Relationship using Genetic and Sibling Differences
#
# Purpose: Script for testing power
#
# Author: Vaishnavi K Madhavan
# Start date: 07-06-2024
# 

# Question: If we have a BIP PRS (5% of variance in BIP) and a stronger EA PRS (15% of variance in EA\< about 60% of that within siblings), 
# what are our expectations on power if we regress BIP on the EA PRS vs EA on the BIP PRS?
# Simulating power for 1. regressing bipolar disorder outcome on EA PRS and 2. regressing EA outcome on BIP PRS

# 1. Regressing bipolar disorder outcome on EA PGS #### 

set.seed(123)

rep <- 1000 
# no of times to repeat the analysis 

output1 <- matrix(NA,nrow=rep,ncol=2)
# creating the matrix for the output

for (i in 1:rep) 
  { 

N <- 40000
# sample size

R2EA <- .15
R2BIP <- .05
# variance R2 obtained from latest GWASs.

PRS_EA <- rnorm(N,0,1)
EA <- sqrt(R2EA) * PRS_EA + sqrt(1-R2EA)*rnorm(N,0,1)

PRS_BIP <- rnorm(N,0,1)
BIP <- sqrt(R2BIP)*PRS_BIP + sqrt(1-R2BIP)*rnorm(N,0,1) 
# creating EA and BIP variables

BIP2 <- .1*EA + BIP
EA2 <- .1*BIP + EA
# assume that there is a slight positive correlation between EA and BIP

BIP_bin <- as.numeric(scale(BIP2) > qnorm(.99))
# converting the BIP variable into a dichotomous variable with a prevalence of ~1%

dataf <- cbind.data.frame(PRS_EA, PRS_BIP, EA2, BIP_bin)
# form into a dataframe

reg1 <- glm(BIP_bin ~ PRS_EA, data=dataf) 
# running the generalised linear model
  
output1[i,] <- summary(reg1)$coef[2,c(1,4)] 
# saving the estimate beta and p-value of the regression 
  }

sum(output1[,2] < 0.05)/rep  
# power = 0.549

  
# 2. Regressing EA outcome on BIP PGS ####

set.seed(123)

rep <- 1000 
# no of times to repeat the analysis 

output2 <- matrix(NA,nrow=rep,ncol=2)
# creating the output matrix

for (i in 1:rep) 
{ 
N <- 40000
# sample size 

R2EA <- .15
R2BIP <- .05
# variance R2 obtained from latest GWASs.

PRS_EA <- rnorm(N,0,1)
EA <- sqrt(R2EA) * PRS_EA + sqrt(1-R2EA)*rnorm(N,0,1)

PRS_BIP <- rnorm(N,0,1)
BIP <- sqrt(R2BIP)*PRS_BIP + sqrt(1-R2BIP)*rnorm(N,0,1) 
# creating the EA and BIP variables

BIP2 <- .1*EA + BIP
EA2 <- .1*BIP + EA
# assume that there is a slight positive correlation between EA and BIP

dataf <- cbind.data.frame(PRS_BIP, EA2)
  
reg2 <- lm(EA2 ~ PRS_BIP, data=dataf) 
# running the regression

output2[i,] <- summary(reg2)$coef[2,c(1,4)] 
# saving the estimate beta and p-value of the regression

}

sum(output2[,2] < 0.05)/rep  
# power = 0.993

