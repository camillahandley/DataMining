### Credit Card Analysis ### 

library(ggplot2)
library(GGally)
library(dplyr)
library(bestglm)
library(car)
library(multcomp)

setwd("/Users/camillahandley/Desktop/Stat 536")
credit <- read.csv("CreditDebt.csv", stringsAsFactors = T)

## Exploratory Data Analysis 

# Are the collected variables able to adequately predict a person’s credit balance?
# Quantitative variables 
quant <- credit %>% select(Income, Limit, Rating, Cards, Age, Education, Balance)
ggpairs(quant)

# Do people generally get “more responsible” (in terms of lower balances) with money as they age?
ggplot(data = credit, mapping = aes(x = Age, y = Balance)) + 
  geom_point() + ggtitle("Credit Card Debt and Age ")
cor(credit$Balance, credit$Age)

# The policy states that limit should increase by 10% of the income increase. 
# Under this policy, what is the expected difference in credit balance when a person’s income goes up by 10,000?
ggplot(data = credit, mapping = aes(x = Income, y = Balance)) + 
  geom_point() + ggtitle("Credit Card Debt and Income ") +
  geom_smooth(se = FALSE, method = "lm") 

ggplot(data = credit, mapping = aes(x = Limit, y = Balance)) + 
  geom_point() + ggtitle("Credit Card Debt and Credit Limit ") +
  geom_smooth(se = FALSE, method = "lm") 


##### In-class analysis 


# Variable Selection
bestglm(credit, IC = "BIC", method = "exhaustive")

## Fit a model 
credit.lm <- lm(Balance ~ Income + Limit + Age + Student, data = credit)

# Checking Assumptions 

# Linearity 
avPlots(credit.lm)
# Independence - Good 
# Normality 
hist(credit.lm$residuals)
# Equal Variance 
plot(credit.lm)

# 1. Are the collected variables able to adequately predict a person’s credit balance
# Check R2 to see how well our model fits the data
summary <- summary(credit.lm)
summary$r.squared

# Leave-one-out Cross Validation
n <- nrow(credit)
bias <- rep(NA, n)
rpmse <- rep(NA, n)
cover <- rep(NA, n)
width <- rep(NA, n)

for(i in 1:nrow(credit)) {
  # Split data into test and training sets
  test.set <- credit[i,]
  train.set <- credit[-i,]
  # Using training data to fit a (possibly transformed) model
  train.lm <-  lm(Balance ~ Income + Limit + Age + Student, data = train.set)
  
  # Predict test set
  test.preds <- as.data.frame(predict.lm(train.lm, newdata = test.set, interval = "prediction"))
  
  # Calculate bias
  bias[i] <- test.preds$fit - test.set$Balance
  # Calculate RPMSE
  rpmse[i] <- sqrt((test.preds$fit - test.set$Balance)^2)
  # Calculate coverage 
  cover[i] <- (test.preds$upr > test.set$Balance && test.preds$lwr < test.set$Balance)
  # Calcuate width
  width[i] <- test.preds$upr - test.preds$lwr
}

mean(bias)
mean(rpmse)
mean(cover)
mean(width)

# 2. Do people generally get “more responsible” (in terms of lower balances) with money as they age 
summary$coefficients

# As you age one year, your balance is predicted to go down by $2 - not practically significant 
# Technically people do get "more responsible" but only by a little bit 


# 3. What is the expected difference in credit balance when a person’s income goes up by 10,000
policy <- glht(credit.lm, linfct = matrix(c(0, 10, 1000, 0, 0), nrow = 1))
summary(policy)


# 3.b  Based on your results, what should the companies policy be for increasing credit limit for increases in in-come?
# Depends on what the optimal balance is for the company - we don't have enough info on how much balance is risky 
# we suggest 7.5% increase in limit and maybe setting an overall cap on limt

policy2 <- glht(credit.lm, linfct = matrix(c(0, 10, 750, 0, 0), nrow = 1))
summary(policy2)

income <- glht(credit.lm, linfct = matrix(c(0, 10, 0, 0, 0), nrow = 1))
summary(income)


