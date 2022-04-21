### Elementary Education Analysis ## 


library(GGally)
library(car)
library(splines)
library(bestglm)
library(ggplot2)


# Read in the data 

class <- read.table("https://mheaton.byu.edu/docs/files/Stat536/OutOfClassCaseStudies/Nonlinear/Schools/Data/SchoolResults.txt", header=TRUE)

ggpairs(class)
# Income not linear 
# possibly English and Lunch? 

avPlots(lm(Score~., data = class))

# Variable Selection
bestglm(class[,c(2:4,5,6,1)], IC = "AIC", method = "exhaustive")
# Eliminates ST ratio

# Try splines
n <- nrow(class)
knots <- 15

rmseknot <- rep(NA, knots)
rmse <- rep(NA, times = n)
for(j in 1:knots){
  for(i in 1:n){
    test.set <- class[i,]
    train.set <- class[-i,]
    
    # Try with spline for Income only first 
    fit <- lm(Score ~ ns(Income, df = j) + Lunch + Computer + Expenditure + English + STratio, 
              data = train.set)
    pred <- predict(fit, newdata = test.set)
    
    rmse[i] <- sqrt(mean((test.set$Score - as.numeric(pred))^2))
  }
  rmseknot[j] <- mean(rmse)
}

plot(rmseknot)
# 6 knots for Income is good 

nsfit <- gam(Score ~ ns(Income, df = 6) + Lunch + Computer + Expenditure + English, 
          data = class)
summary(nsfit)
AIC(nsfit)
avPlots(nsfit)

# Let's try Lunch as well
rmseknot <- rep(NA, knots)
rmse <- rep(NA, times = n)
for(j in 1:knots){
  for(i in 1:n){
    test.set <- class[i,]
    train.set <- class[-i,]
    
    # Try with spline for Income and Lunch
    fit <- lm(Score ~ ns(Income, df = 6) + ns(Lunch, df = j) + Computer + Expenditure + English + STratio, 
              data = train.set)
    pred <- predict(fit, newdata = test.set)
    
    rmse[i] <- sqrt(mean((test.set$Score - as.numeric(pred))^2))
  }
  rmseknot[j] <- mean(rmse)
}
plot(rmseknot)
# No knots is the best for Lunch 


# Let's try splines for English 
rmseknot <- rep(NA, knots)
rmse <- rep(NA, times = n)
for(j in 1:knots){
  for(i in 1:n){
    test.set <- class[i,]
    train.set <- class[-i,]
    
    # Try with spline for Income and English 
    fit <- lm(Score ~ ns(Income, df = 6) + Lunch + Computer + Expenditure + ns(English, df = j) + STratio, 
              data = train.set)
    pred <- predict(fit, newdata = test.set)
    
    rmse[i] <- sqrt(mean((test.set$Score - as.numeric(pred))^2))
  }
  rmseknot[j] <- mean(rmse)
}

plot(rmseknot)
# No knots is best for English


# Try loess smoothing - only can have 1-4 predictors?? 

# Try smooth.spline - only 2 variables?? 


final <- lm(Score~Lunch + Computer + Expenditure + ns(Income,6) + English + I(Income^2),
            data=class)

AIC(final)
summary(final)

poly <- lm(Score~ Lunch + Computer + Expenditure + I(English^2) + I(Income^2), 
           data = class)
AIC(poly)
confint(poly)

# Diminishing returns on Income??

# hold everything else constant at the average value, span Income 5-55 
incomes <- seq(5,55.5, length.out = 420)
new <- data.frame(Lunch = rep(mean(class$Lunch, times = 100)), Computer = rep(mean(class$Lunch, times = 100)),
                  Expenditure = rep(mean(class$Expenditure, times = 100)), English = rep(mean(class$English, times = 100)), 
                  Income = incomes)
preds <- as.data.frame(predict.lm(final, newdata = new, interval = "prediction"))
preds$income <- incomes

ggplot(data = preds, mapping = aes(x = income)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "lightsteelblue2") +
  geom_line(aes(y = fit), col = "dodgerblue4") + 
  ggtitle("Prediction Intervals and Income") +
  xlab("Income") +
  ylab("Score") +
  geom_point(mapping = aes(x = class$Income, y = class$Score), alpha = 0.3)

# English learning barrier? 

confint(final)

## Assumptions for final model 
# linearity 
avPlots(final)
# Independence 

# Normality 
hist(final$residuals, breaks = 20, 
     main = "Histogram of Residuals", xlab = "Residuals")
ks.test(final$residuals, "pnorm")

# Equal Variance 
ggplot(mapping = aes(x = final$fitted.values, y = final$residuals)) +
  geom_point() + xlab("Fitted Values") +
  ylab("Residuals") + ggtitle("Fitted Values vs. Residuals") +
  geom_hline(aes(yintercept = 0), lty = "dashed", col = "red")

# How well does the model predict?
# Cross validation

rmse <- rep(NA, times = n)
bias <- rep(NA, times = n)
cov <- rep(NA, times = n)

for(i in 1:n){
  test.set <- class[i,]
  train.set <- class[-i,]
    
  # Try with spline for Income and English 
  fit <- lm(Score~Lunch + Computer + Expenditure + ns(Income,6) + English + I(Income^2),
            data=train.set)
  pred <- as.data.frame(predict(fit, newdata = test.set, interval = "prediction"))
    
  rmse[i] <- sqrt(mean((test.set$Score - pred$fit)^2))
  bias[i] <- test.set$Score - pred$fit
  cov[i] <- (test.set$Score > pred$lwr) && (test.set$Score < pred$upr)
}

mean(rmse)
sd(class$Score)
mean(bias)
mean(cov)

