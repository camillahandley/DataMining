#### Kelly Blue-book Analysis #### 

# Libraries 
library(ggplot2)
library(dplyr)
library(GGally)
library(bestglm)
library(car)
library(lmtest)

# Read in the data
setwd("/Users/camillahandley/Desktop/Stat 536")
bb <- read.csv("KBB.csv", stringsAsFactors = T)

# Exploratory analysis 
ggpairs(bb %>% select(c(Price, Mileage)))
ggplot(bb, mapping = aes(x = Mileage, y = Price)) +
  geom_point() + geom_smooth(se = F, method = "lm") +
  ggtitle("Resale Price and Mileage")
ggplot(bb, mapping = aes(x = as.factor(Leather), y = Price)) +
  geom_boxplot() + xlab("Leather Seats") +
  scale_x_discrete(labels=c("Does not have","Has")) +
  ggtitle("Resale Price and Leather Seats")
ggplot(bb, mapping = aes(x = as.factor(Sound), y = Price)) +
  geom_boxplot() + xlab("Upgraded Speakers") +
  scale_x_discrete(labels=c("Does not have","Has"))


# Play around with modifying Trim 
# Remove "4D" and "2D" because that doesn't tell us anything extra
bb$Trim <- sub("\\dD", "", bb$Trim)
# Remove Type names ("Sedan", "Conv", "Wagon", "Coupe", Hatchback)
bb$Trim <- sub("Sedan", "", bb$Trim)
bb$Trim <- sub("Conv", "", bb$Trim)
bb$Trim <- sub("Wagon", "", bb$Trim)
bb$Trim <- sub("Coupe", "", bb$Trim)
bb$Trim <- sub("Hatchback", "", bb$Trim)
bb$Trim <- sub("Hback", "", bb$Trim)

# Put all Sportwagons together 
bb$Trim <- sub("[[:upper:]]+ Sportwagon", "Sportwagon", bb$Trim)
# Put all CX together
bb$Trim <- sub("CX[[:upper:]]", "CX", bb$Trim)
# Put all G's together
bb$Trim <- sub("G[[:upper:]]+", "G", bb$Trim)

# Put all L's together
bb$Trim <- sub("L[[:upper:]] [[:upper:]]+", "L", bb$Trim)
bb$Trim <- sub("L[[:upper:]]", "L", bb$Trim)
bb$Trim <- sub("LS Sport", "L", bb$Trim)
bb$Trim <- sub("MAXX", "L", bb$Trim)

# Change all the blank ones to General
bb$Trim <- ifelse(bb$Trim == " ", "General", bb$Trim)
bb$Trim <- sub(" ", "", bb$Trim)



ggplot(bb, mapping = aes(x =Trim, y = Make)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# Make a new variable that combines Make and Trim
bb$MakeTrim <- NA
for (i in 1:nrow(bb)) {
  if (bb$Trim[i] == "Aero " | bb$Trim[i] == "Arc ") { bb$MakeTrim[i] <- "SAAB A"}
  else if (bb$Trim[i] == "DHS " | bb$Trim[i] == "DTS ") { bb$MakeTrim[i] <- "Cadillac D"}
  else if (bb$Trim[i] == "SL " | bb$Trim[i] == "SE ") { bb$MakeTrim[i] <- "Pontiac S"}
  else if (bb$Trim[i] == "G ") { bb$MakeTrim[i] <- "Pontiac G"}
  else if (bb$Trim[i] == "L " | bb$Trim[i] == "Lport ") { bb$MakeTrim[i] <- "Chevrolet L"}
  else if (bb$Trim[i] == "SS " | bb$Trim[i] == "SVM ") { bb$MakeTrim[i] <- "Chevrolet S"}
  else if (bb$Trim[i] == "Custom " | bb$Trim[i] == "CX ") { bb$MakeTrim[i] <- "Buick C"}
  else if (bb$Trim[i] == "General") { bb$MakeTrim[i] <- bb$Make[i] }
  else { bb$MakeTrim[i] <- bb$Make[i] }
}

# Let's just take out Trim all together - not much extra info 
bb <- bb %>% select(-Trim)

# Variable Selection
bestglm(bb[,c(2:11,1)], IC = "BIC", method = "exhaustive")
bestglm(bb[,c(2:11,1)], IC = "AIC", method = "exhaustive")
# Sticking with BIC


## Fit a model 
bb$LogMileage <- log(bb$Mileage)
bb.lm <- lm(log(Price) ~ LogMileage + Cylinder + Type + Model, data = bb)
summary(bb.lm)

# Checking Assumptions 

# Linearity 
avPlots(bb.lm, terms = ~LogMileage)
# Independence - Good 
# Normality 
hist(bb.lm$residuals)
# Equal Variance 
plot(bb.lm)
# This bad - let's ignore it
# bptest(bb.lm)
  

vif(bb.lm)
# Take out Liter - Liter and Cylinder are related 
ggplot(bb, mapping = aes(x = as.factor(Cylinder), y = Liter)) +
  geom_boxplot() + xlab("Number of Cylinders") +
  ggtitle("Comparing Liter and Cylinder")

# 1.What factors lead to higher/lower resale values? 
summary(bb.lm)

# 2. Are there other factors not included in this dataset that likely explain how much a car is worth? 
# If so, what other factors do you think explain resale value?

# If a car has been in an accident before or a history of mechanical problems 

# 3. Generally, as mileage increases, the price should decrease.  
# But, does the amount of de-crease in value from additional mileage differ depending upon the make of the car?  
# If so,which makes hold the value better with more miles?

bestglm(bb[,c(3,5:12,1)], IC = "BIC", method = "exhaustive")

bb.lm.noint <- lm(log(Price) ~ LogMileage + Cylinder + Type + Make, data = bb)
plot(bb.lm.noint)
bb.lm.int <- lm(log(Price) ~ LogMileage + Cylinder  + Type + Make + Make:LogMileage, data = bb)
anova(bb.lm.int, bb.lm.noint)

summary(bb.lm.int)

# 4. Which car (and with what characteristics) has the highest resale value at 15000 miles?

cars <- data.frame(LogMileage = log(15000), Model = bb$Model, Type = bb$Type, Cylinder = bb$Cylinder)
cars$preds <- exp(predict.lm(bb.lm, cars))
cars[which.max(cars$preds),]

# 5. What is a reasonable resale value for the following vehicle:  Cadillac CTS 4D Sedan with 17,000 miles,  
# 6 cylinder,  2.8 liter engine,  cruise control,  upgraded speakers and leather seats?

newcar <- data.frame(LogMileage = log(17000), Type = "Sedan", Model = "CTS", Cylinder = 6 )
mypred <- predict.lm(bb.lm, newcar,  interval = "prediction")
exp(mypred)


# Leave-one-out Cross Validation - Model 1 
n <- nrow(bb)
bias <- rep(NA, n)
rpmse <- rep(NA, n)
cover <- rep(NA, n)
width <- rep(NA, n)

for(i in 1:nrow(bb)) {
  # Split data into test and training sets
  test.set <- bb[i,]
  train.set <- bb[-i,]
  # Using training data to fit a (possibly transformed) model
  train.lm <-  lm(log(Price) ~ LogMileage + Cylinder + Type + Model, data = train.set)
  
  # Predict test set
  test.preds <- as.data.frame(exp(predict.lm(train.lm, newdata = test.set, interval = "prediction")))
  
  # Calculate bias
  bias[i] <- test.preds$fit - test.set$Price
  # Calculate RPMSE
  rpmse[i] <- sqrt((test.preds$fit - test.set$Price)^2)
  # Calculate coverage 
  cover[i] <- (test.preds$upr > test.set$Price && test.preds$lwr < test.set$Price)
  # Calcuate width
  width[i] <- test.preds$upr - test.preds$lwr
}

mean(bias)
mean(rpmse)
mean(cover)
mean(width)

  
# Leave-one-out Cross Validation - Model 2
n <- nrow(bb)
bias <- rep(NA, n)
rpmse <- rep(NA, n)
cover <- rep(NA, n)
width <- rep(NA, n)

for(i in 1:nrow(bb)) {
  # Split data into test and training sets
  test.set <- bb[i,]
  train.set <- bb[-i,]
  # Using training data to fit a (possibly transformed) model
  train.lm <-  lm(log(Price) ~ LogMileage + Cylinder  + Type + Make + Make:LogMileage, data = train.set)
  
  # Predict test set
  test.preds <- as.data.frame(exp(predict.lm(train.lm, newdata = test.set, interval = "prediction")))
  
  # Calculate bias
  bias[i] <- test.preds$fit - test.set$Price
  # Calculate RPMSE
  rpmse[i] <- sqrt((test.preds$fit - test.set$Price)^2)
  # Calculate coverage 
  cover[i] <- (test.preds$upr > test.set$Price && test.preds$lwr < test.set$Price)
  # Calcuate width
  width[i] <- test.preds$upr - test.preds$lwr
}

mean(bias)
mean(rpmse)
mean(cover)
mean(width)
  