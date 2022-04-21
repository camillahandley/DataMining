## Targeted Marketing Analysis ## 

library(ggplot2)
library(bestglm)
library(pROC)
library(glmnet)
library(tidyverse)
library(caret)
library(splines)


# Read in the data 
market <- read.csv("/Users/camillahandley/Desktop/Stat 536/TargetMarketing.csv", sep = ";")

market$y <- ifelse(market$y == "yes", 1, 0)

market %>% 
  group_by(y) %>%
  summarise(num.obs = length(y)) %>% View()

# Combine single and unknown
market$marital <- ifelse(market$marital == "unknown", "single", market$marital)

# Throw the 3 observations out
market <- market %>% filter(default != "yes")

# Combine housing and loan to have five levels (yesyes, yesno, noyes, nono and unknown)

# Change poutcome to be nonexistent for these people 
# test <- market %>% filter(poutcome != "nonexistent" & pdays == 999)
market$house.loan <- NA
for(i in 1:nrow(market)){
  # Change poutcome to be nonexistent for the special case
  if(market$poutcome[i] != "nonexistent" & market$pdays[i] == 999){
    market$poutcome[i] <- "nonexistent"
  }
  if(market$housing[i] == "yes" & market$loan[i] == "yes") {
    market$house.loan[i] <- "YesYes"
  }
  if(market$housing[i] == "yes" & market$loan[i] == "no") {
    market$house.loan[i] <- "YesNo"
  }
  if(market$housing[i] == "no" & market$loan[i] == "yes") {
    market$house.loan[i] <- "NoYes"
  }
  if(market$housing[i] == "no" & market$loan[i] == "no") {
    market$house.loan[i] <- "NoNo"
  }
  if(market$housing[i] == "unknown" & market$loan[i] == "unknown") {
    market$house.loan[i] <- "Unknown"
  }
}


# Make two different columns pdays+1 given failure and pdays+1 given success 
# all nonexistent poutcomes will be zero for both columns
market$pdays.fail <- ifelse(market$poutcome == "failure", market$pdays + 1, 0)
market$pdays.succ <- ifelse(market$poutcome == "success", market$pdays + 1, 0)
market <- market %>% select(-c(poutcome, pdays, housing, loan)) 

# Month - note that january and february are missing 

# Try turning this to 0 
#v market$pdays <- ifelse(market$pdays == 999, 0, market$pdays)

# Only 3 observations with yes 
market %>% 
  group_by(default, y) %>%
  summarise(num.obs = length(default)) %>% View()

market %>% 
  group_by(marital, y) %>%
  summarise(num.obs = length(marital)) %>% View()

market %>% 
  group_by(housing, y) %>%
  summarise(num.obs = length(housing)) %>% View()
market %>% 
  group_by(loan, y) %>%
  summarise(num.obs = length(loan)) %>% View()

market %>% 
  group_by(month) %>%
  summarise(num.obs = length(month)) %>% View()

# split into seasons? 
# market$month <- ifelse( market$month == "dec", "winter", market$month)
# market$month <- ifelse(market$month == "apr" | market$month == "mar" | market$month == "may", 
#                        "spring", market$month)
# market$month <- ifelse(market$month == "jul" | market$month == "aug" | market$month == "jun", 
#                        "summer", market$month)
# market$month <- ifelse(market$month == "oct" | market$month == "nov" | market$month == "sep", 
#                        "fall", market$month)

market %>% 
  group_by(poutcome, y) %>%
  summarise(num.obs = length(poutcome)) %>% View()


# Visualize the continuous variables 
ggplot(data = market, mapping = aes(x = age, y = y)) + 
  geom_smooth() + xlab("Age") + ylab("Open New Account") +
  ggtitle("The Effect of Age on Opening a New Account")
  geom_vline(xintercept = 45)
# non-monotone here 
# specify where the knots are (non-monotonicity - not linearity 2 knots?)
# Specify a knot at age = 45
 
ggplot(data = market, mapping = aes(x = campaign, y = y)) + geom_smooth()


ggplot(data = market, mapping = aes(x = previous, y = y)) + 
  geom_jitter(width=1, height=.2) + 
  geom_smooth(se=FALSE)
  


ggplot(market) +
  geom_smooth(aes(pdays.fail, y)) +
  geom_vline(aes(xintercept = 9)) +
  geom_vline(aes(xintercept = 17))
# do knots at 9, 17
ggplot(market) +
  geom_smooth(aes(pdays.succ, y)) +
  geom_vline(aes(xintercept = 3)) +
  geom_vline(aes(xintercept = 5)) +
  geom_vline(aes(xintercept = 7.5)) +
  geom_vline(aes(xintercept = 12)) +
  geom_vline(aes(xintercept = 15.5)) +
  geom_vline(aes(xintercept = 19))
# do knots at 3, 5, 7.5, 12, 15.5, 19

ggplot(data = market, mapping = aes(x = as.factor(y), y = campaign)) + 
  geom_boxplot() + 
  xlab("Open New Account") + ylab("Current Number of Contacts")

ggplot(data = market, mapping = aes(x = as.factor(y), y = previous)) + 
  geom_boxplot() + 
  xlab("Open New Account") + ylab("Previous Number of Contacts")



market$job <- as.factor(market$job)
market$marital <- as.factor(market$marital)
market$education <- as.factor(market$education)
market$contact <- as.factor(market$contact)
market$month <- as.factor(market$month)
#market$poutcome <- as.factor(market$poutcome)
market$house.loan <- as.factor(market$house.loan)
#market$housing <- as.factor(market$housing)
#market$loan <- as.factor(market$loan)

# This isn't working - try lasso regression
# vs1 <- bestglm(market, IC = "BIC", method = "backward", family=binomial)


# Split the data into a training and test set 
set.seed(123)
training.samples <- market$y %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- market[training.samples, ]
test.data <- market[-training.samples, ]



x <- model.matrix(y ~ bs(age, knots = 45, degree = 1)
                      + bs(pdays.fail, knots = c(9, 17), degree = 1)
                      + bs(pdays.succ, knots = c(3, 5, 7.5, 12, 15.5, 19), degree = 1)
                      + job + marital + education + default + contact + month +
                      + day_of_week + campaign + previous + house.loan, train.data)
y <- train.data$y

# Find the best lambda using cross validation
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
best_lam <- cv.lasso$lambda.1se
plot(cv.lasso)

# Fit the best model on the training data
#model <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)

#coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, cv.lasso$lambda.1se) # Simpler model sometimes 

train.model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = best_lam)

# now fit a model with all the data 
lambda.min <- 0.0006960516
# This is the lambda that Jared had 



#Find threshold that minimizes misclassification 
pred.prob <- predict(train.model, newx=model.matrix(y~bs(age, knots = 45, degree = 1)
                                                    + bs(pdays.fail, knots = c(9, 17), degree = 1)
                                                    + bs(pdays.succ, knots = c(3, 5, 7.5, 12, 15.5, 19), degree = 1)
                                                    + job + marital + education + default + contact + month +
                                                      + day_of_week + campaign + previous + house.loan, test.data), type="response")


thresh <- seq(from=0, to=1, length=1000)
misclass <- rep(NA,length=length(thresh)) 

for(i in 1:length(thresh)) {
  #If probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.prob>thresh[i], 1, 0)
  
  # calculate the pct where my classification not equal truth
  misclass[i] <- mean(my.classification!=test.data$y)
}
#Find threshold which minimizes miclassification
min_thresh <- thresh[which.min(misclass)]
plot(x=thresh, y = misclass)


# Fit the in-sample model
X <- model.matrix(y ~ bs(age, knots = 45, degree = 1)
                  + bs(pdays.fail, knots = c(9, 17), degree = 1)
                  + bs(pdays.succ, knots = c(3, 5, 7.5, 12, 15.5, 19), degree = 1)
                  + job + marital + education + default + contact + month +
                    + day_of_week + campaign + previous + house.loan, market)
Y <- market$y

model <- glmnet(X, Y, alpha = 1, family = "binomial", lambda = best_lam)


# Find IN-sample AUC, sensitivity and specificity

pred.probs <- predict(model, newx=model.matrix(y~bs(age, knots = 45, degree = 1)
                                              + bs(pdays.fail, knots = c(9, 17), degree = 1)
                                              + bs(pdays.succ, knots = c(3, 5, 7.5, 12, 15.5, 19), degree = 1)
                                              + job + marital + education + default + contact + month +
                                                + day_of_week + campaign + previous + house.loan, market), type="response")

my.roc <- roc(market$y, as.numeric(pred.probs))
ggplot() + geom_line(aes(x=1-my.roc[["specificities"]], y=my.roc[["sensitivities"]])) + 
  geom_abline(intercept=0, slope=1)
auc(my.roc)

# Create a confusion matrix 
pred_class <- ifelse(pred.probs>min_thresh,1,0)
(conf_matrix <- addmargins(table(factor(market$y, levels=c(1,0)), factor(pred_class, levels=c(1,0)))))

# Find sensitivity, specificity, ppv and npv
(sens <- conf_matrix[1,1]/conf_matrix[1,3])
(spec <- conf_matrix[2,2]/conf_matrix[2,3])
(ppv <- conf_matrix[1,1]/conf_matrix[3,1])
(npv <- conf_matrix[2,2]/conf_matrix[3,2])

# Find model accuracy - F-Score
f_score <- 2 * (ppv*sens) / (ppv + sens)

mean(pred_class == market$y)


## Choose number of CV studies to run in a loop & test set size
n.cv <- 500
n.test <- round(.1*nrow(market))

## Set my threshold for classifying
cutoff <- min_thresh

## Initialize matrices to hold CV results
sens <- rep(NA, n.cv)
spec <- rep(NA, n.cv)
ppv <- rep(NA, n.cv)
npv <- rep(NA, n.cv)
auc <- rep(NA, n.cv)

## Begin for loop
for(cv in 1:n.cv){
  ## Separate into test and training sets
  test.obs <- sample(1:nrow(market), n.test)
  test.set <- market[test.obs,]
  train.set <- market[-test.obs,]
  
  ## Fit best model to training set
  train.x <- model.matrix(y ~ bs(age, knots = 45, degree = 1)
                    + bs(pdays.fail, knots = c(9, 17), degree = 1)
                    + bs(pdays.succ, knots = c(3, 5, 7.5, 12, 15.5, 19), degree = 1)
                    + job + marital + education + default + contact + month +
                      + day_of_week + campaign + previous + house.loan, train.set)
  train.y <- train.set$y
  train.model <- glmnet(train.x, train.y, alpha = 1, family = "binomial", lambda = best_lam)
  
  ## Use fitted model to predict test set
  pred.probs <- predict(model, newx=model.matrix(y~bs(age, knots = 45, degree = 1)
                                                 + bs(pdays.fail, knots = c(9, 17), degree = 1)
                                                 + bs(pdays.succ, knots = c(3, 5, 7.5, 12, 15.5, 19), degree = 1)
                                                 + job + marital + education + default + contact + month +
                                                   + day_of_week + campaign + previous + house.loan, test.set), type="response")
  
  ## Classify according to threshold
  test.class <- ifelse(pred.probs>cutoff, 1, 0)
  
  ## Create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$y, levels=c(1,0)), factor(test.class, levels=c(1,0))))
  
  ## Pull of sensitivity, specificity, PPV and NPV using bracket notation
  sens[cv] <- conf.mat[1,1]/conf.mat[1,3]
  spec[cv] <- conf.mat[2,2]/conf.mat[2,3]
  ppv[cv] <- conf.mat[1,1]/conf.mat[3,1]
  npv[cv] <- conf.mat[2,2]/conf.mat[3,2] 
  
  ## Calculate AUC
  auc[cv] <- auc(roc(test.set$y, as.numeric(pred.probs)))
}

AUC <- mean(auc)
SENS <- mean(sens)
SPEC <- mean(spec)
PPV <- mean(ppv)
NPV <- mean(npv)



Y <- matrix(market$y, ncol = 1)

 ## Boot strapping code from Jared 
lasso_mod2 <- glmnet(X, Y, alpha=1, lambda = best_lam)
coefs_point <- predict(lasso_mod2, s=best_lam, type = 'coefficients')
n_obs <- nrow(market)
# get bootstrap CI
nreps <- 1000
coefs <- matrix(NA, nrow = dim(coefs_point)[1], ncol = nreps)
for (i in 1:nreps){
  this_index <- sample(1:n_obs, size = n_obs, replace = TRUE)
  this_X <- X[this_index,]
  this_y <- Y[this_index,]
  this_lasso_mod <- glmnet(this_X, this_y, alpha=1, lambda = best_lam)
  this_coefs <- predict(this_lasso_mod, s=best_lam, type = 'coefficients')
  coefs[,i] <- this_coefs[,1]
}
saveRDS(coefs, 'coefs_bootstrap.rds')
coefs <- readRDS('coefs_bootstrap.rds')
coef_est <- tibble(coef = coefs_point@Dimnames[[1]],
                   point = coefs_point[,1],
                   se_b = apply(coefs, 1, sd)) %>%
  mutate(lb_95 = apply(coefs, 1, quantile, probs = 0.025),
         ub_95 = apply(coefs, 1, quantile, probs = 0.975)) %>%
  .[-2,]
effect_est <- coef_est %>%
  mutate(point = 100 * (exp(point) - 1),
         lb_95 = 100 * (exp(lb_95) - 1),
         ub_95 = 100 * (exp(ub_95) - 1)) %>%
  select(-se_b) %>%
  mutate(sig = (sign(lb_95) ==  sign(ub_95)))


colnames(coef_est) <- c("coef","point", "se" , "lb" ,"ub")
coef_est$point <- round(coef_est$point, digits = 3)
coef_est$lb <- round(coef_est$lb, digits = 3)
coef_est$ub <- round(coef_est$ub, digits = 3)
write.csv(coef_est, "/Users/camillahandley/Desktop/coef.csv")



