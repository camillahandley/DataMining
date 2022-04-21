# IN class Admissions analysis

library(class)
library(GGally)
library(e1071)
library(caTools)
library(caret)


# Read in the data 

grad <- read.csv("/Users/camillahandley/Desktop/Stat 536/Admissions.csv")
ggpairs(grad)

# Change to a binary variable
grad$Status <- ifelse(grad$Status == "Admitted", 1, 0)

test <- grad[426:500,]
train <- grad[1:425,]

# Just try logistic regression as a baseline
log <- glm(Status~., data = train, family = binomial)
summary(log)

pred.prob <- predict(log, newdata = test, type = "response")

thresh <- seq(from=0, to=1, length=1000)
misclass <- rep(NA,length=length(thresh)) 

for(i in 1:length(thresh)) {
  #If probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.prob>thresh[i], 1, 0)
  
  # calculate the pct where my classification not equal truth
  misclass[i] <- mean(my.classification!=test$Status)
}
#Find threshold which minimizes miclassification
min_thresh <- thresh[which.min(misclass)]
plot(x=thresh, y = misclass)


pred_class <- ifelse(pred.prob>min_thresh,1,0)
(conf_matrix <- addmargins(table(factor(test$Status, levels=c(1,0)), factor(pred_class, levels=c(1,0)))))

# Find sensitivity, specificity, ppv and npv
(sens <- conf_matrix[1,1]/conf_matrix[1,3])
(spec <- conf_matrix[2,2]/conf_matrix[2,3])
(ppv <- conf_matrix[1,1]/conf_matrix[3,1])
(npv <- conf_matrix[2,2]/conf_matrix[3,2])

# Find model accuracy - F-Score
f_score <- 2 * (ppv*sens) / (ppv + sens)



### K Nearest Neighbors - have to center and scale the data
grad.scale <- as.data.frame(cbind(scale(grad[,-c(6,7)], center = T, scale = T), grad[,c(6,7)]))

test.scale <- grad.scale[426:500,-7]
train.scale <- grad.scale[1:425, -7]

resp.test <- test[,7]
resp.train <- train[,7]

# Run CV to find the best k
ks <- 1:100
fscores <- rep(NA, times = 100)
for(i in 1:100) {
  # Fit the KNN
  pr <- knn(train.scale,test.scale,cl=resp.train,k=i)
  ##create confusion matrix
  tab <- addmargins(table(factor(resp.test,levels=c(1,0)), factor(pr, levels=c(1,0))))
  sens <- tab[1,1]/tab[1,3]
  ppv <- tab[1,1]/tab[3,1]

  # Find model accuracy - F-Score
  fscores[i] <- 2 * (ppv*sens) / (ppv + sens)
}

max(fscores)
which.max(fscores)
# K =  1 is best 


# Try adding the predicted probabilities from logistic regression to the data??
preds.train <- predict(log, type = "response")

new.train <- cbind(train.scale, preds.train)
new.test <- cbind(test.scale, pred.prob)

# Run CV to find the best k
ks <- 1:100
fscores <- rep(NA, times = 100)
for(i in 1:100) {
  # Fit the KNN
  pr <- knn(new.train, new.test, cl=resp.train, k=i)
  ##create confusion matrix
  tab <- addmargins(table(factor(resp.test,levels=c(1,0)), factor(pr, levels=c(1,0))))
  sens <- tab[1,1]/tab[1,3]
  ppv <- tab[1,1]/tab[3,1]
  
  # Find model accuracy - F-Score
  fscores[i] <- 2 * (ppv*sens) / (ppv + sens)
}

max(fscores)
which.max(fscores)
# Doesn't improve F-score

# Try just putting the predicted probabilities from log regression 

# Run CV to find the best k
ks <- 1:100
fscores <- rep(NA, times = 100)
for(i in 1:100) {
  # Fit the KNN
  pr <- knn(as.matrix(preds.train), as.matrix(pred.prob), cl=resp.train, k=i)
  ##create confusion matrix
  tab <- addmargins(table(factor(resp.test,levels=c(1,0)), factor(pr, levels=c(1,0))))
  sens <- tab[1,1]/tab[1,3]
  ppv <- tab[1,1]/tab[3,1]
  
  # Find model accuracy - F-Score
  fscores[i] <- 2 * (ppv*sens) / (ppv + sens)
}
max(fscores)
which.max(fscores)


# Fitting Naive Bayes 
nb <- naiveBayes(Status ~ ., data = grad.scale[1:425,])

y_pred <- predict(nb, newdata = grad.scale[426:500,])

tab <- addmargins(table(factor(resp.test,levels=c(1,0)), factor(y_pred, levels=c(1,0))))
sens <- tab[1,1]/tab[1,3]
ppv <- tab[1,1]/tab[3,1]

f_score <- 2 * (ppv*sens) / (ppv + sens)





