##### Credit Card Fraud Analysis ##### 

library(class)
library(GGally)
library(e1071)
library(caTools)
library(caret)

# Read in the data 
credit <- read.csv("/Users/camillahandley/Desktop/Stat 536/CCFraud.csv")


# Just try logistic regression as a baseline

# Create a test and training set 
test.rows <- sample(nrow(credit), size = nrow(credit)*.2)
test <- credit[test.rows,]
train <- credit[-test.rows,]

log.train <- glm(Class~., data = train, family = binomial)
summary(log.train)

pred.prob <- predict(log.train, newdata = test, type = "response")

thresh <- seq(from=0, to=1, length=1000)
misclass <- rep(NA,length=length(thresh)) 

for(i in 1:length(thresh)) {
  #If probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.prob>thresh[i], 1, 0)
  
  # calculate the pct where my classification not equal truth
  misclass[i] <- mean(my.classification!=test$Class)
}
#Find threshold which minimizes miclassification
min_thresh <- thresh[which.min(misclass)]

pred_class <- ifelse(pred.prob>min_thresh,1,0)
(conf_matrix <- addmargins(table(factor(test$Class, levels=c(1,0)), factor(pred_class, levels=c(1,0)))))

# Find sensitivity, specificity, ppv and npv
(sens <- conf_matrix[1,1]/conf_matrix[1,3])
(spec <- conf_matrix[2,2]/conf_matrix[2,3])
(ppv <- conf_matrix[1,1]/conf_matrix[3,1])
(npv <- conf_matrix[2,2]/conf_matrix[3,2])

# Find model accuracy - F-Score
(f_score <- 2 * (ppv*sens) / (ppv + sens))

# Try calibrating the probabilities 
logFull <- glm(Class~., data = credit, family = binomial)
predict <- data.frame(pred = predict(logFull, type = 'response'), 
                      class = credit$Class)

sorted <- predict[order(predict$pred),] 
n = nrow(predict)
ggplot(sorted, aes(x = cumsum(class)/n, y =  cumsum(pred)/n)) +
  geom_point()

credit$pred <- predict(logFull, type = 'response')
Calibrate <- glm(Class~pred, data = credit, family = binomial)

pred.prob <- predict(Calibrate, type = "response")

thresh <- seq(from=0, to=1, length=1000)
misclass <- rep(NA,length=length(thresh)) 

for(i in 1:length(thresh)) {
  #If probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.prob>thresh[i], 1, 0)
  
  # calculate the pct where my classification not equal truth
  misclass[i] <- mean(my.classification!=credit$Class)
}
#Find threshold which minimizes miclassification
min_thresh <- thresh[which.min(misclass)]

pred_class <- ifelse(pred.prob>min_thresh,1,0)
(conf_matrix <- addmargins(table(factor(credit$Class, levels=c(1,0)), 
                                 factor(pred_class, levels=c(1,0)))))

# Find sensitivity, specificity, ppv and npv
(sens <- conf_matrix[1,1]/conf_matrix[1,3])
(spec <- conf_matrix[2,2]/conf_matrix[2,3])
(ppv <- conf_matrix[1,1]/conf_matrix[3,1])
(npv <- conf_matrix[2,2]/conf_matrix[3,2])

# Find model accuracy - F-Score
(f_score <- 2 * (ppv*sens) / (ppv + sens))
# 0.7881




# Try Naive Bayes 
#Setting outcome variables as categorical
credit <- read.csv("/Users/camillahandley/Desktop/Stat 536/CCFraud.csv")
credit.scale <- as.data.frame(cbind(scale(credit[,-c(30)], center = T, scale = T), credit$Class))

test.scale <- credit.scale[test.rows,]
train.scale <- credit.scale[-test.rows,]


ggplot(data = credit.scale, aes(V1)) + 
  geom_histogram(bins = 40) +
  ylab("Frequency") +
  xlab("V1 - Centered and Scaled") +
  ggtitle("Histogram of V1")

ggplot(data = credit.scale, aes(V19)) + 
  geom_histogram(bins = 40) +
  ylab("Frequency") +
  xlab("V19 - Centered and Scaled") +
  ggtitle("Histogram of V19")

# library(ROSE)
# train_balanced_both <- ovun.sample(V30 ~ ., data = train.scale, method = "both", p=0.5,N=nrow(train.scale), seed = 1)$data
# test_balanced_both <- ovun.sample(V30 ~ ., data = test.scale, method = "both", p=0.5,N=nrow(test.scale), seed = 1)$data
# 
# #look at class distributions
# table(train_balanced_both$V30)
# table(test_balanced_both$V30)
# 
# y.train <- factor(train_balanced_both$V30, levels = c(0,1), labels = c("False", "True"))
# y.test <- factor(test_balanced_both$V30, levels = c(0,1), labels = c("False", "True"))

#model = train(train_balanced_both[,-30], y.train, 'nb',trControl=trainControl(method='cv',number=10))

y.train <- factor(train.scale[,30], levels = c(0,1), labels = c("False", "True"))
y.test <- factor(test.scale[,30], levels = c(0,1), labels = c("False", "True"))

model = train(train.scale[,-30], y.train, 'nb',trControl=trainControl(method='cv',number=10))
model

Predict <- predict(model, newdata = as.data.frame(cbind(test.scale, y.test))) 
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(Predict, y.test)

f_score <- 2 * (0.8478*0.3291) / (0.3291 + 0.8478)


X <- varImp(model)
plot(X)



nb <- naiveBayes(V30 ~ ., data = credit.scale)
y_pred <- predict(nb, newdata = credit.scale[,-30])

tab <- addmargins(table(factor(credit.scale$V30,levels=c(1,0)), factor(y_pred, levels=c(1,0))))
sens <- tab[1,1]/tab[1,3]
ppv <- tab[1,1]/tab[3,1]

f_score <- 2 * (ppv*sens) / (ppv + sens)

nrow(test[test$Class == 1,])



# Try with Kernel Density estimation
library(naivebayes)
kernel <- naive_bayes(x = train.scale %>% select(-c(V30)), y = as.factor(train.scale$V30), 
                      usekernel = T)


pred.outsamp <- predict(kernel, newdata = test.scale[,-30])
tab <- addmargins(table(factor(test.scale[,30],levels=c(1,0)), factor(pred.outsamp, levels=c(1,0))))
sens <- tab[1,1]/tab[1,3]
ppv <- tab[1,1]/tab[3,1]


kernel <- naive_bayes(x = credit.scale %>% select(-c(V30)), y = as.factor(credit.scale$V30), 
                      usekernel = T)
pred.insamp <- predict(kernel, newdata = credit.scale[,-30])
tab <- addmargins(table(factor(credit.scale[,30],levels=c(1,0)), factor(pred.insamp, levels=c(1,0))))
sens <- tab[1,1]/tab[1,3]
ppv <- tab[1,1]/tab[3,1]


# Cross validation 
## Choose number of CV studies to run in a loop & test set size
n.cv <- 300
n.test <- round(.1*nrow(credit))

## Initialize matrices to hold CV results
sens <- rep(NA, n.cv)
ppv <- rep(NA, n.cv)

## Begin for loop
for(cv in 1:n.cv){
  ## Separate into test and training sets
  test.obs <- sample(1:nrow(credit), n.test)
  test.set <- credit.scale[test.obs,]
  train.set <- credit.scale[-test.obs,]
  
  train.model <- naive_bayes(x = train.set %>% select(-c(V30)), y = as.factor(train.set$V30), 
                        usekernel = T)
  
  ## Use fitted model to predict test set
  preds <- predict(train.model, newdata = test.set[,-30])
  
  ## Create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$V30, levels=c(1,0)), factor(preds, levels=c(1,0))))
  
  ## Pull of sensitivity, specificity, PPV and NPV using bracket notation
  sens[cv] <- conf.mat[1,1]/conf.mat[1,3]
  ppv[cv] <- conf.mat[1,1]/conf.mat[3,1]
}


SENS <- mean(sens[!is.na(sens)])
PPV <- mean(ppv[!is.na(ppv)])

nrow(credit %>% filter(Class == 1)) / nrow(credit)
