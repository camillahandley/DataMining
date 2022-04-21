# In-Class Letter Analysis 

library("caret")


# Read in the data
letters <- read.csv( "/Users/camillahandley/Desktop/Stat 536/letter_train.csv")


library(gbm)

test <- gbm(formula=letter~.,
    data= letters,
    distribution= "multinomial",
    shrinkage=0.2, #epsilon
    n.trees=20, #B
    interaction.depth=15, #number of  splits in tree
    cv.folds=4, #K-fold cross validation
    bag.fraction=.3) #percent of  data to use at each iteration
    

library(xgboost)
library(dplyr)
all <- as.factor(letters$letter)
label <- as.numeric(all)-1
letters <- letters %>% select(-c(letter))

n <- nrow(letters)

train.index = sample(n, floor(0.75*n))
train.data <- as.matrix(letters[train.index,])
train.label = label[train.index]
test.data = as.matrix(letters[-train.index,])
test.label = label[-train.index]

# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)


# Define the parameters for multinomial classification
num_class = length(levels(all))

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = num_class, 
                   "eta" = 0.2,
                   "subsample"= 0.6, 
                   "max_depth" = 12)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 10

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = xgb.train, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)

OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train.label + 1)

confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")



bst_model <- xgb.train(params = xgb_params,
                       data = xgb.train,
                       nrounds = nround)

# Predict hold-out test set
test_pred <- predict(bst_model, newdata = xgb.test)
test_prediction <- matrix(test_pred, nrow = num_class,
                          ncol=length(test_pred)/num_class) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test.label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")


test <- read.csv("/Users/camillahandley/Desktop/letter_test.csv")
test.data = as.matrix(test)
xgb.test = xgb.DMatrix(data=test.data)

# Predict hold-out test set
test_pred <- predict(bst_model, newdata = xgb.test)
test_prediction <- matrix(test_pred, nrow = num_class,
                          ncol=length(test_pred)/num_class) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test.label + 1,
         max_prob = max.col(., "last"))


