#### Global River Flow Analysis #### 


# Libraries 
library(GGally)
library(pls)
library(dplyr)
library("glmnet") 
library("ggplot2") 
library("caret") 

# Read in the data 
river <- read.csv("/Users/camillahandley/Desktop/Stat 536/Rivers.csv")

# See the top correlated variables 
rivercor <- data.frame(variable = colnames(river), cor = rep(NA, ncol(river)))
for(i in 1:ncol(river)) {
  rivercor$cor[i] <- abs(cor(river[,i], river$Metric))
}
rivercor %>% arrange(desc(cor)) %>% head() %>% View()

ggpairs(river[,1:6])

# Take out variables with zero variance and Lat/Lon
river <- river[,-c(95:98)]

# PCA will take care of collinearity 

# Principal Components Regression

# Look to see how many components to choose 
pcr.fit=pcr(Metric~., data=river,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
# Maybe around 45 if we want to explain at least 85% of variation in Metric
# and have a low cross validated RPMSE (passes 1 at around 65)

set.seed(13)
# Now try with half the data (Cross Validate)
n <- nrow(river)
sample <- sample(1:nrow(river), size = 0.5*n)
train <- river[sample,]
test <- river[-sample,]

x=model.matrix(Metric~.,river)[,-1]
y=river$Metric

pcr.fit.train <- pcr(Metric~., data=train,scale=TRUE, validation="CV")
summary(pcr.fit.train)
validationplot(pcr.fit.train,val.type="MSEP")
# This says 12 components 

pcr.pred=predict(pcr.fit.train,x[-sample,],ncomp=12)
mean((pcr.pred-test$Metric)^2)
# MSE of 0.34

pcr.fit=pcr(y~x,scale=TRUE,ncomp=12)
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")




