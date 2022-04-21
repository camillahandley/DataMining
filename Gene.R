#### Gene Expression Analysis  #### 

# Libraries I need 
library(dplyr)
library (ggplot2)
library(car)
library(caret)
library(glmnet)

# Read in the data
genes <- read.table("https://mheaton.byu.edu/docs/files/Stat536/InClassCaseStudies/2%20-%20DimRedPenalized/GeneExpression/Data/GeneExpression2.txt", 
                    header = T)

# Exploratory Data Analysis 

# See the top correlated variables 
genecor <- data.frame(gene = colnames(genes), correl = rep(NA, ncol(genes)))
for(i in 1:ncol(genes)) {
  genecor$correl[i] <- cor(genes[,i], genes$Malignant)
}

genecor %>% arrange(desc(correl)) %>% head() %>% View()


# Maybe plot the highest correlated genes with Malignancy 
ggplot(genes, mapping = aes(x = X37639_at, y = Malignant)) + 
  geom_point() + geom_smooth(se = F)

ggplot(genes, mapping = aes(x = X41468_at, y = Malignant)) + 
  geom_point() + geom_smooth(se = F)



# Elastic Net Regression
set.seed(536)

# X and Y datasets 
Y <- genes %>%  
  select(Malignant) %>%  
  scale(center = TRUE, scale = FALSE) %>%  
  as.matrix() 
X <- genes %>%  
  select(-Malignant) %>%  
  as.matrix() 

# Model Building : Elastic Net Regression 
control <- trainControl(method = "repeatedcv", 
                        number = 5, 
                        repeats = 2, 
                        search = "random", 
                        verboseIter = TRUE) 

# Training ELastic Net Regression model 
elastic_model <- train(Malignant ~ ., 
                       data = genes, 
                       method = "glmnet", 
                       preProcess = c("center", "scale"), 
                       tuneLength = 25, 
                       trControl = control) 

elastic_model 

# Model Prediction 
y_hat <- predict(elastic_model, X) 
y_hat

# Multiple R-squared 
rsq <- cor(Y, y_hat)^2 
rsq 


# Bootstrapping
Nsim = 500
betas <- replicate(Nsim, {
  sample <- sample(1:nrow(genes), replace = T)
  boot <- genes[sample,]
  Y <- boot %>%  
    select(Malignant) %>%  
    scale(center = TRUE, scale = FALSE) %>%  
    as.matrix() 
  X <- boot %>%  
    select(-Malignant) %>%  
    as.matrix() 
  model <- glmnet(X, Y, alpha = 0.6392, lambda = 0.0269)
  coef <- coef(model)[,1]
  coef
})


ints <- data.frame(lwr = apply(betas, 1, quantile, c(0.025)), 
                      upr = apply(betas, 1, quantile, c(0.975)))

ints %>% filter(lwr != 0 && upr != 0)

# R squared 1 - sum(y - yhat)^2/sum(y-ybar)^2

