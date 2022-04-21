#### In-Class Study: Irrigated Agriculture ####

# Libraries 
library(ggplot2)

# Read in the data 
agri <- read.csv("https://mheaton.byu.edu/docs/files/Stat536/InClassCaseStudies/3%20-%20Nonlinear/CWSI/Data/CWSI.csv")
agri <- agri[-1]


# Exploratory Data Analysis 
ggplot(agri, mapping = aes(x = CWSI, y = SWC)) + 
  geom_point() +
  geom_smooth(se = F) #+
  geom_smooth(se = F, method = "lm", col = "red") +
  ggtitle("Smooth Curve and Linear Regression Line")

cor(agri$CWSI, agri$SWC)

# Splines 

library(splines)


# Cross validation to choose number of knots
rmseknot <- rep(NA, times = 40)
rmse <- rep(NA, times = 44)
for(j in 1:40){
  for(i in 1:44){
    test.set <- agri[i,]
    train.set <- agri[-i,]
    
    fit <- lm(SWC ~ ns(CWSI, df = j), data = train.set)
    pred <- predict(fit, newdata = test.set)
    
    rmse[i] <- sqrt(mean((test.set$SWC - as.numeric(pred))^2))
  }
  rmseknot[j] <- mean(rmse)
}

plot(rmseknot)



# Try 2 knots 
fit2 <- lm(SWC ~ ns(CWSI, df = 2), data = agri)
pred2 <- predict(fit2)

ggplot(data = agri, mapping = aes(x = CWSI, y = SWC)) + 
  geom_point() + 
  geom_line(mapping = aes(x = CWSI, y = as.numeric(pred2)))

# RMSE for 2 knots
rmseknot[2]

new <- read.table("/Users/camillahandley/Downloads/CWSIHoldoutTruth.txt", header = T)
colnames(new) <- c("SWC", "CWSI")

pred <- predict(fit2, newdata = new)

rmse <- sqrt(mean((new$SWC - as.numeric(pred))^2))


