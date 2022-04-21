## In-Class Car Crash Analysis ##


library(ggplot2)
library(dplyr)
library(GGally)
library(bestglm)

cars <- read.table("https://mheaton.byu.edu/docs/files/Stat536/InClassCaseStudies/5%20-%20LogisticRegression/CarCrashes/Data/Crash.txt", header = T)[,-1]

cars$seatbelt <- ifelse(cars$REST_USE == 3 | cars$REST_USE == 2 | cars$REST_USE == 8 |
                          cars$REST_USE == 1, "Yes", "No")
cars$helmet <- ifelse(cars$REST_USE == 5, "Helmet", "None")
cars$REST_USE <- cars$seatbelt
cars <- cars[,-14]
cars$intersect <- ifelse(cars$TYP_INT == 1, "None", "Intersection")
cars <- cars[,-5]
cars$VALIGN <- ifelse(cars$VALIGN == 1, "Straight", "Curve")
cars$LGT_COND <-  as.character(cars$LGT_COND)
cars$WEATHER <- as.character(cars$WEATHER)
cars$AIR_BAG <- as.character(cars$AIR_BAG)
cars$VTRAFWAY <- as.character(cars$VTRAFWAY)
cars$VSURCOND <- as.character(cars$VSURCOND)
for (i in 1:nrow(cars)) {
  if (cars$LGT_COND[i] == "1") cars$LGT_COND[i] = "day"
  if (cars$LGT_COND[i] == "2") cars$LGT_COND[i] = "night-nolight"
  if (cars$LGT_COND[i] == "3") cars$LGT_COND[i] = "night-lighted"
  if (cars$LGT_COND[i] == "4") cars$LGT_COND[i] = "dawn"
  if (cars$LGT_COND[i] == "%") cars$LGT_COND[i] = "dusk"
  if (cars$WEATHER[i] == "1" | cars$WEATHER[i] == "10") cars$WEATHER[i] = "clear"
  if (cars$WEATHER[i] == "2" | cars$WEATHER[i] == "12") cars$WEATHER[i] = "rain"
  if (cars$WEATHER[i] == "3" | cars$WEATHER[i] == "4" | cars$WEATHER[i] == "11") cars$WEATHER[i] = "snow-hail"
  if (cars$WEATHER[i] == "5" | cars$WEATHER[i] == "6" | cars$WEATHER[i] == "7") cars$WEATHER[i] = "fog-wind-sand"
  if (cars$ALCOHOL[i] == 2) cars$ALCOHOL[i] = 0
  if (cars$AIR_BAG[i] == "0" | cars$AIR_BAG[i] == "20") cars$AIR_BAG[i] ="not deployed"
  else cars$AIR_BAG[i] = "deployed"
  if (cars$VTRAFWAY[i] == "4") cars$VTRAFWAY[i] = "oneway"
  else if (cars$VTRAFWAY[i] == "6") cars$VTRAFWAY[i] = "ramp"
  else cars$VTRAFWAY[i] = "twoway"
  if (cars$VSURCOND[i] == "1") cars$VSURCOND[i] = "dry"
  if (cars$VSURCOND[i] == "2" | cars$VSURCOND[i] == "6") cars$VSURCOND[i] = "wet"
  if (cars$VSURCOND[i] == "3" | cars$VSURCOND[i] == "4" | cars$VSURCOND[i] == "10") cars$VSURCOND[i] = "snow-ice"
  if (cars$VSURCOND[i] == "5" | cars$VSURCOND[i] == "8" | cars$VSURCOND[i] == "11") cars$VSURCOND[i] = "other"
}

cars <- cars[,c(1:6,8:14,7)]

# crash$seatbelt <- ifelse(crash$REST_USE == 3 | crash$REST_USE == 2 | crash$REST_USE == 8 | 
#                            crash$REST_USE == 1, "Yes", "No")
# crash$seatbelt <- ifelse(crash$REST_USE == 5, "Helmet", crash$seatbelt)
# crash$ALCOHOL <- ifelse(crash$ALCOHOL == 2, 0, crash$ALCOHOL)
#crash$intersect <- ifelse(crash$TYP_INT == 1, "None", "Intersection")
# crash$VALIGN <- ifelse(crash$VALIGN == 1, "Straight", "Curve")
# crash <- crash %>% select(-c(REST_USE, TYP_INT))

cars$LGT_COND <- factor(cars$LGT_COND)
cars$WEATHER <- factor(cars$WEATHER)
cars$AIR_BAG <- factor(cars$AIR_BAG)
cars$VTRAFWAY <- factor(cars$VTRAFWAY)
cars$VSURCOND <- factor(cars$VSURCOND)
cars$VALIGN <- factor(cars$VALIGN)
cars$REST_USE <- factor(cars$REST_USE)
cars$intersect <- factor(cars$intersect)
cars$helmet <- factor(cars$helmet)

# Explore the data 

ggplot(data = crash, mapping = aes(x = HOUR, y = SEVERITY)) + 
  geom_point() + geom_smooth(se = F)

ggplot(data = crash, mapping = aes(x = VSPD_LIM, y = SEVERITY)) + 
  geom_point() + geom_smooth(se = F)

ggplot(data = crash, mapping = aes(x = VNUM_LAN, y = SEVERITY)) + 
  geom_point() + geom_smooth(se = F, method = 'lm')

# Two way table for Alcohol 



bestglm(cars, IC = "BIC", method = "forward", family = binomial)


fit1 <- glm(SEVERITY ~ LGT_COND + ALCOHOL + REST_USE + 
              AIR_BAG + VSPD_LIM + VALIGN + helmet, family = binomial, data = cars)
fit2 <- glm(SEVERITY ~ ALCOHOL + REST_USE + 
              AIR_BAG + VSPD_LIM + VALIGN + helmet, family = binomial, data = cars)
summary(fit1)
summary(fit2)

BIC(fit1)
BIC(fit2)

cor(fit1$fitted.values, cars$SEVERITY)^2
cor(fit2$fitted.values, cars$SEVERITY)^2



