### Camilla Handley HW

# How the panels are degrading over time
# How many years till the panels have lost 50% of their 
# power generating capability (on average)
# Obtain projections of power for the next year

predictgls <- function(glsobj, newdframe=NULL, level=0.95){
  
  ## If no new dataframe provided, used the dataframe from glsobj
  ## and create a joint dataframe
  if(is.null(newdframe)){
    newdframe <- eval(glsobj$call$data)
  }
  n <- nrow(eval(glsobj$call$data))
  
  ## Create a joint data frame
  jdframe <- rbind(eval(glsobj$call$data)[names(newdframe)],newdframe)
  
  ## Get point predictions of new data frame
  ## Need to break apart formula, remove response then rebuild formula
  the.form <- as.formula(glsobj$call$model)
  the.terms <- terms(the.form,data=newdframe)
  the.terms <- delete.response(the.terms)
  the.form <- as.formula(paste0(the.terms, collapse=""))
  the.fx <- attr(the.terms, "term.labels")
  the.vars <- sapply(1:length(the.fx), function(trm){
    vn <- all.vars(as.formula(paste0("~",the.fx[trm])))
    if("pi"%in%vn){
      vn <- vn[vn!="pi"]
    }
    return(vn)
  })
  if(length(the.fx)>0){
    Xpred <- lapply(1:length(the.vars), function(x){
      if(substr(the.fx[x], 1, 2)%in%c("ns", "bs", "poly")){
        Xbase <- with(eval(glsobj$call$data), eval(parse(text=the.fx[x])))
        xp <- predict(Xbase, newx=newdframe[[the.vars[x]]])
      } else {
        tf <- as.formula(paste0("~", the.fx[x]))
        xp <- matrix(c(model.matrix(tf, data=newdframe)[,-1]), nrow=nrow(newdframe))
      }
      return(xp)
    })
    Xpred <- cbind(1, do.call("cbind", Xpred))
  } else {
    Xpred <- matrix(1, ncol=1, nrow=nrow(newdframe))
  }
  predVals <- c(Xpred%*%glsobj$coefficients)
  var.from.bhat <- diag(Xpred%*%vcov(glsobj)%*%t(Xpred)/(sigma(glsobj)^2))
  
  ## If original model has a variance structure then construct the diagonal
  ## matrices accordingly
  if("varStruct"%in%names(glsobj$modelStruct)){
    
    ## Get Parameters of Variance structure
    var.pars <- coef(glsobj$modelStruct$varStruct,unconstrained=FALSE)
    
    ## If fixed variance structure then there will be no parameters
    if(length(var.pars)==0){
      ## Initialize var matrix using joint data frame
      var.call <- deparse(glsobj$call$weights)
      
    } else {
      ## Initialize var matrix using joint data frame
      var.call <- deparse(glsobj$call$weights)
      var.call <- paste(substr(var.call,1,nchar(var.call)-1),", value = c(",
                        paste(paste(names(var.pars),as.character(var.pars),sep="="),
                              collapse=","),"))")
    }
    
    #Initialize weights
    varMat.i <- Initialize(eval(parse(text=var.call)),data=jdframe)
    
    ## Create the SD weights
    W <- varWeights(varMat.i)
    W1 <- W[1:n]
    W2 <- W[-(1:n)]
    
  } else {
    
    ## No variance model structure then weights are all 1
    W1 <- rep(1,n)
    W2 <- rep(1,nrow(jdframe)-n)
    
  } ## End if("varStruct"%in%names(glsobj$modelStruct))
  
  ## If original model has correlation structure then construct joint
  ## correlation matrix and calculate conditional correlation matrix
  if("corStruct"%in%names(glsobj$modelStruct)){
    ## Get Parameters and call for correlation structure
    cor.pars <- coef(glsobj$modelStruct$corStruct,unconstrained=FALSE)
    cor.call <- deparse(glsobj$call$correlation)
    cor.call <- paste(substr(cor.call,1,nchar(cor.call)-1),", value = c(",
                      paste(as.character(cor.pars),collapse=","),"),fixed=TRUE)")
    cor.covar <- model.matrix(attr(eval(parse(text=cor.call)), "formula"), data=jdframe)
    if(as.character(glsobj$call$correlation)[1]=='corSymm'){
      warning(paste("The general correlation structure corSymm() cannot be used",
                    "for prediction.  Returning prediction without correlation."))
      allpreds <- predVals
      allpreds.se <- (1/W2)*rep(1,nrow(newdframe))
    } else if(any(duplicated(cor.covar))){
      warning(paste("Some prediction locations are the same as observed locations",
                    "resulting in a singular correlation matrix.",
                    "Returning prediction without correlation."))
      allpreds <- predVals
      allpreds.se <- (1/W2)*rep(1,nrow(newdframe))
    } else {
      ## Initialize joint correlation matrix using joint data frame
      corMat.i <- Initialize(eval(parse(text=cor.call)),data=jdframe)
      
      ## If there are no groups (all data belong to same group) then create a joint
      ## correlation matrix.  If there are groups (each group independent) then loop
      ## through the groups calculating correlation each time.
      if(is.null(glsobj$groups) | length(unique(glsobj$group))==1){
        corMats <- corMatrix(corMat.i, corr=TRUE)
        nr <- nrow(corMats)
        predMat <- corMats[(n+1):nr,1:n]%*%chol2inv(chol(corMats[(1:n),(1:n)]))
        allpreds <- predVals+(1/W2)*(predMat%*%(glsobj$residuals*W1))
        allpreds.se <- (1/W2)*sqrt((1-rowSums(predMat*corMats[(n+1):nr,1:n])))
      } else {
        pred.grps <- sort(getGroups(newdframe,form=as.formula(glsobj$call$correlation$form)))
        ugrps <- as.character(unique(pred.grps))
        norig <- table(glsobj$groups)
        corMats <- corMatrix(corMat.i)[ugrps]
        getpred <- function(x){
          nr <- nrow(corMats[[x]])
          predMat <- corMats[[x]][(norig[x]+1):nr,1:norig[x]]%*%
            chol2inv(chol(corMats[[x]][(1:norig[x]),(1:norig[x])]))
          thepred <- predVals[pred.grps==ugrps[x]]+(1/W2[pred.grps==ugrps[x]])*(predMat%*%(glsobj$residuals[glsobj$groups==ugrps[x]]*W1[glsobj$groups==ugrps[x]]))
          thepred.se <- sqrt((1-rowSums(predMat*corMats[[x]][(norig[x]+1):nr,1:norig[x]])))*(1/W2[pred.grps==ugrps[x]])
          return(list(thepred=thepred,thepred.se=thepred.se))
        }
        tmp <- lapply(1:length(ugrps),getpred)
        allpreds <- rep(0,nrow(newdframe))
        allpreds.se <- rep(0,nrow(newdframe))
        for(gr in 1:length(ugrps)){
          allpreds[pred.grps==ugrps[gr]] <- tmp[[gr]]$thepred
          allpreds.se[pred.grps==ugrps[gr]] <- tmp[[gr]]$thepred.se
        }
      }
    } # End else
  } else {
    allpreds <- predVals
    allpreds.se <- (1/W2)*rep(1,nrow(newdframe))
  }#End if "corStruct"%in%names(glsobj$modelStruct) statement
  
  ## Calculate upper and lower interval limits
  allpreds.se <- glsobj$sigma*sqrt(allpreds.se^2 + var.from.bhat)
  if(level>1){
    level <- level/100
  }
  alpha <- 1-level
  P <- length(coef(glsobj))
  low <- allpreds - qt(1-alpha/2, df=n-P)*allpreds.se
  up <- allpreds + qt(1-alpha/2, df=n-P)*allpreds.se
  
  ## Return the predictions and predictive SE
  return(cbind(newdframe,data.frame(Prediction=allpreds,
                                    SE.pred=allpreds.se,
                                    lwr=low,
                                    upr=up)))
}

stdres.gls <- function(glsobj){
  
  ## If original model has a variance structure then construct the diagonal
  ## matrices accordingly
  if("varStruct"%in%names(glsobj$modelStruct)){
    
    Dinv <- varWeights(glsobj$modelStruct$varStruct)*(1/sigma(glsobj))
    
  } else {
    
    ## No variance model structure then weights are all 1
    norig <- nrow(eval(glsobj$call$data))
    Dinv <- rep(1,norig)*(1/sigma(glsobj))
    
  } ## End if("varStruct"%in%names(glsobj$modelStruct))
  
  if("corStruct"%in%names(glsobj$modelStruct)){
    
    # cor.pars <- coef(glsobj$modelStruct$corStruct,unconstrained=FALSE)
    # cor.call <- deparse(glsobj$call$correlation)
    # cor.call <- paste(substr(cor.call,1,nchar(cor.call)-1),", value = c(",
    #                   paste(as.character(cor.pars),collapse=","),"),fixed=TRUE)")
    # Linv <- corMatrix(Initialize(eval(parse(text=cor.call)),
    #                              data=eval(glsobj$call$data)), 
    #                   corr=FALSE)
    Linv <- corMatrix(glsobj$modelStruct$corStruct, corr=FALSE)
    if(is.null(glsobj$groups) | length(unique(glsobj$group))==1){
      #decorr.resid <- as.numeric(solve(t(chol(R)))%*%glsobj$residuals)*Dinv
      decorr.resid <- as.numeric(Linv%*%glsobj$residuals)*Dinv
    } else {
      
      resid.list <- base::split(residuals(glsobj), getGroups(glsobj))
      decorr.resid <- sapply(1:length(resid.list), function(gind){
        #as.numeric(solve(t(chol(R[[gind]])))%*%resid.list[[gind]])
        as.numeric(Linv[[gind]]%*%resid.list[[gind]])
      })
      decorr.resid <- c(decorr.resid)*Dinv
      
    } ## End groups if statement
    
  } else {
    
    decorr.resid <- Dinv*glsobj$residuals
    
  } ## End if("corStruct"%in%names(glsobj$modelStruct))
  
  return(decorr.resid)
  
} #End stdres.gls function

library(ggplot2)
library(GGally)
library(splines)
library(nlme)
library(forecast)


solar <- read.csv("/Users/camillahandley/Desktop/Stat 536/KWH.csv")

# Parse out the Date data
solar$day <- as.numeric(substr(solar$Date,9,10))
solar$month <- as.numeric(substr(solar$Date,6,7))
solar$year <- as.numeric(substr(solar$Date,1,4))

solar$dayyear <- rep(1:365, times = 3)
solar$time <- solar$year + (solar$dayyear - 1)/365 
solar$timestep <- 1:nrow(solar)


ggplot(data = solar, mapping = aes(x = time, y = kwh)) + 
  geom_line() + geom_smooth(se = FALSE) + 
  xlab("Time") + ylab("KiloWatt Hours (kWh)") +
  ggtitle("KiloWatt Hours Over Time")


test.set <- solar[1035:nrow(solar),]
train.set <- solar[1:1034,]

# Try to see how many knots to include 
knots = 10
rmse <- rep(NA, times = 10)
for(j in 1:knots){
  fit <- lm(kwh ~ ns(time, df = j), data = train.set)
  pred <- predict(fit, newdata = test.set)
  rmse[j] <- sqrt(mean((test.set$kwh - as.numeric(pred))^2))
}

plot(rmse)
min(rmse)
# 7 splines?

# Try smooth.splines or loess smoothing?? 
fit2 <- smooth.spline(solar$kwh, solar$time)
fit3 <- loess(kwh~time, data = solar)

# What about polynomial? 
poly <- 0:10
rmse <- rep(NA, times = 11)
for(j in 1:11){
  fit <- lm(kwh ~ poly(time, j), data = train.set)
  pred <- predict(fit, newdata = test.set)
  rmse[j] <- sqrt(mean((test.set$kwh - as.numeric(pred))^2))
}
plot(poly, rmse)
min(rmse)

# Natural splines does better 

# Now look at the correlation 

# Exponential errors
exp.gls <- gls(kwh~ns(time, df = 7),correlation=corExp(form=~time),data=solar,method="ML")

# Moving Average 
ma.gls <- gls(kwh ~ ns(time, df = 7), correlation = corARMA(form = ~timestep, p=0, q=1), data = solar, method = "ML")

# Auto-regressive (1)
ar1.gls <- gls(kwh ~ ns(time, df = 7), correlation = corAR1(form = ~timestep), data = solar, method = "ML")

  
AIC(ar1.gls)
AIC(exp.gls)
AIC(ma.gls)
# looks like AR1 and exponential do the same
fit <- exp.gls$fitted
fit <- ar1.gls$fitted
std.res <- stdres.gls(ar1.gls)

# Check the standardized residuals
hist(std.res)

# Fitted Values vs. residuals 
ggplot(mapping = aes(x = fit, y = std.res)) + 
  geom_point() 

ggplot(data = solar, mapping = aes(x = time, y = kwh)) + 
  geom_line() + 
  geom_line(aes(x = time, y = fit), col = "red")


# What does the next year look like?? 

# Make a new data set for prediction 
future <- data.frame(timestep = 1096:(1095+365), dayyear = 1:365,
                     year = rep(2020, times = 365))
future$time <- future$year + (future$dayyear - 1)/365 

preds <- predictgls(ar1.gls, newdframe = future)


ggplot(data = solar, aes(x = time, y = kwh)) + 
  geom_line() + 
  geom_line(data = preds, aes(x = time, y = Prediction), col = "red") +
  geom_line(data = preds, aes(x = time, y = lwr), col = "royalblue") + 
  geom_line(data = preds, aes(x = time, y = upr), col = "royalblue") 




# Try a SARIMA model
my.ts <- ts(data = solar$kwh, start = c(2017,1), frequency = 365)
# X <- model.matrix(kwh~ -1 + time, data = solar)

acf(my.ts, lag.max = 365, main = "Series KWH")

components.ts = decompose(my.ts)
plot(components.ts)

auto.arima(my.ts, start.p = 0, start.P = 0, start.q = 0, start.Q = 0,
           max.p=2, max.q=2, max.P=2, max.Q=2, max.d=1, max.D=1, ic="aic", stepwise=F)


sarima <- Arima(my.ts, order = c(1,1,1), seasonal = c(0,1,0))
sarima2 <- Arima(my.ts, order = c(1,0,0), seasonal = c(0,1,0))
sarima3 <- Arima(my.ts, order = c(1,1,0), seasonal = c(0,1,0))
sarima4 <- Arima(my.ts, order = c(1,0,1), seasonal = c(0,1,0))

AIC(sarima)
AIC(sarima2)
AIC(sarima3)
AIC(sarima4)

checkresiduals(sarima)

preds <- forecast(sarima, h=(365*2), level = 0.95) 
preds %>% autoplot


# How is the power degrading over time? (#1)
lm.test <- lm(kwh~time, data = solar)
confint(lm.test)


# Find when power is at 50% of orginial capacity (#2)
# Make a new data set for prediction
future <- data.frame(dayyear = rep(1:365, times = 8),
                     year = rep(2020:2027, each = 365))
future$time <- future$year + (future$dayyear - 1)/365 

preds <- forecast(sarima, h=(365*8), level = 0.95)

future$est <- preds$mean

lm.est <- predict.lm(lm.test, newdata = future, interval = "confidence")
future$lwr <- lm.est[,2]
future$upr <- lm.est[,3]
future$lm.est <- lm.est[,1]



ggplot(data = solar, mapping = aes(x = time, y = kwh)) + 
  geom_line() +
  geom_smooth(method = "lm", col = "red", alpha = .9) +
  geom_line(aes(x = time, y = lm.test$fitted.values), col = "red") +
  geom_line(data = future, aes(x = time, y = est), col = "blue") +
  geom_line(data = future, aes(x = time, y = lwr), col = "red", lty = "dashed") +
  geom_line(data = future, aes(x = time, y = upr), col = "red", lty = "dashed") +
  geom_line(data = future, aes(x = time, y = lm.est), col = "red") +
  geom_hline(yintercept = 16.63, lty = "dashed") +
  ggtitle("Future Power Projections") +
  ylab("Kilowatt Hours") + 
  xlab("Time") +
  scale_x_continuous(breaks=c(2017:2028))

# This is 50% capacity 
lm.test$fitted.values[1] / 2
data2017 <- solar %>% filter(year == 2017) %>% select(kwh) 
mean(data2017$kwh) / 2

# Find predictions for next year (#3)
year <- forecast(sarima, h=(365), level = 0.95) 
year %>% autoplot


# Check assumptions 

hist(residuals(sarima), breaks = 15,
     main = "Histogram of Residuals", xlab = "Residuals (SARIMA)")

ggplot(mapping = aes(x = fitted(sarima), y = residuals(sarima))) + 
  geom_point() +
  ggtitle("Fitted Values vs. Residuals") +
  xlab("Fitted Values") + ylab("Residuals")

acf(residuals(sarima), main = "Series SARIMA Model Residuals")

lm <- lm(kwh ~ ns(time, knots = 7), data = solar)
acf(lm$residuals, main = "Series Linear Model Resiudals")

ggplot(data = solar, mapping = aes(x = time, y = kwh)) + 
  geom_line() + 
  geom_line(aes(x = time, y = fitted(sarima)), col = "red", lty = "dashed")

# Cross Validation for Predictions 

test.set <- solar[912:nrow(solar),]
train.set <- solar[1:911,]

ts.cv <- ts(data = train.set$kwh, start = c(2017,1), frequency = 365)
sarima.cv <- Arima(ts.cv, order = c(1,1,1), seasonal = c(0,1,0))
preds.cv <- forecast(sarima.cv, h=(184), level = 0.95)


rmse <- sqrt(mean((test.set$kwh - preds.cv$mean)^2))
sd(solar$kwh)
max(solar$kwh) - min(solar$kwh)
cover <- mean(preds.cv$upper > test.set$kwh & preds.cv$lower < test.set$kwh)
bias <- mean(preds.cv$mean - test.set$kwh)

# Psuedo R-squared
cor(fitted(sarima), solar$kwh)^2




