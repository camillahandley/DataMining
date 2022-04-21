## In-class Analysis - Global Temperatures ## 

library(ggplot2)
library(nlme)
library(splines)
library(dplyr)

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

# Read in the data 
temps <- read.table("https://mheaton.byu.edu/docs/files/Stat536/InClassCaseStudies/4%20-%20CorrelatedData/ClimateChange/Data/AnnAvgGlobalClimate.txt", header = T)

ggplot(data = temps, mapping = aes(x = Time, y = AnnAnom)) + 
  geom_line() + geom_smooth(se = FALSE)

cor(temps$AnnAnom, temps$Time)


# Try to see how many knots to include 
knots = 10
rmse <- rep(NA, times = 10)
for(j in 1:knots){
  test.set <- temps[1964:nrow(temps),]
  train.set <- temps[1:1963,]
  fit <- lm(AnnAnom ~ ns(Time, df = j), data = train.set)
  pred <- predict(fit, newdata = test.set)
  rmse[j] <- sqrt(mean((test.set$AnnAnom - as.numeric(pred))^2))
}

plot(rmse)

# 5 knots is best 


# Auto-regressive (1)
temps$TimeStep <- 1:nrow(temps)
ar1.gls <- gls(AnnAnom ~ ns(Time, df = 5), correlation = corAR1(form = ~TimeStep), data = temps, method = "ML")
fit <- ar1.gls$fitted
std.res <- stdres.gls(ar1.gls)


# Check the standardized residuals
hist(std.res)

# Fitted Values vs. residuals 
ggplot(mapping = aes(x = fit, y = std.res)) + 
  geom_point() 
  

# What is the current annual increase in global temperatures?

ggplot(data = temps, mapping = aes(x = Time, y = AnnAnom)) + 
  geom_line() + 
  geom_line(aes(x = Time, y = fit, col = "red"))

# take the difference between June 2018 and June 2019 
temps[2029,3] - temps[2017,3]
# Use predictions to calculate difference between 2019 and 2020

# What do the next 30 years look like?? 

# Make a new data set for prediction 
future <- data.frame(Year = c(rep(2019, time = 6), rep(2020:2050, each = 12)),
                     Month = c(7:12, rep(1:12, times = 31)))
future$Time <- future$Year + (future$Month - 1)/12
future$TimeStep <- 2030:(2030+nrow(future)-1)

preds <- predictgls(ar1.gls, newdframe = future)

ggplot(data = temps, aes(x = Time, y = AnnAnom)) + 
  geom_line() + 
  geom_line(data = preds, aes(x = Time, y = Prediction), col = "red") +
  geom_line(data = preds, aes(x = Time, y = lwr), col = "royalblue") + 
  geom_line(data = preds, aes(x = Time, y = upr), col = "royalblue") 
 






