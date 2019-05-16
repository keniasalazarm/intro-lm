BestModelReg <- function(df, varMax, method, form) {
  # regsubsets() function (part of the leaps library) performs best subregsubsets()
  # set selection by identifying the best model that contains a given number
  # of predictors, where best is quantified using RSS
  # nvmax... option can be used to return as many variables as are desired
  regfit =regsubsets (form, df ,nvmax =varMax, 
                         method = method)
  reg.summary = summary (regfit)
  names(reg.summary)
  # Here best is defined as having the smallest RSS, or equivalently largest R2.
  # As expected, the R2 statistic increases monotonically as more
  # variables are included.
  reg.summary$rsq
  
  # low RSS or a high R2 indicates a model with a low training error,
  # whereas we wish to choose a model that has a low test error.
  
  # RSS, adjusted R2, Cp Akaike information criterion (AIC), 
  # and BIC Bayesian information for all of the models at once will
  # help us decide which model to select.
  par(mfrow =c(2,2))
  plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",
       type="l")
  plot(reg.summary$adjr2 ,xlab =" Number of Variables ",
       ylab=" Adjusted RSq",type="l")
  
  # which.max() function can be used to identify the location of
  # the maximum point of a vector
  maxr2 <- which.max (reg.summary$adjr2)
  points (maxr2, reg.summary$adjr2[maxr2], col ="red",cex =2, pch =20)
  
  # In a similar fashion we can plot the Cp and BIC statistics, and indicate the
  # models with the smallest statistic using which.min().
  plot(reg.summary$cp, xlab =" Number of Variables ", ylab="Cp", type="l")
  mincp <- which.min (reg.summary$cp)
  points(mincp, reg.summary$cp [mincp], col ="red",cex =2, pch =20)
  minbic <- which.min (reg.summary$bic )
  
  plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC", type="l")
  points (minbic, reg.summary$bic[minbic], col =" red",cex =2, pch =20)
  print(paste0("rss- min: ", reg.summary$rss[minbic]))
  print(paste0("adjr2- max: ", reg.summary$adjr2[minbic]))
  # Essentially, the Cp statistic adds a penalty
  # of  to the training RSS in order to adjust for the fact that the training
  # error tends to underestimate the test error.
  #  BIC adds a heavier penalty log(n)
  return(regfit)
}