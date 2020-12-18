############################################################
##                                                        ##      
##                     est_ts_model                       ##
##                                                        ##
##                D.R.Moxley | 20200511                   ##
##                                                        ##
##    Generic function to estimate a time series model    ##
##                                                        ##
##    Take: dataframe of time series, model specification ##
##  , and loss function requiring s_t,s_t1, and s_hat_t1  ##
##    Return: estimate & predict model, white noise test  ##
##      and loss function                                 ##
##                                                        ##
############################################################

est_ts_model <- function(df,time_series_name,model_specification,loss_function){
  
  #timeSeriesName <- "priceSpread_Tran"
  #model <- c(0,0,0)
  ########
  # load required apckages
  if(!require(normwhn.test)){install.packages("normwhn.test")}; library(normwhn.test) #req. for whitenoise.test
  if(!require(forecast)){install.packages("forecast")}; library(forecast)
  
  # Estimate model  
  len_series <- length(df[[timeSeriesName]])
  len_train <- round(len_series*.8,0)
  df_train <- df[[timeSeriesName]][1:len_train]
  len_test <- len_series-len_train
  df_test <- df[[timeSeriesName]][(len_train+1):len_series]
  
  m_fit <- arima(df_train,order=model)
  
  # get predictions
  s_hat_t <- as.vector(forecast(m_fit,h=len_test)$mean)
  
  # get residuals
  s_t <- df_test
  e_t <- s_t - s_hat_t
  
  
  #plot(m_residuals)
  
  # check for White noise of residuals
  wn <- whitenoise.test(e_t)
  
  # compute loss function
  lf <- c()
  for(i in 1:(length(s_hat_t)-1)){
    lf <- append(lf,loss_function(s_t[i],s_t[i+1],s_hat_t[1+i]))
  }
  lf <- mean(na.omit(lf))
  
  
  results <- c(time_series_name, paste0(model[1],",",model[2],",",model[3]), wn, lf)
  return(results)
}