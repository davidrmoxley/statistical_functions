############################################################
##                                                        ##
##    Diebold-Marino Test for Multiple Comparison         ##
##                                                        ##
##                                                        ##
############################################################

diebold_marino <- function(df,time_series_name,model_1,model_2){
  
  # Returns DM statistic and p-value
  
  #time_series_name <- "priceSpread_Tran"
  # model_1 <- c(0,0,0)
  # model_2 <- c(1,0,0)
  
  # Estimate models
  # Estimate model  
  len_series <- length(df[[time_series_name]])
  len_train <- round(len_series*.8,0)
  df_train <- df[[time_series_name]][1:len_train]
  len_test <- len_series-len_train
  df_test <- df[[time_series_name]][(len_train+1):len_series]
  
  m_fit_1 <- arima(df_train,order=model_1)
  m_fit_2 <- arima(df_train,order=model_2)
  
  # predictions and residuals
  ## Model 1
  s1_hat_t <- as.vector(forecast(m_fit_1,h=len_test)$mean)
  s_t <- df_test
  e1_t <- s_t - s1_hat_t
  ## Model 2
  s2_hat_t <- as.vector(forecast(m_fit_2,h=len_test)$mean)
  e2_t <- s_t - s2_hat_t
  
  # set s_t1, s_t, and s_hat for model 1 and model2
  s_t1 <- s_t[2:length(s_t)]
  s_t <- s_t[1:(length(s_t)-1)]
  s1_hat_t1 <- s1_hat_t[2:length(s1_hat_t)]
  s2_hat_t1 <- s2_hat_t[2:length(s2_hat_t)]
  
  # compute loss function
  lf_1 <- c()
  #s_t,s_t1, s_hat_t1
  for(i in 1:length(s1_hat_t1)){
    lf_1 <- append(lf_1,lossFunction(s_t[i],s_t1[i],s1_hat_t1[i]))
  }
  lf_2 <- c()
  for(i in 1:length(s2_hat_t1)){
    lf_2 <- append(lf_2,lossFunction(s_t[i],s_t1[i],s2_hat_t1[i]))
  }
  
  # get Loss differential
  d_t <-lf_1-lf_2
  
  # compute Diebold-Mariano 
  dm <- mean(d_t)/sqrt(2*pi*sum(autocovariances(d_t))/(length(d_t)-1))
  dm <- if(is.nan(dm)){0}
  
  # compute pvalue
  p_value <- pnorm(q=dm, mean=0, sd=1,lower.tail=TRUE)
  
  dm_results <- data.frame(DM_Stat=dm,pValue=p_value)
  return(dm_results)
}
