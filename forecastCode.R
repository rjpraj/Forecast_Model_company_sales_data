
dat1 = read.csv("input15001510.csv", header = TRUE)   #takimg the input file
i = 1
L = length(dat1)    #calculating total no. of columns in data
z = grep("PPC", colnames(dat1))   # storing index after which the past sales data start
D = grep("Description", colnames(dat1))
N = length(dat1$PPC)    # storing total number of rows i.e total number data
N
z
D
M=0
COUNT = 0 
data = data.frame()

for(k in 1:N) {     #separating columns with data to columns with their resp. source information
  for(i in 1:(L-z)){ 
    data[k,i] = dat1[k,(i+z)]
  }  
}
#data
df = dat1[1:N, 1:D] # dataframe with the source information of each data

#Overall Forecasting loop 

for (k in 1:N) {  #passing the number of data for which forecast is to be produced
  e = data[k,]   #storing each data one ny one in e and performing the forecasting operation 
  e = t(e)
  e[is.na(e)] <- 0
  
  i = 1
  
  #removing the starting zeros
  if(e[1]== 0){
    while (e[i]==0) {
      i = i+1
    }
  }
  i
  e= e[i:length(e)]
  
  #counting the number of zeros in the dataset after intial zeros removal
  j = 0
  for(i in 1:length(e)){
    if(e[i]==0)
      j= j+1
  }
  j
  
  Y <- ts(e, frequency=12) #assinging the frequency to the data
  n = length(e) 
  
  res = 0
  if(n>24){
    f =e
    #actual dataset for different lag
    A7 = f[(n-6):n] #storing the actual values with lag upto 6
    A6 = f[(n-5):n]
    A5 = f[(n-4):n]
    A4 = f[(n-3):n]
    A3 = f[(n-2):n]
    A2 = f[(n-1):n]
    A1 = f[n:n]
    
    
    #lag till 6
    L7 = f[1:(n-7)] #creating a new dataset with 7 months less data from the original one
    Y7 <- ts(L7, frequency=12)
    fit <- tbats(Y7)
    seasonaL7 <- !is.null(fit$seasonal) # checking if the data is seasonal or not
    #till lag 5
    L6 = f[1:(n-6)] 
    Y6 <- ts(L6, frequency=12)
    fit <- tbats(Y6)
    seasonaL6 <- !is.null(fit$seasonal)
    #till lag 4
    L5 = f[1:(n-5)]
    Y5 <- ts(L5, frequency=12)
    fit <- tbats(Y5)
    seasonaL5 <- !is.null(fit$seasonal)
    #till lag 3
    L4 = f[1:(n-4)]
    Y4 <- ts(L4, frequency=12)
    fit<- tbats(Y4)
    seasonaL4 <- !is.null(fit$seasonal)
    #till lag 2
    L3 = f[1:(n-3)]
    Y3 <- ts(L3, frequency=12)
    fit <- tbats(Y3)
    seasonaL3 <- !is.null(fit$seasonal)
    #till lag 1
    L2 = f[1:(n-2)]
    Y2 <- ts(L2, frequency=12)
    fit<- tbats(Y2)
    seasonaL2 <- !is.null(fit$seasonal)
    #till lag 0
    L1 = f[1:(n-1)] 
    Y1 <- ts(L1, frequency=12)
    fit<- tbats(Y1)
    seasonaL1 <- !is.null(fit$seasonal)
    
    
    #vales for comparing from exponential function
    #FOR CHOOSING BTW ADDITIVE AND MULTIPLICATIVE SEASONALITY  
    ADD = 0 # variable to store if additive seasonality model is selected for each of L7,L6....
    MUL = 0 # variable to store if multiplicative seasonality model is selected for each of L7,L6....
    
    diff7 = 0  # variables to store the difference in forecasted and actual value
    diff6 = 0
    diff5 = 0
    diff4 = 0
    diff3 = 0
    diff2 = 0
    diff1 = 0
    
    c7 = 0
    c6 = 0
    c5 = 0
    c4 = 0
    c3 = 0
    c2 = 0
    c1 = 0
    
    #exponential function
    ###Calculating the sum of error terms lag till 7
    d7 = L7
    n7 = length(d7)
    #taking last six values for error calculation
    
    #actual dataset for different lag
    a3 = d7[(n7-2):n7]
    a2 = d7[(n7-1):n7]
    a1 = d7[n7:n7]
    
    #till lag 2
    l3 = d7[1:(n7-3)]
    y3 <- ts(l3, frequency=12)
    fit <- tbats(y3)
    seasonal3 <- !is.null(fit$seasonal)
    #seasonal3
    #till lag 1
    l2 = d7[1:(n7-2)]
    y2 <- ts(l2, frequency=12)
    fit<- tbats(y2)
    seasonal2 <- !is.null(fit$seasonal)
    #seasonal2
    #till lag 0
    l1 = d7[1:(n7-1)] 
    y1 <- ts(l1, frequency=12)
    fit<- tbats(y1)
    seasonal1 <- !is.null(fit$seasonal)
    #seasonal1
    
    #function for providing the solution of both additive and multiplicative model if seasonal
    
    diff31 = 0
    diff21 = 0
    diff11 = 0
    
    diff32 = 0
    diff22 = 0
    diff12 = 0
    
    ch = 0
    
    #choosing the exponential smoothing model for lag upto 3 
    if(seasonal3 == 'TRUE' && seasonaL7 == 'TRUE') {  # if the data is seasonal forecasting with an additive seasonality model 
      fit1<-ets(y3, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else { # if the data is not seasonal than forecasting with no seasonal model
      fit1<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y3) #checking if ZZZ model is using a multiplicative seasonality model 
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y3, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,    # if the data is seasonal forecasting with an additive seasonality model
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{ # if the data is not seasonal than forecasting with no seasonal model
      fit2<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1) 
    c3 = (AA11a$mean) #storing the forecast of either additive or no seasonality model 
    c3 = c3[1:3]
    for (i in 1:length(c3))
      diff31[i] = c3[i]-a3[i] # taking the difference of actual and forecasted value
    sum31 = sum(abs(diff31)) # storing the sum of all the three differences
    #print(sum31)
    c3 = 0
    sum32 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c3 = (AA11a$mean) #storing the forecast of either multiplicative or no seasonality model 
      c3 = c3[1:3]
      for (i in 1:length(c3))
        diff32[i] = c3[i]-a3[i] # taking the difference of actual and forecasted value
      sum32 = sum(abs(diff32)) # storing the sum of all the three differences
      #print(sum32)
      ch = 0
    }
    #choosing the exponential smoothing model for lag upto 2
    if(seasonal2 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y2, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y2)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y2, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c2 = (AA11a$mean)
    c2 = c2[1:2]
    for (i in 1:length(c2))
      diff21[i] = c2[i]-a2[i]
    sum21 = sum(abs(diff21))
    #  print(sum21)
    c2 = 0
    sum22 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c2 = (AA11a$mean)
      c2 = c2[1:2]
      for (i in 1:length(c2))
        diff22[i] = c2[i]-a2[i]
      sum22 = sum(abs(diff22))
      # print(sum22)
      ch = 0
    }
    
    #choosing the exponential smoothing model for lag upto 1
    if(seasonal1 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y1, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y1)
    m = AA11a$method
    m
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y1, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c1 = (AA11a$mean)
    c1 = c1[1:1]
    for (i in 1:length(c1))
      diff11[i] = c1[i]-a1[i]
    sum11 = sum(abs(diff11))
    #print(sum11)
    c1 = 0
    sum12 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c1 = (AA11a$mean)
      c1 = c1[1:1]
      for (i in 1:length(c1))
        diff12[i] = c1[i]-a1[i]
      sum12 = sum(abs(diff12))
      #print(sum12) 
    }
    
    #Comparing for which type of model has maximum number types the least sum of diffenece 
    i = 0 
    j = 0 
    
    if(sum31<=sum32| sum32==0){ #if the sum of difference obtained from first type is smaller then the second type model 1 is selected else model 2 
      i = i+1
    }else
      j = j+1
    
    if(sum21<=sum22| sum22==0){
      i = i+1
    }else
      j = j+1
    
    if(sum11<=sum12| sum12==0){
      i = i+1
    }else
      j = j+1
    
    #forecasting the best model of exponential smoothing for lag upto 7 from the original data
    if(seasonaL7 == 'TRUE') { # if the data is seasonal forecasting with an additive seasonality model 
      fit1<-ets(Y7, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {  # if the data is not seasonal than forecasting with no seasonal model
      fit1<-ets(Y7, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(Y7)     #checking if the ZZZ model gives a multipliactive model then only we apply mutliplicative seasonality model to remove the chances of error
    m = AA11a$method 
    if(seasonaL7 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(Y7, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,     # if the data is seasonal forecasting with an multipliactive seasonality model 
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }else{     #if the data is not seasonal than forecasting with no seasonal model
      fit2<-ets(Y7, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    if(i>j){
      AA11a<-forecast(fit1) #if more number of times model type 1 has smaller sum difference forecast is produced foe model type 1
      cf7 = AA11a
      ADD = ADD + 1 #storing which model is selected either additive or no seasonal model
    }else{
      AA11a<-forecast(fit2) #if more number of times model type 2 has smaller sum difference forecast is produced foe model type 2
      cf7 = AA11a
      MUL = MUL + 1 #storing which model is selected either multiplicative or no seasonal model
    }
    # print(cf7$model)
    cf7 = (cf7$mean) #final forecast ffrom either of the model selected
    cf7 = cf7[1:7]
    for (i in 1:length(cf7))
      diff7[i] = cf7[i]-A7[i] #storing the difference of actual data and forecast generated for the lag upto 7 from original data 
    sume7 = sum(abs(diff7)) 
    print(sume7) #storing the sum of differences to compare it with arima model  
    
    
    ###Calculating the sum of error terms lag till 6 from the original data with similar operation as above 
    d6 = L6
    n6 = length(d6)
    #taking last six values for error calculation
    
    #actual dataset for different lag
    a3 = d6[(n6-2):n6]
    a2 = d6[(n6-1):n6]
    a1 = d6[n6:n6]
    
    #till lag 2
    l3 = d6[1:(n6-3)]
    y3 <- ts(l3, frequency=12)
    fit <- tbats(y3)
    seasonal3 <- !is.null(fit$seasonal)
    #seasonal3
    #till lag 1
    l2 = d6[1:(n6-2)]
    y2 <- ts(l2, frequency=12)
    fit<- tbats(y2)
    seasonal2 <- !is.null(fit$seasonal)
    #seasonal2
    #till lag 0
    l1 = d6[1:(n6-1)] 
    y1 <- ts(l1, frequency=12)
    fit<- tbats(y1)
    seasonal1 <- !is.null(fit$seasonal)
    #seasonal1
    
    
    #function for providing the solution of both additive and multiplicative model if seasonal
    
    diff31 = 0
    diff21 = 0
    diff11 = 0
    
    diff32 = 0
    diff22 = 0
    diff12 = 0
    
    ch = 0
    
    #choosing the exponential smoothing model for lag upto 3 
    if(seasonal3 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y3, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y3)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y3, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c3 = (AA11a$mean)
    c3 = c3[1:3]
    for (i in 1:length(c3))
      diff31[i] = c3[i]-a3[i]
    sum31 = sum(abs(diff31))
    #print(sum31)
    c3 = 0
    sum32 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c3 = (AA11a$mean)
      c3 = c3[1:3]
      for (i in 1:length(c3))
        diff32[i] = c3[i]-a3[i]
      sum32 = sum(abs(diff32))
      #print(sum32)
      ch = 0
    }
    #choosing the exponential smoothing model for lag upto 2
    if(seasonal2 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y2, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y2)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y2, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c2 = (AA11a$mean)
    c2 = c2[1:2]
    for (i in 1:length(c2))
      diff21[i] = c2[i]-a2[i]
    sum21 = sum(abs(diff21))
    #  print(sum21)
    c2 = 0
    sum22 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c2 = (AA11a$mean)
      c2 = c2[1:2]
      for (i in 1:length(c2))
        diff22[i] = c2[i]-a2[i]
      sum22 = sum(abs(diff22))
      # print(sum22)
      ch = 0
    }
    
    #choosing the exponential smoothing model for lag upto 1
    if(seasonal1 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y1, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y1)
    m = AA11a$method
    m
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y1, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c1 = (AA11a$mean)
    c1 = c1[1:1]
    for (i in 1:length(c1))
      diff11[i] = c1[i]-a1[i]
    sum11 = sum(abs(diff11))
    #print(sum11)
    c1 = 0
    sum12 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c1 = (AA11a$mean)
      c1 = c1[1:1]
      for (i in 1:length(c1))
        diff12[i] = c1[i]-a1[i]
      sum12 = sum(abs(diff12))
      #print(sum12) 
    }
    
    i = 0 
    j = 0 
    
    if(sum31<=sum32| sum32==0){
      i = i+1
    }else
      j = j+1
    
    if(sum21<=sum22| sum22==0){
      i = i+1
    }else
      j = j+1
    
    if(sum11<=sum12| sum12==0){
      i = i+1
    }else
      j = j+1
    
    #forecasting the best model of exponential smoothing for lag upto 7
    if(seasonaL6 == 'TRUE') {
      fit1<-ets(Y6, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(Y6, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(Y6)
    m = AA11a$method
    if(seasonaL6 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(Y6, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }else{
      fit2<-ets(Y6, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    if(i>j){
      AA11a<-forecast(fit1)
      cf6 = AA11a
      ADD = ADD + 1
    }else{
      AA11a<-forecast(fit2)
      cf6 = AA11a
      MUL = MUL + 1
    }
    cf6 = (cf6$mean)
    cf6 = cf6[1:6]
    for (i in 1:length(cf6))
      diff6[i] = cf6[i]-A6[i]
    sume6 = sum(abs(diff6)) #storing the differences of the actual data from the forecasted values for the lag of 6 from the original data 
    #print(sume6) 
    
    sum72 = 0
    sum62 = 0
    sum52 = 0
    sum42 = 0
    sum32 = 0
    sum22 = 0
    sum12 = 0
    
    ###Calculating the sum of error terms lag till 5 from the original data with similar operation as above 
    d5 = L5
    n5 = length(d5)
    #taking last six values for error calculation
    
    #actual dataset for different lag
    a3 = d5[(n5-2):n5]
    a2 = d5[(n5-1):n5]
    a1 = d5[n5:n5]
    
    #till lag 2
    l3 = d5[1:(n5-3)]
    y3 <- ts(l3, frequency=12)
    fit <- tbats(y3)
    seasonal3 <- !is.null(fit$seasonal)
    #seasonal3
    #till lag 1
    l2 = d5[1:(n5-2)]
    y2 <- ts(l2, frequency=12)
    fit<- tbats(y2)
    seasonal2 <- !is.null(fit$seasonal)
    #seasonal2
    #till lag 0
    l1 = d5[1:(n5-1)] 
    y1 <- ts(l1, frequency=12)
    fit<- tbats(y1)
    seasonal1 <- !is.null(fit$seasonal)
    #seasonal1
    
    #function for providing the solution of both additive and multiplicative model if seasonal
    
    diff31 = 0
    diff21 = 0
    diff11 = 0
    
    # diff52=  0
    # diff42 = 0
    diff32 = 0
    diff22 = 0
    diff12 = 0
    
    ch = 0
    
    #choosing the exponential smoothing model for lag upto 3 
    if(seasonal3 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y3, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y3)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y3, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c3 = (AA11a$mean)
    c3 = c3[1:3]
    for (i in 1:length(c3))
      diff31[i] = c3[i]-a3[i]
    sum31 = sum(abs(diff31))
    #print(sum31)
    c3 = 0
    sum32 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c3 = (AA11a$mean)
      c3 = c3[1:3]
      for (i in 1:length(c3))
        diff32[i] = c3[i]-a3[i]
      sum32 = sum(abs(diff32))
      #print(sum32)
      ch = 0
    }
    #choosing the exponential smoothing model for lag upto 2
    if(seasonal2 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y2, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y2)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y2, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c2 = (AA11a$mean)
    c2 = c2[1:2]
    for (i in 1:length(c2))
      diff21[i] = c2[i]-a2[i]
    sum21 = sum(abs(diff21))
    #  print(sum21)
    c2 = 0
    sum22 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c2 = (AA11a$mean)
      c2 = c2[1:2]
      for (i in 1:length(c2))
        diff22[i] = c2[i]-a2[i]
      sum22 = sum(abs(diff22))
      # print(sum22)
      ch = 0
    }
    
    #choosing the exponential smoothing model for lag upto 1
    if(seasonal1 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y1, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y1)
    m = AA11a$method
    m
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y1, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c1 = (AA11a$mean)
    c1 = c1[1:1]
    for (i in 1:length(c1))
      diff11[i] = c1[i]-a1[i]
    sum11 = sum(abs(diff11))
    #print(sum11)
    c1 = 0
    sum12 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c1 = (AA11a$mean)
      c1 = c1[1:1]
      for (i in 1:length(c1))
        diff12[i] = c1[i]-a1[i]
      sum12 = sum(abs(diff12))
      #print(sum12) 
    }
    
    i = 0 
    j = 0 
    
    if(sum31<=sum32| sum32==0){
      i = i+1
    }else
      j = j+1
    
    if(sum21<=sum22| sum22==0){
      i = i+1
    }else
      j = j+1
    
    if(sum11<=sum12| sum12==0){
      i = i+1
    }else
      j = j+1
    
    #forecasting the best model of exponential smoothing for lag upto 7
    if(seasonaL5 == 'TRUE') {
      fit1<-ets(Y5, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(Y5, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(Y5)
    m = AA11a$method
    if(seasonaL5 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(Y5, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }else{
      fit2<-ets(Y5, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    if(i>j){
      AA11a<-forecast(fit1)
      cf5 = AA11a
      ADD = ADD + 1
    }else{
      AA11a<-forecast(fit2)
      cf5 = AA11a
      MUL = MUL + 1
    }
    cf5 = (cf5$mean)
    cf5 = cf5[1:5]
    for (i in 1:length(cf5))
      diff5[i] = cf5[i]-A5[i]
    sume5 = sum(abs(diff5)) #storing the differences of the actual data from the forecasted values for the lag of 5 from the original data 
    #print(sume5)
    
    
    ###Calculating the sum of error terms lag till 4 from the original data with similar operation as above 
    d4 = L4
    n4 = length(d4)
    #taking last six values for error calculation
    
    #actual dataset for different lag
    a3 = d4[(n4-2):n4]
    a2 = d4[(n4-1):n4]
    a1 = d4[n4:n4]
    
    #till lag 2
    l3 = d4[1:(n4-3)]
    y3 <- ts(l3, frequency=12)
    fit <- tbats(y3)
    seasonal3 <- !is.null(fit$seasonal)
    #seasonal3
    #till lag 1
    l2 = d4[1:(n4-2)]
    y2 <- ts(l2, frequency=12)
    fit<- tbats(y2)
    seasonal2 <- !is.null(fit$seasonal)
    #seasonal2
    #till lag 0
    l1 = d4[1:(n4-1)] 
    y1 <- ts(l1, frequency=12)
    fit<- tbats(y1)
    seasonal1 <- !is.null(fit$seasonal)
    #seasonal1
    
    # function for providing the solution of both additive and multiplicative model if seasonal
    
    diff31 = 0
    diff21 = 0
    diff11 = 0
    
    diff32 = 0
    diff22 = 0
    diff12 = 0
    
    ch = 0
    
    #choosing the exponential smoothing model for lag upto 3 
    if(seasonal3 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y3, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y3)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y3, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c3 = (AA11a$mean)
    c3 = c3[1:3]
    for (i in 1:length(c3))
      diff31[i] = c3[i]-a3[i]
    sum31 = sum(abs(diff31))
    #print(sum31)
    c3 = 0
    sum32 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c3 = (AA11a$mean)
      c3 = c3[1:3]
      for (i in 1:length(c3))
        diff32[i] = c3[i]-a3[i]
      sum32 = sum(abs(diff32))
      #print(sum32)
      ch = 0
    }
    #choosing the exponential smoothing model for lag upto 2
    if(seasonal2 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y2, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y2)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y2, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c2 = (AA11a$mean)
    c2 = c2[1:2]
    for (i in 1:length(c2))
      diff21[i] = c2[i]-a2[i]
    sum21 = sum(abs(diff21))
    #  print(sum21)
    c2 = 0
    sum22 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c2 = (AA11a$mean)
      c2 = c2[1:2]
      for (i in 1:length(c2))
        diff22[i] = c2[i]-a2[i]
      sum22 = sum(abs(diff22))
      # print(sum22)
      ch = 0
    }
    
    #choosing the exponential smoothing model for lag upto 1
    if(seasonal1 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y1, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y1)
    m = AA11a$method
    m
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y1, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c1 = (AA11a$mean)
    c1 = c1[1:1]
    for (i in 1:length(c1))
      diff11[i] = c1[i]-a1[i]
    sum11 = sum(abs(diff11))
    #print(sum11)
    c1 = 0
    sum12 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c1 = (AA11a$mean)
      c1 = c1[1:1]
      for (i in 1:length(c1))
        diff12[i] = c1[i]-a1[i]
      sum12 = sum(abs(diff12))
      #print(sum12) 
    }
    
    i = 0 
    j = 0 
    
    if(sum31<=sum32| sum32==0){
      i = i+1
    }else
      j = j+1
    
    if(sum21<=sum22| sum22==0){
      i = i+1
    }else
      j = j+1
    
    if(sum11<=sum12| sum12==0){
      i = i+1
    }else
      j = j+1
    
    #forecasting the best model of exponential smoothing for lag upto 7
    if(seasonaL4 == 'TRUE') {
      fit1<-ets(Y4, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(Y4, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(Y4)
    m = AA11a$method
    if(seasonaL4 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(Y4, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }else{
      fit2<-ets(Y4, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    if(i>j){
      AA11a<-forecast(fit1)
      cf4 = AA11a
      ADD = ADD + 1
    }else{
      AA11a<-forecast(fit2)
      cf4 = AA11a
      MUL = MUL + 1
    }
    cf4 = (cf4$mean)
    cf4 = cf4[1:4]
    for (i in 1:length(cf4))
      diff4[i] = cf4[i]-A4[i]
    sume4 = sum(abs(diff4)) #storing the differences of the actual data from the forecasted values for the lag of 4 from the original data 
    #print(sume4)
    
    ###Calculating the sum of error terms lag till 3 from the original data with similar operation as above 
    d3 = L3
    n3 = length(d3)
    #taking last six values for error calculation
    
    #actual dataset for different lag
    
    a3 = d3[(n3-2):n3]
    a2 = d3[(n3-1):n3]
    a1 = d3[n3:n3]
    
    
    #till lag 2
    l3 = d3[1:(n3-3)]
    y3 <- ts(l3, frequency=12)
    fit <- tbats(y3)
    seasonal3 <- !is.null(fit$seasonal)
    #seasonal3
    #till lag 1
    l2 = d3[1:(n3-2)]
    y2 <- ts(l2, frequency=12)
    fit<- tbats(y2)
    seasonal2 <- !is.null(fit$seasonal)
    #seasonal2
    #till lag 0
    l1 = d3[1:(n3-1)] 
    y1 <- ts(l1, frequency=12)
    fit<- tbats(y1)
    seasonal1 <- !is.null(fit$seasonal)
    #seasonal1
    #function for providing the solution of both additive and multiplicative model if seasonal
    # 
    #   diff51=  0
    #   diff41 = 0
    diff31 = 0
    diff21 = 0
    diff11 = 0
    
    # diff52=  0
    # diff42 = 0
    diff32 = 0
    diff22 = 0
    diff12 = 0
    
    ch = 0
    
    #choosing the exponential smoothing model for lag upto 3 
    if(seasonal3 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y3, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y3)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y3, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c3 = (AA11a$mean)
    c3 = c3[1:3]
    for (i in 1:length(c3))
      diff31[i] = c3[i]-a3[i]
    sum31 = sum(abs(diff31))
    #print(sum31)
    c3 = 0
    sum32 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c3 = (AA11a$mean)
      c3 = c3[1:3]
      for (i in 1:length(c3))
        diff32[i] = c3[i]-a3[i]
      sum32 = sum(abs(diff32))
      #print(sum32)
      ch = 0
    }
    #choosing the exponential smoothing model for lag upto 2
    if(seasonal2 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y2, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y2)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y2, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c2 = (AA11a$mean)
    c2 = c2[1:2]
    for (i in 1:length(c2))
      diff21[i] = c2[i]-a2[i]
    sum21 = sum(abs(diff21))
    #  print(sum21)
    c2 = 0
    sum22 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c2 = (AA11a$mean)
      c2 = c2[1:2]
      for (i in 1:length(c2))
        diff22[i] = c2[i]-a2[i]
      sum22 = sum(abs(diff22))
      # print(sum22)
      ch = 0
    }
    
    #choosing the exponential smoothing model for lag upto 1
    if(seasonal1 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y1, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y1)
    m = AA11a$method
    m
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y1, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c1 = (AA11a$mean)
    c1 = c1[1:1]
    for (i in 1:length(c1))
      diff11[i] = c1[i]-a1[i]
    sum11 = sum(abs(diff11))
    #print(sum11)
    c1 = 0
    sum12 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c1 = (AA11a$mean)
      c1 = c1[1:1]
      for (i in 1:length(c1))
        diff12[i] = c1[i]-a1[i]
      sum12 = sum(abs(diff12))
      #print(sum12) 
    }
    
    i = 0 
    j = 0 
    
    if(sum31<=sum32| sum32==0){
      i = i+1
    }else
      j = j+1
    
    if(sum21<=sum22| sum22==0){
      i = i+1
    }else
      j = j+1
    
    if(sum11<=sum12| sum12==0){
      i = i+1
    }else
      j = j+1
    
    #forecasting the best model of exponential smoothing for lag upto 7
    if(seasonaL3 == 'TRUE') {
      fit1<-ets(Y3, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(Y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(Y3)
    m = AA11a$method
    if(seasonaL3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(Y3, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }else{
      fit2<-ets(Y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    if(i>j){
      AA11a<-forecast(fit1)
      cf3 = AA11a
      ADD = ADD + 1
    }else{
      AA11a<-forecast(fit2)
      cf3 = AA11a
      MUL = MUL + 1
    }
    cf3 = (cf3$mean)
    cf3 = cf3[1:3]
    for (i in 1:length(cf3))
      diff3[i] = cf3[i]-A3[i]
    sume3 = sum(abs(diff3)) #storing the differences of the actual data from the forecasted values for the lag of 3 from the original data 
    #print(sume3)
    
    ###Calculating the sum of error terms lag till 2 from the original data with similar operation as above 
    d2 = L2
    n2 = length(d2)
    # seasonal <- !is.null(fit$seasonal)
    
    #actual dataset for different lag
    
    # a5 = d2[(n2-4):n2]
    # a4 = d2[(n2-3):n2]
    a3 = d2[(n2-2):n2]
    a2 = d2[(n2-1):n2]
    a1 = d2[n2:n2]
    
    
    #till lag 2
    l3 = d2[1:(n2-3)]
    y3 <- ts(l3, frequency=12)
    fit <- tbats(y3)
    seasonal3 <- !is.null(fit$seasonal)
    #seasonal3
    #till lag 1
    l2 = d2[1:(n2-2)]
    y2 <- ts(l2, frequency=12)
    fit<- tbats(y2)
    seasonal2 <- !is.null(fit$seasonal)
    #seasonal2
    #till lag 0
    l1 = d2[1:(n2-1)] 
    y1 <- ts(l1, frequency=12)
    fit<- tbats(y1)
    seasonal1 <- !is.null(fit$seasonal)
    #seasonal1
    
    # function for providing the solution of both additive and multiplicative model if seasonal
    
    diff31 = 0
    diff21 = 0
    diff11 = 0
    
    # diff52=  0
    # diff42 = 0
    diff32 = 0
    diff22 = 0
    diff12 = 0
    
    ch = 0
    
    #choosing the exponential smoothing model for lag upto 3 
    if(seasonal3 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y3, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y3)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y3, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c3 = (AA11a$mean)
    c3 = c3[1:3]
    for (i in 1:length(c3))
      diff31[i] = c3[i]-a3[i]
    sum31 = sum(abs(diff31))
    #print(sum31)
    c3 = 0
    sum32 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c3 = (AA11a$mean)
      c3 = c3[1:3]
      for (i in 1:length(c3))
        diff32[i] = c3[i]-a3[i]
      sum32 = sum(abs(diff32))
      #print(sum32)
      ch = 0
    }
    #choosing the exponential smoothing model for lag upto 2
    if(seasonal2 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y2, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y2)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y2, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c2 = (AA11a$mean)
    c2 = c2[1:2]
    for (i in 1:length(c2))
      diff21[i] = c2[i]-a2[i]
    sum21 = sum(abs(diff21))
    #  print(sum21)
    c2 = 0
    sum22 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c2 = (AA11a$mean)
      c2 = c2[1:2]
      for (i in 1:length(c2))
        diff22[i] = c2[i]-a2[i]
      sum22 = sum(abs(diff22))
      # print(sum22)
      ch = 0
    }
    
    #choosing the exponential smoothing model for lag upto 1
    if(seasonal1 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y1, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y1)
    m = AA11a$method
    m
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y1, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c1 = (AA11a$mean)
    c1 = c1[1:1]
    for (i in 1:length(c1))
      diff11[i] = c1[i]-a1[i]
    sum11 = sum(abs(diff11))
    #print(sum11)
    c1 = 0
    sum12 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c1 = (AA11a$mean)
      c1 = c1[1:1]
      for (i in 1:length(c1))
        diff12[i] = c1[i]-a1[i]
      sum12 = sum(abs(diff12))
      #print(sum12) 
    }
    
    i = 0 
    j = 0 
    
    if(sum31<=sum32| sum32==0){
      i = i+1
    }else
      j = j+1
    
    if(sum21<=sum22| sum22==0){
      i = i+1
    }else
      j = j+1
    
    if(sum11<=sum12| sum12==0){
      i = i+1
    }else
      j = j+1
    
    #forecasting the best model of exponential smoothing for lag upto 2
    if(seasonaL2 == 'TRUE') {
      fit1<-ets(Y2, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(Y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(Y2)
    m = AA11a$method
    if(seasonaL2 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(Y2, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }else{
      fit2<-ets(Y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    if(i>j){
      AA11a<-forecast(fit1)
      cf2 = AA11a
      ADD = ADD + 1
    }else{
      AA11a<-forecast(fit2)
      cf2 = AA11a
      MUL = MUL + 1
    }
    cf2 = (cf2$mean)
    cf2 = cf2[1:2]
    for (i in 1:length(cf2))
      diff2[i] = cf2[i]-A2[i]
    sume2 = sum(abs(diff2)) #storing the differences of the actual data from the forecasted values for the lag of 2 from the original data 
    # print(sume2)
    
    ###Calculating the sum of error terms lag till 1 from the original data with similar operation as above 
    d1 = L1
    n1 = length(d1)
    #taking last six values for error calculation
    
    
    #actual dataset for different lag
    a3 = d1[(n1-2):n1]
    a2 = d1[(n1-1):n1]
    a1 = d1[n1:n1]
    
    
    l3 = d1[1:(n1-3)]
    y3 <- ts(l3, frequency=12)
    fit <- tbats(y3)
    seasonal3 <- !is.null(fit$seasonal)
    #seasonal3
    #till lag 1
    l2 = d1[1:(n1-2)]
    y2 <- ts(l2, frequency=12)
    fit<- tbats(y2)
    seasonal2 <- !is.null(fit$seasonal)
    #seasonal2
    #till lag 0
    l1 = d1[1:(n1-1)] 
    y1 <- ts(l1, frequency=12)
    fit<- tbats(y1)
    seasonal1 <- !is.null(fit$seasonal)
    #seasonal1
    
    
    # function for providing the solution of both additive and multiplicative model if seasonal
    
    diff31 = 0
    diff21 = 0
    diff11 = 0
    
    # diff52=  0
    # diff42 = 0
    diff32 = 0
    diff22 = 0
    diff12 = 0
    
    ch = 0
    
    #choosing the exponential smoothing model for lag upto 3 
    if(seasonal3 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y3, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y3)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y3, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y3, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c3 = (AA11a$mean)
    c3 = c3[1:3]
    for (i in 1:length(c3))
      diff31[i] = c3[i]-a3[i]
    sum31 = sum(abs(diff31))
    #print(sum31)
    c3 = 0
    sum32 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c3 = (AA11a$mean)
      c3 = c3[1:3]
      for (i in 1:length(c3))
        diff32[i] = c3[i]-a3[i]
      sum32 = sum(abs(diff32))
      #print(sum32)
      ch = 0
    }
    #choosing the exponential smoothing model for lag upto 2
    if(seasonal2 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y2, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y2)
    m = AA11a$method
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y2, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y2, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c2 = (AA11a$mean)
    c2 = c2[1:2]
    for (i in 1:length(c2))
      diff21[i] = c2[i]-a2[i]
    sum21 = sum(abs(diff21))
    #  print(sum21)
    c2 = 0
    sum22 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c2 = (AA11a$mean)
      c2 = c2[1:2]
      for (i in 1:length(c2))
        diff22[i] = c2[i]-a2[i]
      sum22 = sum(abs(diff22))
      # print(sum22)
      ch = 0
    }
    
    #choosing the exponential smoothing model for lag upto 1
    if(seasonal1 == 'TRUE' && seasonaL7 == 'TRUE') {
      fit1<-ets(y1, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(y1)
    m = AA11a$method
    m
    if(seasonal3 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(y1, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      AA11a<-forecast(fit2) #checking if ZZM model works
      ch = AA11a$mean
    }else{
      fit2<-ets(y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    AA11a<-forecast(fit1)
    c1 = (AA11a$mean)
    c1 = c1[1:1]
    for (i in 1:length(c1))
      diff11[i] = c1[i]-a1[i]
    sum11 = sum(abs(diff11))
    #print(sum11)
    c1 = 0
    sum12 = 0
    if(ch[1]!=0){
      AA11a<-forecast(fit2)
      c1 = (AA11a$mean)
      c1 = c1[1:1]
      for (i in 1:length(c1))
        diff12[i] = c1[i]-a1[i]
      sum12 = sum(abs(diff12))
      #print(sum12) 
    }
    
    i = 0 
    j = 0 
    
    if(sum31<=sum32| sum32==0){
      i = i+1
    }else
      j = j+1
    
    if(sum21<=sum22| sum22==0){
      i = i+1
    }else
      j = j+1
    
    if(sum11<=sum12| sum12==0){
      i = i+1
    }else
      j = j+1
    
    #forecasting the best model of exponential smoothing for lag upto 1
    if(seasonaL1 == 'TRUE') {
      fit1<-ets(Y1, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else {
      fit1<-ets(Y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(Y1)
    m = AA11a$method
    if(seasonaL1 == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(Y1, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }else{
      fit2<-ets(Y1, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    if(i>j){
      AA11a<-forecast(fit1)
      cf1 = AA11a
      ADD = ADD + 1
    }else{
      AA11a<-forecast(fit2)
      cf1 = AA11a
      MUL = MUL + 1
    }
    cf1 = (cf1$mean)
    cf1 = cf1[1:1]
    for (i in 1:length(cf1))
      diff1[i] = cf1[i]-A1[i]
    sume1 = sum(abs(diff1))  #storing the differences of the actual data from the forecasted values for the lag of 1 from the original data 
    # print(sume1)
    
    
    #exponential model for original values 
    fit <- tbats(Y) 
    plot(fit) 
    seasonaL <- !is.null(fit$seasonal) #checking seasonality of original data 
    #  seasonaL
    
    if(seasonaL == 'TRUE') { # if original data is seasonal then applying additive seasonality model
      fit1<-ets(Y, model = "ZZA", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
      
    }  else { #if data is not seasonal applying no seasonality model
      fit1<-ets(Y, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }
    
    i #allowing mltiplicative seasonality only when ZZZ model gives multiplicative model to remove the error
    m = 0
    AA11a = ets(Y)  #Checking if ZZZ model selects multiplicative model then only apllyimg the multiplicative model on the data to avoid errors
    m = AA11a$method 
    if(seasonaL == 'TRUE' && (m == "ETS(M,N,M)"| m == "ETS(M,A,M)" |m == "ETS(M,M,M)" |m == "ETS(N,N,M)" |m == "ETS(N,A,M)" |m == "ETS(N,M,M)" |m == "ETS(A,N,M)" |m == "ETS(A,A,M)" |m == "ETS(A,M,M)")){
      fit2<-ets(Y, model = "ZZM", damped = NULL, alpha = NULL, beta = NULL,  # if original data is seasonal then applying additive seasonality model
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,
                use.initial.values = FALSE)
    }else{ #if data is not seasonal applying no seasonality model
      fit2<-ets(Y, model = "ZZN", damped = NULL, alpha = NULL, beta = NULL,
                gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
                biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999,
                                                                              3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"), nmse = 3,
                bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"),
                restrict = TRUE, allow.multiplicative.trend = TRUE,   
                use.initial.values = FALSE)
    }
    
    if(ADD>MUL){  #if more number of times model type 1 is choosen final forecast frm exponential smoothing model will be produced using model 1
      AA11a<-forecast(fit1,h=12)
      EM = AA11a
    }else{   #if more number of times model type 2 is choosen final forecast frm exponential smoothing model will be produced using model 2
      AA11a<-forecast(fit2,h=12)
      EM = AA11a
    }
  }
 
  
  
#deciding the Forecasting models via expert selection
  if(n==1)
  {
    for (i in 1:12) {
      a[i] = e[1]
    }
    print(a)
    M[k] = "modified SMA lag1" #storing the name of the model
  }
  
  if(n==2){
    a[1] =  (e[1]+e[2])/2
    a[2] = (e[2]+a[1])/2
    for (i in 3:12) { #storing data upto 12 
      a[i] = (a[i-2]+a[i-1])/2
    }
    plot(a)
    print(a)
    M[k] = "modified SMA lag2" #storing the name of the model
  }
  
  if((n<=8) && (n>=3)){
    sum3 = 0
    ma3 <- 0
    smalag3 <- function(x){
      for (i in (length(x)-2):length(x))  
        sum3 = sum3 + x[i] #using the last three values
      ma3[1] =sum3/3  #taking the average of last three values 
      sum3 = 0
      for (i in (length(x)-1):length(x))
        sum3 = sum3 + x[i]  
      ma3[2] = (sum3+ma3[1])/3 #taking the average with last two values of data and one value of the forecast produced above 
      sum3 = 0
      
      ma3[3] = (ma3[1]+ma3[2]+ x[length(x)])/3 #taking the last one value and two forecasted values produced for forecasting the third one  
      #ma3
     # n = length(ma3)
      for(j in 1:12-3){ #getting the remaining forecast of the 12 value to be forecasted for lag upto 3
        for (i in (length(ma3)-2):length(ma3)){
          sum3 = sum3 + ma3[i]} #taking the avaerage of latest three values to forecast the next one
        s = sum3 
        sum3 = 0
        ma3[(j+3)] <- (s)/3
      }
      return(ma3) #returuning an array with forecast upto 12 values
    }
    a = smalag3(e)
    print(a)
    plot(a)
    M[k] = "modified SMA lag3"
  }
  
  #for data less then 2 years sma model
  if((n<=24) && (n>=8)){
    d= e[1:(n-8)]  #data with last 8 items less then original
    f = e[(n-7):n] #last 8 remaining data
    f
    asum = 0
    for(i in 1:8)
      asum = asum + f[i]  #calculating the sum of of last 8 remaining items
    asum
    
    sum3 = 0 #variable to store the sum of last 8 forecasted values with lag upto 3
    sum4 = 0 #variable to store the sum of last 8 forecasted values with lag upto 4
    sum5 = 0
    sum6 = 0
    ma3 <- 0
    ma4 <- 0
    ma5 <- 0
    ma6 <- 0
    # function for sma model for lag of 3 (forecasting the last 8 values)
    smalag3 <- function(x){
      for (i in (length(x)-2):length(x))  
        sum3 = sum3 + x[i] #using the last three values
      ma3[1] =sum3/3  #taking the average of last three values 
      sum3 = 0
      for (i in (length(x)-1):length(x))
        sum3 = sum3 + x[i]  
      ma3[2] = (sum3+ma3[1])/3 #taking the average with last two values of data and one value of the forecast produced above 
      sum3 = 0
      
      ma3[3] = (ma3[1]+ma3[2]+ x[length(x)])/3 #taking the last one value and two forecasted values produced for forecasting the third one  
      #ma3
      
      for(j in 1:12-3){ #getting the first forecast of the 8 value to be forecastedfor lag upto 3
        for (i in (length(ma3)-2):length(ma3)){ 
          sum3 = sum3 + ma3[i]} #taking the avaerage of latest three values to forecast the next one
        a = sum3 
        sum3 = 0
        ma3[(j+3)] <- (a)/3
      }
      return(ma3) #returuning an array with forecast upto 12 values
    } 
    
    ## sma model for lag of 4 (forecasting the last 8 values)
    smalag4 <- function(x){
      for (i in (length(x)-3):length(x))
        sum4 = sum4 + x[i] #calculating the avaerage using the last 4 values to forecast the next value
      ma4[1] =sum4/4 
      sum4 = 0
      for (i in (length(x)-2):length(x)) 
        sum4 = sum4 + x[i]             
      ma4[2] = (sum4+ma4[1])/4    #calcuating the average using the latest 4 values including the one produced above
      sum4 = 0
      for (i in (length(x)-1):length(x))
        sum4 = sum4 + x[i]
      ma4[3] = (sum4+ma4[1]+ma4[2])/4 #calcuating the average using the latest 4 values including the two produced above
      sum4 = 0
      ma4[4] = (ma4[1]+ma4[2]+ ma4[3]+ x[length(x)])/4
      #ma3
      
      for(j in 1:12-4){ #getting the remaining forecast of the 8 value to be forecasted for lag upto 4
        for (i in (length(ma4)-3):length(ma4)){
          sum4 = sum4 + ma4[i]}      #taking the avaerage of latest 4 values to forecast the next one
        b = sum4 
        sum4 = 0
        ma4[(j+4)] <- (b)/4
        # sum3 = 0
      }
      return(ma4)}   #returuning an array with forecast upto 8 values
    
    ## sma model for lag of 5 (forecasting the last 8 values)
    smalag5 <- function(x){
      for (i in (length(x)-4):length(x)) 
        sum5 = sum5 + x[i]   
      ma5[1] =sum5/5    #calculating the average using the last 5 values of the data to forecast next value
      sum5 = 0
      for (i in (length(x)-3):length(x))
        sum5 = sum5 + x[i]
      ma5[2] = (sum5+ma5[1])/5    #calcuating the average using the latest 5 values including the one produced above
      sum5 = 0
      for (i in (length(x)-2):length(x))
        sum5 = sum5 + x[i]
      ma5[3] = (sum5+ma5[1]+ma5[2])/5       #calcuating the average using the latest 5 values including the 2 produced above
      sum5 = 0
      for (i in (length(x)-1):length(x))
        sum5 = sum5 + x[i]
      ma5[4] = (sum5+ma5[1]+ma5[2]+ma5[3])/5  #calcuating the average using the latest 5 values including the 3 produced above
      sum5 = 0
      ma5[5] = (ma5[1]+ma5[2]+ ma5[3]+ma5[4]+ x[length(x)])/5
      
      for(j in 1:12-5){ #getting the remaining forecast of the 8 values to be forecasted for lag upto 5
        for (i in (length(ma5)-4):length(ma5)){
          sum5 = sum5 + ma5[i]}
        c = sum5               #taking the avaerage of latest 4 values to forecast the next one
        sum5 = 0
        ma5[(j+5)] <- (c)/5
      }
      return(ma5)   #returuning an array with forecast upto 8 values 
    }
    #smalag5(e)
    
    ##sma model for lag of 6 (forecasting the last 8 values)
    smalag6 <- function(x){
      for (i in (length(x)-5):length(x)) 
        sum6 = sum6 + x[i]
      ma6[1] =sum6/6   #calculating the average using the last 6 values of the data to forecast next value
      sum6 = 0
      for (i in (length(x)-4):length(x))
        sum6 = sum6 + x[i]             #calcuating the average using the latest 6 values including the one produced above
      ma6[2] = (sum6+ma6[1])/6
      sum6 = 0
      for (i in (length(x)-3):length(x))
        sum6 = sum6 + x[i]              
      ma6[3] = (sum6+ma6[1]+ma6[2])/6   #calcuating the average using the latest 6 values including the 2 produced above
      sum6 = 0
      for (i in (length(x)-2):length(x))
        sum6 = sum6 + x[i]
      ma6[4] = (sum6+ma6[1]+ma6[2]+ma6[3])/6  #calcuating the average using the latest 6 values including the 3 produced above
      sum6 = 0
      for (i in (length(x)-1):length(x))
        sum6 = sum6 + x[i]
      ma6[5] = (sum6+ma6[1]+ma6[2]+ma6[3]+ma6[4])/6
      sum6 = 0
      ma6[6] = (ma6[1]+ma6[2]+ ma6[3]+ma6[4]+ x[length(x)])/6  #calcuating the average using the latest 6 values including the 4 produced above
      #n = length(ma6)
      
      for(j in 1:12-6){  #getting the remaining forecast of the 8 values to be forecasted for lag upto 6
        for (i in (length(ma6)-5):length(ma6)){
          sum6 = sum6 + ma6[i]}
        d = sum6              
        sum6 = 0             
        ma6[(j+6)] <- (d)/6      #taking the avaerage of latest 4 values to forecast the next one
      }
      return(ma6) #returuning an array with forecast upto 8 values
    }
    #putting the vales of forecasted vales in array
    a1 = smalag3(d)
    a2 = smalag4(d)
    a3 = smalag5(d)
    a4 = smalag6(d)
    a1 = a1[1:8]
    a2 = a2[1:8]
    a3 = a3[1:8]
    a4 = a4[1:8]
    s1 = 0
    s2 = 0
    s3 = 0
    s4 = 0
    #calculating the sum 8 values forecasted in each of the case
    for(i in 1:8)
      s1 = s1+a1[i]  #for lag 3
    for(i in 1:8) 
      s2 = s2+a2[i]  #for lag 4
    for(i in 1:8)
      s3 = s3+a3[i]  #for lag 5
    for(i in 1:8)
      s4 = s4+a4[i]  #for lag 6
    
  #calculating the absolute difference for each forecast with the sum of Actual forecast
    diff1 = abs(s1-asum) #for lag 3
    diff2 = abs(s2-asum) #for lag 4
    diff3 = abs(s3-asum) #for lag 5
    diff4 = abs(s4-asum) #for lag 6
    
    v = 0
    v[1]= diff1
    v[2]= diff2
    v[3]= diff3
    v[4]= diff4
    
    t = min(v)
    
    #  result <- function(x){
    if(t == diff1){       #if the absolute difference is smallest for lag 3 this model is used to forecast the future values
      print('smalag3')
      a = (smalag3(e))
      print(a)
      plot(a)
      M[k] = "modified SMA lag3"  #storing the name of the model
    }
    if(t == diff2){
      print('smalag4')   #if the absolute difference is smallest for lag 4 this model is used to forecast the future values
      a = (smalag4(e))
      print(a)
      plot(a)
      M[k] = "modified SMA lag4" #storing the name of the model
    }
    if(t == diff3){
      print('smalag5')  #if the absolute difference is smallest for lag 5 this model is used to forecast the future values
      a = (smalag5(e))
      print(a)
      plot(a)
      M[k] = "modified SMA lag5" #storing the name of the model
    }
    if(t == diff4){
      print('smalag6')  #if the absolute difference is smallest for lag 6 this model is used to forecast the future values
      a = (smalag6(e))
      print(a)
      plot(a)
      M[k] = "modified SMA lag6"  #storing the name of the model
    }
  }
  
  #intermittent model is selected if the number of zeros is more then 0.35 times the length of the the data  
  if(j>=(.35*length(e))){
    fcast <- croston(e,h=12)  #crosten model is applied on the data
    AA11a = fcast
    a = AA11a$mean  #forecast is stored in "a" 
    for (i in 1:12) {
      if(a[i]<0)
        a[i] = 0
    }
    M[k] = AA11a$method  #storing the name of the model
    print(a)
    plot(fcast)
  }
  
  #if data is ntw 2 years and 2.5 years applying exponential smoothing model
  if((n>24)&& (n<=30)){
    #exponential smoothing model
    a = EM$mean    #storing the model selected above 
    for (i in 1:12) { 
      if(a[i]<0)     
        a[i] = 0  #if forecast has negative values assigning those values as "0" 
    }
    M[k] = EM$method #storing the name of the model
    print(EM$mean)
    plot(EM)  
  }
  
  # possibility of either arima or the exponential smoothing model is to be selected 
  # we decide on the basis of MAD values
  if(n>30){
    
    diff7 = 0
    diff6 = 0
    diff5 = 0
    diff4 = 0
    diff3 = 0
    diff2 = 0
    diff1 = 0
    
    #creating a function for choosing the best arima model using auto.arima 
    arima <- function(x){
      auto.arima(x, max.p = 5, max.q = 5, max.P = 2,
                 max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
                 start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
                 seasonal = TRUE, ic = c("aicc", "aic", "bic"), stepwise = TRUE,
                 trace = FALSE, approximation = (length(x) > 150 | frequency(x) > 12),
                 xreg = NULL, test = c("kpss", "adf", "pp"),
                 seasonal.test = c("ocsb"), allowdrift = TRUE,
                 allowmean = TRUE, lambda = NULL, biasadj = FALSE, parallel = FALSE,
                 num.cores = 2)
    }
    # calculating the total error for lag till 6
    AA11a<-forecast(arima(Y7))
    c7 = (AA11a$mean)
    c7 = c7[1:7]  #taking 7 forecasted values to compare with exponential model  
    for (i in 1:length(c7))
      diff7[i] = c7[i]-A7[i] #calculating the difference of actual and forecasted value for lag 7
    suma7 = sum(abs(diff7))  #calculating the sum of differences for lag upto 7
    print(suma7)
    print(sume7)
    
    #calculating the total error for lag till 5
    AA11a<-forecast(arima(Y6))
    c6 = (AA11a$mean)
    c6 = c6[1:6]   #taking 6 forecasted values to compare with exponential model 
    for (i in 1:length(c6))
      diff6[i] = c6[i]-A6[i] #calculating the difference of actual and forecasted value for lag 6
    suma6 = sum(abs(diff6))  #calculating the sum of differences for lag upto 6
    print(suma6)
    print(sume6)
    
    #calculating the total error for lag till 4
    AA11a<-forecast(arima(Y5))
    c5 = (AA11a$mean)
    c5 = c6[1:5]   #taking 5 forecasted values to compare with exponential model 
    for (i in 1:length(c5))
      diff5[i] = c5[i]-A5[i]  #calculating the difference of actual and forecasted value for lag 5
    suma5 = sum(abs(diff5))  #calculating the sum of differences for lag upto 5
    print(suma5)
    print(sume5)
    
    #calculating the total error for lag till 3
    AA11a<-forecast(arima(Y4))
    c4 = (AA11a$mean)
    c4 = c4[1:4]   #taking 4 forecasted values to compare with exponential model 
    for (i in 1:length(c4))
      diff4[i] = c4[i]-A4[i]  #calculating the difference of actual and forecasted value for lag 4 
    suma4 = sum(abs(diff4))  #calculating the sum of differences for lag upto 4
    print(suma4)
    print(sume4)
    
    #calculating the total error for lag till 2
    AA11a<-forecast(arima(Y3))
    c3 = (AA11a$mean)
    c3 = c3[1:3] #taking 3 forecasted values to compare with exponential model 
    for (i in 1:length(c3))
      diff3[i] = c3[i]-A3[i] #calculating the difference of actual and forecasted value for lag 3
    suma3 = sum(abs(diff3)) #calculating the sum of differences for lag upto 3
    print(suma3)
    print(sume3)
    
    #for lag till 1
    AA11a<-forecast(arima(Y2))
    c2 = (AA11a$mean)
    c2 = c2[1:2]  #taking 2 forecasted values to compare with exponential model 
    for (i in 1:length(c2))
      diff2[i] = c2[i]-A2[i] #calculating the difference of actual and forecasted value for lag 2
    suma2 = sum(abs(diff2)) #calculating the sum of differences for lag upto 2
    print(suma2)
    print(sume2)
    
    #calculating the total error for lag till 0
    AA11a<-forecast(arima(Y1))
    c1 = (AA11a$mean)
    c1 = c1[1:1]  #taking 1 forecasted values to compare with exponential model 
    for (i in 1:length(c1))
      diff1[i] = c1[i]-A1[i]  #calculating the difference of actual and forecasted value for lag 1
    suma1 = sum(abs(diff1)) #calculating the sum of differences for lag upto 1
    print(suma1)
    print(sume1)
    
    #printing the sum of lag for exponential
    
    #COMPARING THE MAD VALUES OF ARIMA AND EXPONENTIAL MODEL
    A = 0 
    E = 0 
    
    if(suma1<=sume1){ #for lag upto 1
      A = A+1 # A is increased if the MAD value for arima is less 
    }else
      E = E+1 # E is increased if the MAD value for exponential smoothing  is less
    
    if(suma2<=sume2){ #for lag upto 2
      A = A+1  # A is increased if the MAD value for arima is less 
    }else
      E = E+1  # E is increased if the MAD value for exponential smoothing  is less
    
    if(suma3<=sume3){ #for lag upto 3
      A = A+1 # A is increased if the MAD value for arima is less
    }else
      E = E+1 # E is increased if the MAD value for exponential smoothing  is less
    
    if(suma4<=sume4){ #for lag upto 4
      A = A+1 # A is increased if the MAD value for arima is less
    }else
      E = E+1  # E is increased if the MAD value for exponential smoothing  is less
    
    if(suma5<=sume5){ #for lag upto 5
      A = A+1 # A is increased if the MAD value for arima is less
    }else
      E = E+1 # E is increased if the MAD value for exponential smoothing  is less
    
    if(suma6<=sume6){ #for lag upto 6
      A = A+1 # A is increased if the MAD value for arima is less
    }else
      E = E+1 # E is increased if the MAD value for exponential smoothing  is less
    
    if(suma7<=sume7){ #for lag upto 7
      A = A+1 # A is increased if the MAD value for arima is less
    }else
      E = E+1 # E is increased if the MAD value for exponential smoothing  is less
    
    #selecting b/w arima and exponential model based on MAD values 
    
    if(A>E){  #if arima model has more smaller error sums then arima model is selected 
      AA11a<-forecast(arima(Y),h=12) 
      a = AA11a$mean #storing the forecast in "a"
      for (i in 1:12) {
        if(a[i]<0)
          a[i] = 0
      }
      M[k] = AA11a$method #storing the model name
      print(M[k])
      print(a)
      plot(AA11a)
    }else{  
      # best of the exponential model selected above
      a = EM$mean  #storing the forecast in "a"
      for (i in 1:12) {
        if(a[i]<0)
          a[i] = 0
      }
      print(a)
      M[k] = EM$method  #storing the model name
      print(M[k])
      plot(EM)
    }
  }
  #CONVERTING THE FORECAST TO OUTPUT FORM
  # print(a)
  # print(M[k])
  COUNT = k #for assinging the forecast to its respective data in output
  N
  D
  leng = length(a)
  for (j in 1:leng) {
    for (i in COUNT:N) {
      if(i==COUNT)
        df[(i),j+D] = a[j] #merging the source data with its respective forecast values
      else
        df[i,j+D] = 0
    }
  }
}

#giving the model name in output
da = data.frame()
#da = da[,'model'] 
for (i in 1:N) {
  if(i<=k){
    da[i,'model'] = M[i]  #creating a new dataframe with the name of model used to forecast for each data 
  }else{
    da[i,'model'] = "NA"
  }
}
df['model'] = da #merging the model name dataframe with the datframe with forecast value and its sorce info.

# df = merge(df, da)

 write.csv(df, file = "output15001510.csv") #converting the dataframe to csv
