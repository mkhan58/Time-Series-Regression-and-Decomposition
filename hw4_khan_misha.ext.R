#Misha Khan
#BANA 288 Predictive Analytics
#Spring 2021

#Homework #4: Time Series Regression and Decomposition

install.packages("dplyr")
library(dplyr)

pre.dat <- read.csv("~/Desktop/SPRING 2021/Predictive/Homework 4/hw4_bike_share_day.csv")
names(pre.dat)
dim(pre.dat)

################################################################################
#1. Perform all necessary cleaning and transformation of the data to make it useful 
#for linear regression.  That is, transform all qualitative variables to factors.  
#No need to set up individual indicator columns in this assignment.  Add a 
#time trend variable and a quadratic time trend (trend squared) variable to the 
#data set.  Hint: One way to add these new “time” columns is to define a new column

#timetr <- data.frame(1:731) timetrsq <- timetr^2 and 
#then use the “cbind” command to bind these new columns to the data set.  
#Use the “names” command to give these new variables names, if necessary.  
#List all the variables transformed and/or added in the comments.

#Qualitative var: season, yr, mnth, weekday, weathersit into factors
dat <- pre.dat
dat$season <- as.factor(dat$season)
dat$yr <- as.factor(dat$yr)
dat$mnth <- as.factor(dat$mnth) 
dat$weekday <- as.factor(dat$weekday) 
dat$weathersit <- as.factor(dat$weathersit) 

#Add a time trend variable and a quadratic time trend
#timetr <- data.frame(1:731)
#timetrsq <- timetr^2
timetr <- data.frame(1:nrow(dat))
timersq <- timetr^2
date <- as.Date(dat$dteday, format = "%m/%d/%Y")

dat <- cbind(dat, timetr, timersq)


#Rename timetr, timersq var
dat <- dat %>% rename(timetr = names(.)[15])
dat <- dat %>% rename(timetrsq = names(.)[16])

################################################################################
#2. Which of the variables could be considered as candidates for seasonal 
#variables in this data set?  Explain you answer clearly.

#wouldn't make sense to run plots on qualitative var -> we get box plots
plot(dat$date, dat$cnt, xlab = "Date", ylab = "Count")
lines(dat$date, dat$cnt, type = "l")


plot(dat$mnth, dat$cnt, xlab = "Month", ylab = "Count")
lines(dat$mnth, dat$cnt, type = "l")

#Date does not explain seasonality. Month is more comprehensive of the changes
#over the year. The plot displays warmer months have a higher count of bike
#rentals compared to colder months. Months is a good variable to explain 
#seasonality in future forecasts.
################################################################################
#3. Run a regression model with time, time-squared, and the month variables.  
#Comment on the quality of the model.  Is there a significant trend in the 
#data set?  Squared trend?  Is there significant seasonality?  Compute the RMSE.

reg1 <- lm(cnt ~ timetr + timetrsq + mnth, data = dat)
summary(reg1)

#df: month is being factorized 12 var = 11 var, timetr and timetrsq both have 1 var and -1 for intercept

yhat <- predict(reg1, dat)
err_reg1 <- dat$cnt - yhat
RSS_reg1 <- sum(err_reg1^2)
MSE_reg1 <- RSS_reg1/(nrow(dat)-13-1)
RMSE_reg1 <- MSE_reg1^0.5
RMSE_reg1

#RMSE: 1047.201

#R squared 71.31% -> explains the variability of # in counts is explained by
#timer, timersq and month variables (fairly good)

#But with regression, the RMSE is very high (1047.201). Therefore,
#regression is not a good approach to use to fit the dataset.

#Timersq is a negative coeff so there is an inverse relationship
#with bikes rented (with every increase in timrsq, count decreases)

#Regression is unable to pick up on seasonality of the data because
#when the months are warmer, rental goes up. 

################################################################################
#4. Repeat question 3 by replacing the “month” variables with the “season” variables.  
#Compare to the model of question 3.  Which is preferred and why?  Compute the RMSE.

reg2 <- lm(cnt ~ timetr + timetrsq + season, data = dat)
summary(reg2)

#df: season has 4, time+timetrsq has 1 = 5, intercept has 1

yhat <- predict(reg2, dat)
err_reg2 <- dat$cnt - yhat
RSS_reg2 <- sum(err_reg2^2)
MSE_reg2 <- RSS_reg2/(nrow(dat)-5-1)
RMSE_reg2 <- MSE_reg2^0.5
RMSE_reg2
#RMSE: 1170.851

#In this model, the regression RMSE is higher with season than months.
#This makes sense because season is more general than month so 4 seasons fails
#to pick up on seasonality like the 12 months.

################################################################################
#5. Create a (new) time series object, “cntts”, from the count (cnt) variable 
#with frequency 7.  Plot the new time series version of the count variable in time. 
#Describe any signals in this series.  Plot the autocorrelation function for the series.  
#Is there significant autocorrelation?  Explain.

install.packages("fpp2")
library(fpp2)

#Weekly: frequency = 7 
cntts <- ts(dat[,14], frequency = 7) 

#Plot
autoplot(cntts) +
  ggtitle("Weekly Bike Rentals 2011 - 2012") +
  ylab("Bike Rental Count") +
  xlab("Week")

ggAcf(cntts)

#Signals: Extreme Autocorrelation, Upward Trend, Seasonality
#In ACF plot, it decreases over time which is due to trend, but has a slight parabolic pattern due to seasonality.
#Significant autocorrelation because spikes exceed blue dotted line.

################################################################################
#6. Generate a 28-period forecast using random walk forecast with drift and a 
#second forecast using the seasonal naïve forecast using the count series (cntts).  
#Compute the RMSE for each forecast.  Run time series cross-validation for each 
#of the two forecasted series.  Report RMSEs from the cross-validation routines.

#Naive with drift uses the last observation, but allows for a trend component
cntts_nvd <- rwf(cntts, h = 28, drift = TRUE)

autoplot(cntts_nvd) +
  ggtitle("Random Walk Forecase with Drift")
  ylab("Bike Rental Count") +
  xlab("Week")
#Constant line, not a good forecast
  
#Seasonal naive forecast
cntts_snv <- snaive(cntts, h = 28)
autoplot(cntts_snv) +
  ggtitle("Seasonal Naive Forecast")
ylab("Bike Rental Count") +
  xlab("Week")

#RMSE nvd
cntts.res_nvd <- cntts_nvd$residuals
MSE_rwf <- mean((cntts.res_nvd[2:731])^2)
RMSE_rwf <- MSE_rwf^0.5
RMSE_rwf #RMSE 1064.525


#RMSE snv
cntts.res_snv <- cntts_snv$residuals
MSE_snv <- mean((cntts.res_snv[8:731])^2)
RMSE_snv <- MSE_snv^0.5
RMSE_snv #RMSE 1341.106

#TSCV nvd
cntts.tsCV.nvd <- tsCV(cntts, rwf, drift=TRUE, h=28)
sqrt(mean(cntts.tsCV.nvd^2, na.rm=TRUE))
RMSE.nvd.tsCV <- sqrt(mean(residuals(rwf(cntts, drift=TRUE))^2, na.rm=TRUE)) #RMSE 1064.525

#TSCV snv
cntts.tsCV.snv <- tsCV(cntts, snaive, h=28)
sqrt(mean(cntts.tsCV.snv^2, na.rm=TRUE))
RMSE.snv.tsCV <- sqrt(mean(residuals(snaive(cntts))^2, na.rm=TRUE)) #RMSE 1341.106

#Random walk forecast with drift has a lower RMSE than seasonal naive forecast.
#CV did not improve either RMSE

################################################################################
#7. Estimate a 5-period (centered) moving average model for the count data 
#(cntts), that is, a moving average model with order equal to 5.  
#What is the RMSE (root mean squared error) of this model?  
#Try a few other odd values (e.g., 11 and 17) for the number periods 
#and compute RMSEs to see if a better model can be found.  
#Plot these models on the same graph.  Report the RMSE for the best “ma” model.

ma7 <- ma(cntts, order = 5) 
residual7 <- cntts - ma7
#MSE7 <- mean((residual7[3:729])^2) #first 2 and last 2 values are NA
MSE7 <- mean((na.omit(residuals7))^2)
RMSE7 <- MSE7^0.5
RMSE7 #707.5955

ma7.2 <- ma(cntts, order = 11)
residual7.2 <- cntts - ma7.2
#MSE7.2 <- mean((residual7.2[6:725])^2) #first 2 and last 2 values are NA
MSE7.2 <- mean((na.omit(residuals7.2))^2)
RMSE7.2 <- MSE7.2^0.5
RMSE7.2 #866.9827

ma7.3 <- ma(cntts, order = 17) 
residual7.3 <- cntts - ma7.3
#MSE7.3 <- mean((residual7.3[9:723])^2) #first 2 and last 2 values are NA
MSE7.3 <- mean((na.omit(residuals7.3))^2)
RMSE7.3 <- MSE7.3^0.5
RMSE7.3 #879.68

autoplot(cntts, series="Data") +
  autolayer(ma(cntts, 5), series= "5-MA") +
  autolayer(ma(cntts, 11), series= "11-MA") +
  autolayer(ma(cntts, 17), series= "17-MA") +
  xlab("Time") + ylab("Rentals") +
  ggtitle("Bike Rental Count") +
  scale_colour_manual(values=c("Data" = "black","5-MA" = "blue", "11-MA" = "red", "17-MA" = "green"),
                      breaks=c("Data", "5-MA", "11-MA", "17-MA"))

#Plot shows MA-5 follows the variation of the data
#Whereas MA-11 and MA-17 don't include the variation
#So MA-5 is the best and also has the lowest RMSE

################################################################################
#8. Execute the classical additive and multiplicative decompositions on the 
#count (cntts) series.  Based on the two decompositions, what is observed 
#regarding trend and seasonal effect?  Compute the RMSE from the remainder 
#series for both these decompositions.  Hint:  Use the “remainder” command 
#to obtain the remainder component of a decomposition. 

#Add
dc1_classical_add <- decompose(cntts, type = "additive")
autoplot(dc1_classical_add) +
  ggtitle("Bike Rental Additive Decomposition") + 
  xlab("Year")

dc1_classical_add$random
MSE_dc1_cA <- mean((dc1_classical_add$random[4:728])^2)
RMSE_dc1_cA <- MSE_dc1_cA^0.5
RMSE_dc1_cA #781.3811


#Mult
dc1_classical_mult <- decompose(cntts, type = "multiplicative")
autoplot(dc1_classical_mult) +
  ggtitle("Bike Rental Multiplicative Decomposition") + 
  xlab("Year")

fit_cM <- dc1_classical_mult$trend * dc1_classical_mult$seasonal
MSE_dc1_cM <- mean((na.omit(cntts - dc1_classical_mult$trend * dc1_classical_mult$seasonal))^2)
RMSE_dc1_cM <- MSE_dc1_cM^0.5
RMSE_dc1_cM #782.5306

#Additive did better on seasonality (has spikes then flattening then spikes > explains seasonality)
#Multiplicative explains the trend, over time the pattern is amplified

################################################################################
#9. Apply the STL decomposition to the count (cntts) series.  Use s.window = 11 
#and t.window = 7 for these required parameters.  Compute the RMSE from the 
#remainder series.  Hint:  See Hyndman, Section 6.6.  Assuming the data is in 
#series “cntts”, the command to run the STL decomposition would be:

decomp_STL <- stl(cntts, s.window = 11, t.window = 7, robust = TRUE) 
rem_stl <- decomp_STL$time.series[,3]
MSE_stl <- mean((rem_stl)^2)
RMSE_stl <- MSE_stl^0.5
RMSE_stl #750.8186

################################################################################
#10. Compare all RMSEs computed.  Which one had the lowest RMSE?  
#Was the best procedure a forecasting procedure?  If not, which of the 
#forecasting procedures have the lowest RMSE?  Summarize, in a sentence or 
#two, the signals detected in the count data based on the work in this assignment.

table.all <- matrix(c(RMSE_reg1,
                       RMSE_reg2,
                       RMSE_rwf,
                       RMSE_snv,
                       RMSE.nvd.tsCV,
                       RMSE.snv.tsCV,
                       RMSE7,
                       RMSE7.2,
                       RMSE7.3,
                       RMSE_dc1_cA,
                       RMSE_dc1_cM,
                       RMSE_stl), 
                     ncol = 1, nrow = 12)
options(scipen = 100)
rownames(table.all) <- c("Regression (Month)",
                         "Regression (Season)",
                         "RWF",
                         "SN",
                         "RWF CV",
                         "SN CV",
                         "MA-5",
                         "MA-11",
                         "MA-17",
                         "Additive",
                         "Multiplicative",
                         "STL")
colnames(table.all) <- c("RMSE")
table.all

Acf(cntts)
Pacf(cntts)
#STL: Lowest RMSE of 750.81 
#Yes the best forecasting procedure was a forecasting procedure
#Yes, cntts displays seasonality and trend. 
#Autocorrelation also show there is nonrandomness in cntts
#Trend is the up and down trend while seasonality is the pattern
#We can see both in the cntts data as the number of rentals flutuactes
#from cold to warm months and there is a pattern across the year.