#setwd("~/Documents/IIIT/Case-Study/Global_Mart")
#install.packages("dplyr")
library(graphics)
library(forecast)
library(dplyr)
library(tseries)
global_mart <- read.csv("Global Superstore.csv", stringsAsFactors = FALSE )
#View(global_mart)

#Check for NA values.
colnames(global_mart)[colSums(is.na(global_mart)) > 0]

#ONLY Postal Code has NA values
#but we can live with it

#Check the unique values of market and segment
unique(global_mart$Segment)
unique(global_mart$Market)

#Convert Order dates to month and year. 
global_mart$Order.Date <- as.Date(as.POSIXlt(global_mart$Order.Date,format = "%d-%m-%Y"))
global_mart$Order.Date.MY <- format(global_mart$Order.Date,"%Y%m")

#Create subset of data as per each market and segment
global_mart_US_Consumer <- subset(global_mart,Market=='US' & Segment=='Consumer')
global_mart_US_Corporate <- subset(global_mart,Market=='US' & Segment=='Corporate')
global_mart_US_HomeOffice <- subset(global_mart,Market=='US' & Segment=='Home Office')

global_mart_APAC_Consumer <- subset(global_mart,Market=='APAC' & Segment=='Consumer')
global_mart_APAC_Corporate <- subset(global_mart,Market=='APAC' & Segment=='Corporate')
global_mart_APAC_HomeOffice <- subset(global_mart,Market=='APAC' & Segment=='Home Office')

global_mart_EU_Consumer <- subset(global_mart,Market=='EU' & Segment=='Consumer')
global_mart_EU_Corporate <- subset(global_mart,Market=='EU' & Segment=='Corporate')
global_mart_EU_HomeOffice <- subset(global_mart,Market=='EU' & Segment=='Home Office')

global_mart_Africa_Consumer <- subset(global_mart,Market=='Africa' & Segment=='Consumer')
global_mart_Africa_Corporate <- subset(global_mart,Market=='Africa' & Segment=='Corporate')
global_mart_Africa_HomeOffice <- subset(global_mart,Market=='Africa' & Segment=='Home Office')

global_mart_EMEA_Consumer <- subset(global_mart,Market=='EMEA' & Segment=='Consumer')
global_mart_EMEA_Corporate <- subset(global_mart,Market=='EMEA' & Segment=='Corporate')
global_mart_EMEA_HomeOffice <- subset(global_mart,Market=='EMEA' & Segment=='Home Office')

global_mart_LATAM_Consumer <- subset(global_mart,Market=='LATAM' & Segment=='Consumer')
global_mart_LATAM_Corporate <- subset(global_mart,Market=='LATAM' & Segment=='Corporate')
global_mart_LATAM_HomeOffice <- subset(global_mart,Market=='LATAM' & Segment=='Home Office')

global_mart_Canada_Consumer <- subset(global_mart,Market=='Canada' & Segment=='Consumer')
global_mart_Canada_Corporate <- subset(global_mart,Market=='Canada' & Segment=='Corporate')
global_mart_Canada_HomeOffice <- subset(global_mart,Market=='Canada' & Segment=='Home Office')

#Group by Order.Date
#Function to aggregate values
agg_val <- function(name_var) {
  name_var %>% group_by(name_var$Order.Date.MY) %>% summarize_at(c(19,20,22),sum)
}

#Aggregating for each bucket
agg_mart_US_Consumer <- agg_val(global_mart_US_Consumer)
agg_mart_US_Corporate <- agg_val(global_mart_US_Corporate)
agg_mart_US_HomeOffice <- agg_val(global_mart_US_HomeOffice)

agg_mart_APAC_Consumer <- agg_val(global_mart_APAC_Consumer)
agg_mart_APAC_Corporate <- agg_val(global_mart_APAC_Corporate)
agg_mart_APAC_HomeOffice <- agg_val(global_mart_APAC_HomeOffice)

agg_mart_EU_Consumer <- agg_val(global_mart_EU_Consumer)
agg_mart_EU_Corporate <- agg_val(global_mart_EU_Corporate)
agg_mart_EU_HomeOffice <- agg_val(global_mart_EU_HomeOffice)

agg_mart_Africa_Consumer <- agg_val(global_mart_Africa_Consumer)
agg_mart_Africa_Corporate <- agg_val(global_mart_Africa_Corporate)
agg_mart_Africa_HomeOffice <- agg_val(global_mart_Africa_HomeOffice)

agg_mart_EMEA_Consumer <- agg_val(global_mart_EMEA_Consumer)
agg_mart_EMEA_Corporate <- agg_val(global_mart_EMEA_Corporate)
agg_mart_EMEA_HomeOffice <- agg_val(global_mart_EMEA_HomeOffice)

agg_mart_LATAM_Consumer <- agg_val(global_mart_LATAM_Consumer)
agg_mart_LATAM_Corporate <- agg_val(global_mart_LATAM_Corporate)
agg_mart_LATAM_HomeOffice <- agg_val(global_mart_LATAM_HomeOffice)

agg_mart_Canada_Consumer <- agg_val(global_mart_Canada_Consumer)
agg_mart_Canada_Corporate <- agg_val(global_mart_Canada_Corporate)
agg_mart_Canada_HomeOffice <- agg_val(global_mart_Canada_HomeOffice)

#Create time series for Sales, Quantity & Profit for each of the parameters
#Profit time series
ts_US_Consumer_Profit <- ts(agg_mart_US_Consumer$Profit)
ts_US_Corporate_Profit <- ts(agg_mart_US_Corporate$Profit)
ts_US_HomeOffice_Profit <- ts(agg_mart_US_HomeOffice$Profit)

ts_APAC_Consumer_Profit <- ts(agg_mart_APAC_Consumer$Profit)
ts_APAC_Corporate_Profit <- ts(agg_mart_APAC_Corporate$Profit)
ts_APAC_HomeOffice_Profit <- ts(agg_mart_APAC_HomeOffice$Profit)

ts_EU_Consumer_Profit <- ts(agg_mart_EU_Consumer$Profit)
ts_EU_Corporate_Profit <- ts(agg_mart_EU_Corporate$Profit)
ts_EU_HomeOffice_Profit <- ts(agg_mart_EU_HomeOffice$Profit)

ts_Africa_Consumer_Profit <- ts(agg_mart_Africa_Consumer$Profit)
ts_Africa_Corporate_Profit <- ts(agg_mart_Africa_Corporate$Profit)
ts_Africa_HomeOffice_Profit <- ts(agg_mart_Africa_HomeOffice$Profit)

ts_EMEA_Consumer_Profit <- ts(agg_mart_EMEA_Consumer$Profit)
ts_EMEA_Corporate_Profit <- ts(agg_mart_EMEA_Corporate$Profit)
ts_EMEA_HomeOffice_Profit <- ts(agg_mart_EMEA_HomeOffice$Profit)

ts_LATAM_Consumer_Profit <- ts(agg_mart_LATAM_Consumer$Profit)
ts_LATAM_Corporate_Profit <- ts(agg_mart_LATAM_Corporate$Profit)
ts_LATAM_HomeOffice_Profit <- ts(agg_mart_LATAM_HomeOffice$Profit)

ts_Canada_Consumer_Profit <- ts(agg_mart_Canada_Consumer$Profit)
ts_Canada_Corporate_Profit <- ts(agg_mart_Canada_Corporate$Profit)
ts_Canada_HomeOffice_Profit <- ts(agg_mart_Canada_HomeOffice$Profit)

#Time series for Sales
ts_US_Consumer_Sales <- ts(agg_mart_US_Consumer$Sales)
ts_US_Corporate_Sales <- ts(agg_mart_US_Corporate$Sales)
ts_US_HomeOffice_Sales <- ts(agg_mart_US_HomeOffice$Sales)

ts_APAC_Consumer_Sales <- ts(agg_mart_APAC_Consumer$Sales)
ts_APAC_Corporate_Sales <- ts(agg_mart_APAC_Corporate$Sales)
ts_APAC_HomeOffice_Sales <- ts(agg_mart_APAC_HomeOffice$Sales)

ts_EU_Consumer_Sales <- ts(agg_mart_EU_Consumer$Sales)
ts_EU_Corporate_Sales <- ts(agg_mart_EU_Corporate$Sales)
ts_EU_HomeOffice_Sales <- ts(agg_mart_EU_HomeOffice$Sales)

ts_Africa_Consumer_Sales <- ts(agg_mart_Africa_Consumer$Sales)
ts_Africa_Corporate_Sales <- ts(agg_mart_Africa_Corporate$Sales)
ts_Africa_HomeOffice_Sales <- ts(agg_mart_Africa_HomeOffice$Sales)

ts_EMEA_Consumer_Sales <- ts(agg_mart_EMEA_Consumer$Sales)
ts_EMEA_Corporate_Sales <- ts(agg_mart_EMEA_Corporate$Sales)
ts_EMEA_HomeOffice_Sales <- ts(agg_mart_EMEA_HomeOffice$Sales)

ts_LATAM_Consumer_Sales <- ts(agg_mart_LATAM_Consumer$Sales)
ts_LATAM_Corporate_Sales <- ts(agg_mart_LATAM_Corporate$Sales)
ts_LATAM_HomeOffice_Sales <- ts(agg_mart_LATAM_HomeOffice$Sales)

ts_Canada_Consumer_Sales <- ts(agg_mart_Canada_Consumer$Sales)
ts_Canada_Corporate_Sales <- ts(agg_mart_Canada_Corporate$Sales)
ts_Canada_HomeOffice_Sales <- ts(agg_mart_Canada_HomeOffice$Sales)

#Time series for Quantity
ts_US_Consumer_Quantity <- ts(agg_mart_US_Consumer$Quantity)
ts_US_Corporate_Quantity <- ts(agg_mart_US_Corporate$Quantity)
ts_US_HomeOffice_Quantity <- ts(agg_mart_US_HomeOffice$Quantity)

ts_APAC_Consumer_Quantity <- ts(agg_mart_APAC_Consumer$Quantity)
ts_APAC_Corporate_Quantity <- ts(agg_mart_APAC_Corporate$Quantity)
ts_APAC_HomeOffice_Quantity <- ts(agg_mart_APAC_HomeOffice$Quantity)

ts_EU_Consumer_Quantity <- ts(agg_mart_EU_Consumer$Quantity)
ts_EU_Corporate_Quantity <- ts(agg_mart_EU_Corporate$Quantity)
ts_EU_HomeOffice_Quantity <- ts(agg_mart_EU_HomeOffice$Quantity)

ts_Africa_Consumer_Quantity <- ts(agg_mart_Africa_Consumer$Quantity)
ts_Africa_Corporate_Quantity <- ts(agg_mart_Africa_Corporate$Quantity)
ts_Africa_HomeOffice_Quantity <- ts(agg_mart_Africa_HomeOffice$Quantity)

ts_EMEA_Consumer_Quantity <- ts(agg_mart_EMEA_Consumer$Quantity)
ts_EMEA_Corporate_Quantity <- ts(agg_mart_EMEA_Corporate$Quantity)
ts_EMEA_HomeOffice_Quantity <- ts(agg_mart_EMEA_HomeOffice$Quantity)

ts_LATAM_Consumer_Quantity <- ts(agg_mart_LATAM_Consumer$Quantity)
ts_LATAM_Corporate_Quantity <- ts(agg_mart_LATAM_Corporate$Quantity)
ts_LATAM_HomeOffice_Quantity <- ts(agg_mart_LATAM_HomeOffice$Quantity)

ts_Canada_Consumer_Quantity <- ts(agg_mart_Canada_Consumer$Quantity)
ts_Canada_Corporate_Quantity <- ts(agg_mart_Canada_Corporate$Quantity)
ts_Canada_HomeOffice_Quantity <- ts(agg_mart_Canada_HomeOffice$Quantity)

#Find coefficient of variation for Profits of all the 21 buckets
#Store these values in named vector 
bucket_array <- c("US_Consumer"=cv(ts_US_Consumer_Profit),"US_Corporate"=cv(ts_US_Corporate_Profit),"US_HomeOffice"=cv(ts_US_HomeOffice_Profit),
                  "APAC_Consumer"=cv(ts_APAC_Consumer_Profit),"APAC_Corporate"=cv(ts_APAC_Corporate_Profit),"APAC_HomeOffice"=cv(ts_APAC_HomeOffice_Profit),
                  "EU_Consumer"=cv(ts_EU_Consumer_Profit),"EU_Corporate"=cv(ts_EU_Corporate_Profit),"EU_HomeOffice"=cv(ts_EU_HomeOffice_Profit),
                  "Africa_Consumer"=cv(ts_Africa_Consumer_Profit),"Africa_Corporate"=cv(ts_Africa_Corporate_Profit),"Africa_HomeOffice"=cv(ts_Africa_HomeOffice_Profit),
                  "EMEA_Consumer"=cv(ts_EMEA_Consumer_Profit),"EMEA_Corporate"=cv(ts_EMEA_Corporate_Profit),"EMEA_HomeOffice"=cv(ts_EMEA_HomeOffice_Profit),
                  "LATAM_Consumer"=cv(ts_LATAM_Consumer_Profit),"LATAM_Corporate"=cv(ts_LATAM_Corporate_Profit),"LATAM_HomeOffice"=cv(ts_LATAM_HomeOffice_Profit),
                  "Canada_Consumer"=cv(ts_Canada_Consumer_Profit),"Canada_Corporate"=cv(ts_Canada_Corporate_Profit),"Canada_HomeOffice"=cv(ts_Canada_HomeOffice_Profit))

#Get the lowest two cv values as we need to find most consistent performers
bucket_array <- sort(bucket_array)
bucket_array[1:2]

#
#Segments finalised are EU-Consumer(0.62) and APAC-Consumer(0.63)
#

############# Prediction for EU_Consumer Sales #################
#Data preparation will continue, where we divide data into train and test segements
#We must sort the data by Months before getting training and test data
agg_mart_EU_Consumer$`name_var$Order.Date.MY` <- as.numeric(agg_mart_EU_Consumer$`name_var$Order.Date.MY`)
agg_mart_EU_Consumer <- agg_mart_EU_Consumer[order(agg_mart_EU_Consumer$`name_var$Order.Date.MY`),]

#Select first 42row for model building and remaining 6 for testing
indata <- agg_mart_EU_Consumer[1:42,]
timesvals_in <- indata$`name_var$Order.Date.MY`

#Create fresh time series on train data
ts_EU_Consumer_Sales <- ts(indata$Sales)

#Smoothen the time series
# Forecast Sales
plot(ts_EU_Consumer_Sales)
#Smoothening time series using Moving Average Smoothing
w <-4
smoothedseries <- stats::filter(ts_EU_Consumer_Sales, 
                         filter=rep(1/(1*w+1),(1*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(ts_EU_Consumer_Sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="blue", lwd=2)

## Holtwinters method
plot(ts_EU_Consumer_Sales)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.25,0.3,0.4,0.5)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) 
{
  smoothed_series <- HoltWinters(ts_EU_Consumer_Sales, alpha=alphas[i],
                                     beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothed_series)[,1], col=cols[i], lwd=2)
}
#Selecting 0.5 as the alpha
smoothed_series <- HoltWinters(ts_EU_Consumer_Sales, alpha=alphas[4],
                               beta=FALSE, gamma=FALSE)
smoothedseries1 <- (fitted(smoothed_series)[,1])

#Create smoothed data frame
smootheddf <- as.data.frame(cbind(timesvals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Build Multiplicative Prediction model
lmfit <- lm(Sales ~ Month, data=smootheddf)
# lmfit_s <- lm(Sales ~ sin(0.6*Month) * poly(Month,2) + cos(0.6*Month) * poly(Month,2)
#                           + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)

lines(timesvals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- ts_EU_Consumer_Sales-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
outdata <- agg_mart_EU_Consumer[43:48,]
timevals_out <- outdata$`name_var$Order.Date.MY`
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,outdata$Sales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(agg_mart_EU_Consumer$Sales)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

##Auto Arima
autoarima <- auto.arima(ts_EU_Consumer_Sales)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- ts_EU_Consumer_Sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Sales)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

## Auto Arima model is better, we will choose it over Decomposition

############# Prediction for EU_Consumer Quantity #################
#Data preparation will continue, where we divide data into train and test segements
#We must sort the data by Months before getting training and test data
agg_mart_EU_Consumer$`name_var$Order.Date.MY` <- as.numeric(agg_mart_EU_Consumer$`name_var$Order.Date.MY`)
agg_mart_EU_Consumer <- agg_mart_EU_Consumer[order(agg_mart_EU_Consumer$`name_var$Order.Date.MY`),]

#Select first 42row for model building and remaining 6 for testing
indata <- agg_mart_EU_Consumer[1:42,]
timesvals_in <- indata$`name_var$Order.Date.MY`

#Create fresh time series on train data
ts_EU_Consumer_Quantity <- ts(indata$Quantity)

#Smoothen the time series
# Forecast Sales
plot(ts_EU_Consumer_Quantity)
#Smoothening time series using Moving Average Smoothing
w <-1
smoothedseries <- stats::filter(ts_EU_Consumer_Quantity, 
                                filter=rep(1/(1*w+1),(1*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(ts_EU_Consumer_Quantity)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="blue", lwd=2)

## Holtwinters method
plot(ts_EU_Consumer_Quantity)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2,0.3,0.4,0.5)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) 
{
  smoothed_series <- HoltWinters(ts_EU_Consumer_Quantity, alpha=alphas[i],
                                 beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothed_series)[,1], col=cols[i], lwd=2)
}
#selecting 0.4 as alpha
smoothed_series <- HoltWinters(ts_EU_Consumer_Quantity, alpha=alphas[3],
                               beta=FALSE, gamma=FALSE)
smoothedseries1 <- (fitted(smoothed_series)[,1])

#Create smoothed data frame
smootheddf <- as.data.frame(cbind(timesvals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Build Prediction model
#lmfit <- lm(Sales ~ Month, data=smootheddf)
lmfit_s <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                           + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timesvals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- ts_EU_Consumer_Quantity-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
outdata <- agg_mart_EU_Consumer[43:48,]
timevals_out <- outdata$`name_var$Order.Date.MY`
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,outdata$Sales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(agg_mart_EU_Consumer$Quantity)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


##Auto Arima
autoarima <- auto.arima(ts_EU_Consumer_Quantity)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- ts_EU_Consumer_Quantity - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Quantity)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

## Auto Arima model is better, we will choose it over Decomposition

############# Prediction for APAC_Consumer Sales #################
#Data preparation will continue, where we divide data into train and test segements
#We must sort the data by Months before getting training and test data
agg_mart_APAC_Consumer$`name_var$Order.Date.MY` <- as.numeric(agg_mart_APAC_Consumer$`name_var$Order.Date.MY`)
agg_mart_APAC_Consumer <- agg_mart_APAC_Consumer[order(agg_mart_APAC_Consumer$`name_var$Order.Date.MY`),]

#Select first 42row for model building and remaining 6 for testing
indata <- agg_mart_APAC_Consumer[1:42,]
timesvals_in <- indata$`name_var$Order.Date.MY`

#Create fresh time series on train data
ts_APAC_Consumer_Sales <- ts(indata$Sales)

#Smoothen the time series
# Forecast Sales
plot(ts_APAC_Consumer_Sales)
#Smoothening time series using Moving Average Smoothing
w <-2
smoothedseries <- stats::filter(ts_APAC_Consumer_Sales, 
                                filter=rep(1/(1*w),(1*w)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(ts_APAC_Consumer_Sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="red", lwd=2)

## Holtwinters method
plot(ts_APAC_Consumer_Sales)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.3,0.4,0.5,0.6)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) 
{
  smoothed_series <- HoltWinters(ts_APAC_Consumer_Sales, alpha=alphas[i],
                                 beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothed_series)[,1], col=cols[i], lwd=2)
}
#Selecting 0.5 as the alpha
smoothed_series <- HoltWinters(ts_APAC_Consumer_Sales, alpha=alphas[3],
                               beta=FALSE, gamma=FALSE)
smoothedseries1 <- (fitted(smoothed_series)[,1])

#Create smoothed data frame
smootheddf <- as.data.frame(cbind(timesvals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Build Prediction model
lmfit <- lm(Sales ~ Month, data=smootheddf)
# lmfit_s <- lm(Sales ~ sin(0.7*Month) * poly(Month,3) + cos(0.2*Month) * poly(Month,3)
#                           + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timesvals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- ts_APAC_Consumer_Sales-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
outdata <- agg_mart_APAC_Consumer[43:48,]
timevals_out <- outdata$`name_var$Order.Date.MY`
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,outdata$Sales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(agg_mart_APAC_Consumer$Sales)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


##Auto Arima
autoarima_APAC_C_S <- auto.arima(ts_APAC_Consumer_Sales)
autoarima_APAC_C_S
tsdiag(autoarima_APAC_C_S)
plot(autoarima_APAC_C_S$x, col="black")
lines(fitted(autoarima_APAC_C_S), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- ts_APAC_Consumer_Sales - fitted(autoarima_APAC_C_S)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima_APAC_C_S, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Sales)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima_APAC_C_S),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

## Auto Arima model is better, we will choose it over Decomposition

############# Prediction for APAC_Consumer Quantity #################
#Data preparation will continue, where we divide data into train and test segements
#We must sort the data by Months before getting training and test data

agg_mart_APAC_Consumer$`name_var$Order.Date.MY` <- as.numeric(agg_mart_APAC_Consumer$`name_var$Order.Date.MY`)
agg_mart_APAC_Consumer <- agg_mart_APAC_Consumer[order(agg_mart_APAC_Consumer$`name_var$Order.Date.MY`),]

#Select first 42row for model building and remaining 6 for testing
indata <- agg_mart_APAC_Consumer[1:42,]
timesvals_in <- indata$`name_var$Order.Date.MY`

#Create fresh time series on train data
ts_APAC_Consumer_Quantity <- ts(indata$Quantity)

#Smoothen the time series
# Forecast Quantity
plot(ts_APAC_Consumer_Quantity)
#Smoothening time series using Moving Average Smoothing
w <-3
smoothedseries <- stats::filter(ts_APAC_Consumer_Quantity, 
                                filter=rep(1/(1*w+1),(1*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
n <- length(ts_APAC_Consumer_Quantity)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="blue", lwd=2)

## Holtwinters method
plot(ts_APAC_Consumer_Quantity)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.3,0.4,0.5)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) 
{
  smoothed_series <- HoltWinters(ts_APAC_Consumer_Quantity, alpha=alphas[i],
                                 beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothed_series)[,1], col=cols[i], lwd=2)
}
#Selecting 0.5 as alpha
smoothed_series <- HoltWinters(ts_APAC_Consumer_Quantity, alpha=alphas[3],
                               beta=FALSE, gamma=FALSE)
smoothedseries1 <- (fitted(smoothed_series)[,1])

#Create smoothed data frame
smootheddf <- as.data.frame(cbind(timesvals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Build Prediction model
lmfit <- lm(Quantity ~ Month, data=smootheddf)
# lmfit_s <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
#                            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timesvals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- ts_APAC_Consumer_Quantity-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
outdata <- agg_mart_APAC_Consumer[43:48,]
timevals_out <- outdata$`name_var$Order.Date.MY`
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,outdata$Quantity)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(agg_mart_APAC_Consumer$Quantity)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

##Auto Arima
autoarima_APAC_C_Q <- auto.arima(ts_APAC_Consumer_Quantity)
autoarima_APAC_C_Q
tsdiag(autoarima_APAC_C_Q)
plot(autoarima_APAC_C_Q$x, col="black")
lines(fitted(autoarima_APAC_C_Q), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- ts_APAC_Consumer_Quantity - fitted(autoarima_APAC_C_Q)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima_APAC_C_Q, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata$Quantity)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima_APAC_C_Q),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

## Auto Arima model is better, we will choose it over Decomposition

#################################################################
########### Prediction for future 6 months ######################
#################################################################
#Using auto-arima function to predict data for next 6 months
#Prediction for EU Consumer Sales
autoarima_EU_CS <- auto.arima(ts(agg_mart_EU_Consumer$Sales))
plot(autoarima_EU_CS$x, col="black")
fcast_auto_arima <- predict(autoarima_EU_CS, n.ahead = 6)
auto_arima_pred_EU_CS <- c(fitted(autoarima_EU_CS),ts(fcast_auto_arima$pred))
plot(auto_arima_pred_EU_CS, col = "red", type = "l")
lines(auto_arima_pred_EU_CS[1:48], col = "black")
sum(auto_arima_pred_EU_CS[49:54])
mean(auto_arima_pred_EU_CS[49:54])

#Prediction for EU Consumer Sales
autoarima_EU_CS <- auto.arima(ts(agg_mart_EU_Consumer$Sales))
plot(autoarima_EU_CS$x, col="black")
fcast_auto_arima <- predict(autoarima_EU_CS, n.ahead = 6)
auto_arima_pred_EU_CS <- c(fitted(autoarima_EU_CS),ts(fcast_auto_arima$pred))
plot(auto_arima_pred_EU_CS, col = "red", type = "l")
lines(auto_arima_pred_EU_CS[1:48], col = "black")
auto_arima_pred_EU_CS[49:54]
sum(auto_arima_pred_EU_CS[49:54])
mean(auto_arima_pred_EU_CS[49:54])

#Prediction for EU Consumer Quantity
autoarima_EU_CQ <- auto.arima(ts(agg_mart_EU_Consumer$Quantity))
plot(autoarima_EU_CQ$x, col="black")
fcast_auto_arima <- predict(autoarima_EU_CQ, n.ahead = 6)
auto_arima_pred_EU_CQ <- c(fitted(autoarima_EU_CQ),ts(fcast_auto_arima$pred))
plot(auto_arima_pred_EU_CQ, col = "red", type = "l")
lines(auto_arima_pred_EU_CQ[1:48], col = "black")
auto_arima_pred_EU_CQ[49:54]
sum(auto_arima_pred_EU_CQ[49:54])
mean(auto_arima_pred_EU_CQ[49:54])

#Prediction for APAC Consumer Sales
autoarima_APAC_CS <- auto.arima(ts(agg_mart_APAC_Consumer$Sales))
plot(autoarima_APAC_CS$x, col="black")
fcast_auto_arima <- predict(autoarima_APAC_CS, n.ahead = 6)
auto_arima_pred_APAC_CS <- c(fitted(autoarima_APAC_CS),ts(fcast_auto_arima$pred))
plot(auto_arima_pred_APAC_CS, col = "red", type = "l")
lines(auto_arima_pred_APAC_CS[1:48], col = "black")
auto_arima_pred_APAC_CS[49:54]
sum(auto_arima_pred_APAC_CS[49:54])
mean(auto_arima_pred_APAC_CS[49:54])

#Prediction for APAC Consumer Quantity
autoarima_APAC_CQ <- auto.arima(ts(agg_mart_APAC_Consumer$Quantity))
plot(autoarima_APAC_CQ$x, col="black")
fcast_auto_arima <- predict(autoarima_APAC_CQ, n.ahead = 6)
auto_arima_pred_APAC_CQ <- c(fitted(autoarima_APAC_CQ),ts(fcast_auto_arima$pred))
plot(auto_arima_pred_APAC_CQ, col = "red", type = "l")
lines(auto_arima_pred_APAC_CQ[1:48], col = "black")
auto_arima_pred_APAC_CQ[49:54]
sum(auto_arima_pred_APAC_CQ[49:54])
mean(auto_arima_pred_APAC_CQ[49:54])
