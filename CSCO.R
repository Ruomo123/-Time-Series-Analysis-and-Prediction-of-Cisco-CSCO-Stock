library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(ggplot2)
library(psych)
library(fBasics)
library(dplyr)
library(tidyr)
library(lubridate)
library(plyr)
library(lmtest)
library("xts")
library(timetk)
library(ggplot2)
library(fGarch)
library(aTSA)

# loading daily stock price data csv
CSCO_STOCKPRICE = read.csv('C:/Users/Yueru/Desktop/工作期间/文书/programming supplement/CSCO/CSCOxls.csv')
str(CSCO_STOCKPRICE)
View(CSCO_STOCKPRICE)
length(CSCO_STOCKPRICE$Date)
length(CSCO_STOCKPRICE$Adj.Close)

## plot the adjusted stock price
plot(as.Date(CSCO_STOCKPRICE$Date), CSCO_STOCKPRICE$Adj.Close,data = CSCO_STOCKPRICE)

describe(CSCO_STOCKPRICE$Adj.Close)

# Create data frame with date and adj.close price 
adj_closing_prices  <- CSCO_STOCKPRICE[, "Adj.Close", drop = F]
head(adj_closing_prices)
## Create a new data frame that contains the price data with the dates as the row names
CSCO_STOCKPRICE_prices <- CSCO_STOCKPRICE[, "Adj.Close", drop = FALSE]
rownames(CSCO_STOCKPRICE_prices) <- CSCO_STOCKPRICE$Date
head(CSCO_STOCKPRICE_prices)
tail(CSCO_STOCKPRICE_prices)
## Plotting the adj close price with timeseries
plot(CSCO_STOCKPRICE_prices$Adj.Close, type="l", col="blue", lwd=2, 
     ylab="Adjusted closing Price", main="Adj.Closing price of CSCO")

# Caculating the simple Return 

## Denote n the number of time periods:
n <- nrow(CSCO_STOCKPRICE_prices)
CSCO_STOCKPRICE_return <- ((CSCO_STOCKPRICE_prices[2:n, 1] - CSCO_STOCKPRICE_prices[1:(n-1), 1])/CSCO_STOCKPRICE_prices[1:(n-1), 1])
# Now add dates as names to the vector 
names(CSCO_STOCKPRICE_return) <- CSCO_STOCKPRICE[2:n, 1]
head(CSCO_STOCKPRICE_return)
# Plot the simple returns of CSCO
plot(CSCO_STOCKPRICE_return, type = "l", col = "blue", lwd = 2, ylab = "Simple Return",
     main = "Daily Returns on CSCO")

describe(CSCO_STOCKPRICE_return)

hist(CSCO_STOCKPRICE_return, nclass = 200)

# ACF and PACF Criterion for Adj.close price
acf(CSCO_STOCKPRICE$Adj.Close, lag.max = 50, plot = TRUE)
pacf(CSCO_STOCKPRICE$Adj.Close, lag.max = 50, plot = TRUE)

timeseries <- ts(CSCO_STOCKPRICE$Adj.Close, frequency = 50)
timeseriescomponents <- decompose(timeseries)
plot(timeseriescomponents)


#days of the week effect
date <- as.Date(CSCO_STOCKPRICE$Date, format = "%Y/%m/%d")

CSCO_STOCKPRICE$WeekDay <- weekdays(date)

Mon<- filter(CSCO_STOCKPRICE,WeekDay=='Monday')
Tue<- filter(CSCO_STOCKPRICE,WeekDay=='Tuesday')
Wed<- filter(CSCO_STOCKPRICE,WeekDay=='Wednesday')
Thr<- filter(CSCO_STOCKPRICE,WeekDay=='Thursday')
Fri<- filter(CSCO_STOCKPRICE,WeekDay=='Friday')

CSCO_STOCKPRICE$week <- format(date, format="%Y-%U")
li<-ddply(CSCO_STOCKPRICE, CSCO_STOCKPRICE$week, summarize, price=sum(CSCO_STOCKPRICE_prices))
fit<-lm(unlist(li[20:640])~Mon$Adj.Close[20:640]+Tue$Adj.Close[20:640]+Wed$Adj.Close[20:640]+Thr$Adj.Close[20:640]+Fri$Adj.Close[20:640])
summary(fit)
anova(fit)
# Computing the log returns for the CSCO
log_data = log(CSCO_STOCKPRICE$Adj.Close)
log_ret = diff(log_data, differences = 1)

#Data Cleaning
na.omit(log_ret)
CSCO_log_ret <- na.omit(log_ret)

# Plot log returns 
plot(CSCO_log_ret,type='l', main='log returns plot')
print(adf.test(CSCO_log_ret))

# Determine the Order in ARIMA model
pacf(CSCO_log_ret, lag.max = 20, main = "PACF on CSCO_Log Return")
acf(CSCO_log_ret, lag.max = 20, main = "ACF on CSCO_Log Return")

# Conduct ADF test on log returns series
print(adf.test(CSCO_log_ret))
count_d1 = diff(CSCO_STOCKPRICE$Adj.Close, differences = 1)
plot(count_d1)
ndiffs(log_ret)
hist(log_ret, nclass = 200)

# Split the dataset in two parts - training and testing
CSCO_log_ret_train = CSCO_log_ret[1:3000]
length(CSCO_log_ret_train)
CSCO_log_ret_test = CSCO_log_ret[3001:3525]
length(CSCO_log_ret_test)

# AR1 Model1
# (Summary of the ARIMA model using the determined (p,d,q) parameters)
model1 = arima(CSCO_log_ret_train, order = c(1, 1, 0),include.mean=FALSE)
summary(model1)
tsdisplay(model1$residuals)


#AR1 Model2 
# (graph shows serious lag at 19, so modify model for p or q = 19)
model2 = arima(CSCO_log_ret_train, order = c(19, 1, 1),include.mean=FALSE)
summary(model2)
tsdisplay(model2$residuals, lag.max = 30)
# forcast new arima model for h = 30 period (30 days)
model_forecast = forecast::forecast(model2,h = 1,level=99)
model_forecast
head(model_forecast)
# dwtest(model_forecast)
Box.test(model2$residuals, lag = 2, type = 'Ljung-Box')

# Creating a series of forecasted returns for the forecasted period
arima.forecast = forecast::forecast(model2, h = 1,level=99)
forecasted_series = data.frame(Forecasted = numeric())
forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
colnames(forecasted_series) = c("Forecasted")

fit = Arima(CSCO_STOCKPRICE$Adj.Close[1:3000], order = c(19,1,1), 
            include.drift = T)
future = forecast::forecast(fit, h = 419,level=99)
plot(future,main='Forecast for CSCO')

# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date("2005-12-23","%Y-%m-%d"))
dates <- seq(as.Date("2005-12-23"), length = 522, by = "days")

for (b in 3000:(nrow(CSCO_STOCKPRICE)-1)) {
  
  CSCO_stockprice_train = CSCO_STOCKPRICE[1:b, ]
  CSCO_STOCKPRICE_test = CSCO_STOCKPRICE[(b+1):nrow(CSCO_STOCKPRICE),] }



# Creating a series of actual returns for the forecasted period
Actual_return = CSCO_STOCKPRICE[3001:3522,6]
Actual_series = c(Actual_return,xts(Actual_return,order.by = dates))
rm(Actual_return)
print(CSCO_STOCKPRICE_prices[3001,])
print(CSCO_STOCKPRICE_prices[3002,])

# Adjust the length of the Actual return series
Actual_series = CSCO_STOCKPRICE[-1]
# Create a time series object of the forecasted series
dates2 <- seq(from=as.Date("2017-11-24"),to=as.Date("2019-4-30"), length = 522,order.by="dates")
#arima.forecast=forecast::forecast(CSCO_STOCKPRICE$Adj.Close[1:3000],h=419,level=99)

arima.forecast=data.frame(arima.forecast)

m=arima(CSCO_STOCKPRICE$Adj.Close[1:3000],order=c(1,1,0))
f=forecast::forecast(m,h=419,level=99)
plot(f,col='red')


#forecasted_series = xts(arima.forecast$Lo.99,order.by=dates)
forecasted_series =arima.forecast$Lo.99

# Create a plot of the two return series - Actual versus Forecasted
plot(Actual_series$Adj.Close,type="l",main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series,lwd=1.5,col='red')
legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))
# Create a table for the accuracy of the forecast
comparsion = merge(Actual_series,forecasted_series)
comparsion$Accuracy ==sign(comparsion$Volume)
print(comparsion)
# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)

accuracy(model2,x = CSCO_log_ret_test)
plot(model2$residuals, model_forecast[-1])
return_forecast= ts(model_forecast)
return_forecast

arch.test(model2, output = TRUE)
hist(model2$residuals)

#residual plot
fit <- lm(CSCO_log_ret_train ~ (0.07085*CSCO_log_ret_train), data = CSCO_STOCKPRICE)  # Fit the model
summary(fit)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit)
par(mfrow = c(1, 1))
