# Problem Set: Time Series and Spatial Analysis
# Question Number 2
# Status: Completed


# Load packages
library(TTR)
library(forecast)

# 2. Read the temp.txt file. The data correspond to monthly average temperatures.
temp<-read.table("R08_timeseries_spatialanalysis/temp.txt", header=T)

# a) Plot the time series data. [Hint: first you need to create a monthly time series object]
temp_ts<-ts(temp$temps, frequency=12) 
plot(temp_ts)

# b) Calculate the 5-point moving average and plot it together with the time series
temp_ma<-SMA(temp_ts, n = 5)
lines(temp_ma, col="red", lwd=2)

# c) Decompose the time series into seasonal, trend and residual error components
temp_decomp<-decompose(temp_ts)
plot(temp_decomp)

# d) Generate a temporal correlogram to assess the autocorrelation of the time series
acf(temp_ts)

# e) Generate a new correlogram but removing the trend and seasonal variation
acf(temp_decomp$random[!is.na(temp_decomp$random)])

# f) Find the best ARIMA model using the forecast package
temp_fit<-auto.arima(temp_ts)
summary(temp_fit)
checkresiduals(temp_fit)

# g) Estimate future values using the previous ARIMA model and plot the results
temp_fcast<-forecast(temp_fit)
plot(temp_fcast)









