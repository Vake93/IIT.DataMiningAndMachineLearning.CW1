library(ggplot2)
library(prophet)

#Load CSV from Disk
exchangeRates<-read.csv("D:/Projects/ExchangeUSD.csv")

str(exchangeRates)

#Update the Date column to data type date
exchangeRates$Date <- as.Date(exchangeRates$Date, format = "%m/%d/%Y")
str(exchangeRates)
qplot(Date, Rate, data=exchangeRates)


ds <- exchangeRates$Date
y <- exchangeRates$Rate
df <- data.frame(ds, y)
qplot(ds, y, data=df)

# Creating a model with Facebook's prophet package
m <- prophet(df)

# Make 100 days predictions using the model
future <- make_future_dataframe(m, periods = 100)

forecast <- predict(m, future)

# Plot forecast
plot(m, forecast)
prophet_plot_components(m, forecast)


# Model performance
pred <- forecast$yhat[1:500]
actual <- m$history$y
plot(actual, pred)

#The ideal predictions lines
abline(lm(pred~actual), col = 'red')
summary(lm(pred~actual))

x <- cross_validation(m, horizon = 100, initial = 400, units = 'days')
performance_metrics(x, rolling_window = 0)

plot_cross_validation_metric(x,
                             metric = 'mae',
                             rolling_window = 0.1)

plot_cross_validation_metric(x,
                             metric = 'rmse',
                             rolling_window = 0.1)

plot_cross_validation_metric(x,
                             metric = 'mape',
                             rolling_window = 0.1)
