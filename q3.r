library('e1071')

exchangeRates<-read.csv("D:/Projects/ExchangeUSD.csv")
str(exchangeRates)
exchangeRates$Date <- as.Date(exchangeRates$Date, format = "%m/%d/%Y")

Dates <- exchangeRates$Date
Rates <- exchangeRates$Rate
df <- data.frame(Dates, Rates)

model <- svm(Rates~Dates, df)

prediction <- predict(model, data = df$Dates)

plot(
  y = df$Rates,
  x = df$Dates)

points(
  y = prediction,
  x = df$Dates,
  col = 'blue',
  pch = 4)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

rmse(model$residuals)
summary(model)

svm_tune <- tune(svm, Rates~Dates, data = df, ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9)))
print(svm_tune)


best_model <- svm_tune$best.model
best_prediction <- predict(best_model, data = df$Dates)
rmse(best_model$residuals)

points(
  y = best_prediction,
  x = df$Dates,
  col = 'green',
  pch = 4)

plot(svm_tune)
