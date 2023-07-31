Alc <- read.csv("AlcSales.csv")
attach(Alc)

library(lattice)
library(dplyr)
library(ggplot2)
Alc$Sales <- Sales/1000
Alc$DATE <- as.Date(DATE, format = "%m/%d/%y")
Alc_New <- Alc %>% filter(DATE>'2016-12-01')
detach(Alc)
attach(Alc_New)
testdata <- Alc_New %>% filter(DATE>'2022-02-01')

ggplot(data = Alc_New, aes(x = DATE, y = Sales)) +
  geom_line(color = "purple") +
  labs(x = "Date",
       y = "Total Sales (Millions of Dollars)",
       title = "Monthly Alcohol Sales Since 2017",
       subtitle = "United States", type = 'l')

#Steadily increasing trend with slight ups and downs

ggplot(Alc_New) +
  aes(x = "", y = Sales) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

boxplot.stats(Alc$Sales)$out

#No Outliers


library(tseries)
Sales_ts <- ts(Sales, frequency = 12)
plot(decompose(Sales_ts))
#seaonal trend


adf.test(Sales_ts)
#not stationary
diff_Sales <- diff(diff((Sales_ts),12))
adf.test(diff_Sales)
plot(diff_Sales, main = "Differenced Data")
#data now stationary

#Auto Arima
library(forecast)
fit1 = auto.arima(Sales_ts)
fit1

plot(forecast(fit1))
F1 <- forecast(fit1, h=12, level = .95)
F1

#Manual Arima
par(mfrow=c(1,1))
acf(diff_Sales)
pacf(diff_Sales)

fit2=Arima(Sales_ts,order=c(1,0,15),
           seasonal=list(order=c(1,0,0),period=12))
fit2
F2 <- forecast(fit2, h = 12, level = 0.95)
F2
plot(forecast(fit2))

#HW Forecast
dfts <- ts(Sales_ts, frequency = 12)
HW1 <- HoltWinters(dfts)
HW1_for <- forecast(HW1, h=12, level=.95)
plot(HW1_for)
lines(HW1_for$fitted, lty=2, col="red")

library(Metrics)
library(MLmetrics)
#F1 Values
rmse(testdata$Sales, F1$mean)
MAPE(testdata$Sales, F1$mean)

#F2 Values
rmse(testdata$Sales, F2$mean)
MAPE(testdata$Sales, F2$mean)

#F3 Values
rmse(testdata$Sales, HW1_for$mean)
MAPE(testdata$Sales, HW1_for$mean)

head(Alc)
