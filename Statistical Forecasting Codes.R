library(tseries)
library(lmtest)
library(readxl)
library(leaps)
library(Metrics)

data <- read_excel("ProjectData.xlsx")
data.ts1 <- ts(data$`Unleaded Gasoline Sale (UGS)`, start= c(2000, 1),end= c(2006, 4), frequency=4)
data.ts2 <- ts(data$`Diesel Gasoline Sale (DGS)`, start= c(2000, 1), end= c(2006, 4), frequency=4)
plot(xlab='time', ylab='Unleaded Gasoline Sale', data.ts1)
plot(xlab='time', ylab='Diesel Gasoline Sale', data.ts2)
acf(data.ts1)
acf(data.ts2)
pacf(data.ts1)
pacf(data.ts2)
data.ts1.diff <- diff(data.ts1)
plot(ylab='first difference applied to UGS', data.ts1.diff)
data.ts2.diff <- diff(data.ts2)
plot(ylab='first difference applied to DGS', data.ts2.diff)
acf(data.ts1.diff)
acf(data.ts2.diff)
pacf(data.ts1.diff)
pacf(data.ts2.diff)

adf.test(data.ts1.diff)
adf.test(data.ts2.diff)


data.ts1.diff.seasonal <- diff(data.ts1.diff, 4)
data.ts2.diff.seasonal <- diff(data.ts2.diff, 4)
plot(ylab='first seasonal difference applied to UGS', data.ts1.diff.seasonal)
plot(ylab='first seasonal difference applied to DGS', data.ts2.diff.seasonal)
acf(data.ts1.diff.seasonal, ci=0.90)
pacf(data.ts1.diff.seasonal)
acf(data.ts2.diff.seasonal, ci=0.90)
pacf(data.ts2.diff.seasonal)


data.ts1.model <- auto.arima(data.ts1)
data.ts1.model
data.ts2.model <- auto.arima(data.ts2)
data.ts2.model

first.suggested.model.ts1 <- Arima(data.ts1, order=c(0,1,1), seasonal=c(1,1,0), include.constant=TRUE)
first.suggested.model.ts1
tsdisplay(residuals(first.suggested.model.ts1), lag = 20)

first.suggested.model.ts2 <- Arima(data.ts2, order=c(1,1,0), seasonal=c(0,1,1), include.constant=TRUE)
first.suggested.model.ts2
tsdisplay(residuals(first.suggested.model.ts2))

#we start neighborhood search

#for ts1 (UGS) six models are inspected

second.suggested.model.ts1 <- Arima(data.ts1, order=c(1,1,1), seasonal=c(1,1,0), include.constant=TRUE) #increased regular AR by 1 wrt to model 1
second.suggested.model.ts1                                                                              #worse than first
tsdisplay(residuals(second.suggested.model.ts1), lag = 20)


third.suggested.model.ts1 <- Arima(data.ts1, order=c(1,1,0), seasonal=c(1,1,0), include.constant=TRUE)  #decreased regular MA and increased regular AR by 1 wrt to model 1
third.suggested.model.ts1                                                                               #better than first
tsdisplay(residuals(third.suggested.model.ts1), lag=20)

fourth.suggested.model.ts1 <- Arima(data.ts1, order=c(1,1,0), seasonal=c(2,1,0), include.constant=TRUE) #increased seasonal AR by 1 wrt to model 3
fourth.suggested.model.ts1                                                                              #better than 1,  worse than 3
tsdisplay(residuals(fourth.suggested.model.ts1), lag=20)


fifth.suggested.model.ts1 <- Arima(data.ts1, order=c(1,1,0), seasonal=c(0,1,1), include.constant=TRUE) #decreased seasonal AR by 1 and increased seasonal MA by 1 wrt to model 3
fifth.suggested.model.ts1                                                                               #better than 3  BEST MODEL
tsdisplay(residuals(fifth.suggested.model.ts1), lag = 20)


sixth.suggested.model.ts1 <- Arima(data.ts1, order=c(1,1,0), seasonal=c(0,1,2), include.constant=TRUE) #increased seasonal MA by 1 wrt to model 5
sixth.suggested.model.ts1                                                                               #worse than 5
tsdisplay(residuals(sixth.suggested.model.ts1), lag = 20)

#forecasting

forecast(fifth.suggested.model.ts1)
plot(forecast(fifth.suggested.model.ts1))

#for ts2 (DGS)

second.suggested.model.ts2 <- Arima(data.ts2, order=c(1,1,1), seasonal=c(0,1,1), include.constant=TRUE)
second.suggested.model.ts2
tsdisplay(residuals(second.suggested.model.ts2), lag = 20)

third.suggested.model.ts2 <- Arima(data.ts2, order=c(1,1,0), seasonal=c(1,1,0), include.constant=TRUE)
third.suggested.model.ts2
tsdisplay(residuals(third.suggested.model.ts2), lag=20)

fourth.suggested.model.ts2 <- Arima(data.ts2, order=c(0,1,0), seasonal=c(1,1,0), include.constant=TRUE)
fourth.suggested.model.ts2
tsdisplay(residuals(fourth.suggested.model.ts2), lag=20)

fifth.suggested.model.ts2 <- Arima(data.ts2, order=c(0,1,1), seasonal=c(1,1,0), include.constant=TRUE)
fifth.suggested.model.ts2
tsdisplay(residuals(fifth.suggested.model.ts2), lag = 20)

sixth.suggested.model.ts2 <- Arima(data.ts2, order=c(0,1,0), seasonal=c(1,1,1), include.constant=TRUE)
sixth.suggested.model.ts2
tsdisplay(residuals(sixth.suggested.model.ts2), lag = 20)

#forecasting

forecast(fourth.suggested.model.ts2)
plot(forecast(fourth.suggested.model.ts2))

#------Regression

data <- read_excel("ProjectData.xlsx")
data.ts1 <- ts(data$`Unleaded Gasoline Sale (UGS)`, start= c(2000, 1),end= c(2006, 4), frequency=4)
data.ts2 <- ts(data$`Diesel Gasoline Sale (DGS)`, start= c(2000, 1), end= c(2006, 4), frequency=4)
plot(xlab='time', ylab='Unleaded Gasoline Sale', data.ts1)
plot(xlab='time', ylab='Diesel Gasoline Sale', data.ts2)


#take logarithm of the data
data.log.ts1 = ts(log(data$`Unleaded Gasoline Sale (UGS)`), start= c(2000, 1),end= c(2006, 4), frequency=4)
data.log.ts2 = ts(log(data$`Diesel Gasoline Sale (DGS)`), start= c(2000, 1), end= c(2006, 4), frequency=4)
plot(xlab='time', ylab='Log Unleaded Gasoline Sale', data.log.ts1)
plot(xlab='time', ylab='Log Diesel Gasoline Sale', data.log.ts2)


UGS = data$`Unleaded Gasoline Sale (UGS)`
DGS = data$`Diesel Gasoline Sale (DGS)`
RNUV = data$RNUV
PU = data$`Price of Unleaded Gasoline (PU)`
PG = data$`Price of Diesel Gasoline (PG)`
AGR = data$`GNP Agriculture`
COM = data$`GNP Commerce`
GNPT = data$`GNP Total`
QUAR = data$...1
NLPG = data$`# LPG Vehicles (NLPG)`
NUGV = data$`# Unleaded Gasoline Vehicles (NUGV)`
NDGV = data$`# of Diesel Gasoline Vehicles (NDGV)`
Q1 = rep(c(1, 0, 0, 0), length.out = 32)
Q2 = rep(c(0, 1, 0, 0), length.out = 32)
Q3 = rep(c(0, 0, 1, 0), length.out = 32)
data=data.frame(UGS, DGS, RNUV, PU, PG, AGR, COM, GNPT, NLPG, NUGV, NDGV, Q1, Q2, Q3)

#find the best model according to BIC ---- Unleaded Gasoline Sale (UGS)
leapsmodel=regsubsets(`UGS`~ RNUV+PU+PG+AGR+COM+GNPT+NLPG+NUGV+NDGV+Q1+Q2+Q3, data=data,nbest=10)
plot(leapsmodel,scale="bic")

reg_bic_1 <- lm(`UGS`~ PU+GNPT+NUGV+NDGV+Q1+Q3, data=data)
summary(reg_bic_1)    #significance of coefficients
plot(ylab='Unleaded Gasoline Sale Residuals',ts(reg_bic_1$residuals, start= c(2000, 1),end= c(2006, 4), frequency=4)) #residual analysis
dwtest(reg_bic_1)     #Durbin-Watson statistics

predictions <- predict(reg_bic_1, newdata = data[29:32,]) #predictions
predictions

#RMSE
predicted_values <- predict(reg_bic_1, newdata = data)
rmse_value <- rmse(data$UGS[1:28], predicted_values[1:28])
rmse_value

#MAPE
mape_value <- mape(data$UGS[1:28], predicted_values[1:28])
mape_value

#visualize
x <- seq_len(nrow(data))
plot(x, data$UGS, type = "l", xlab = "Index", ylab = "UGS", ylim=c(650000,1400000))
lines(x[29:32], predictions, col = "red")
legend("topright", legend = c("Original Data", "Predictions"), col = c("black", "red"), lty = c(1, 1))



#find the best model according to BIC ---- Diesel Gasoline Sale (DGS)
leapsmodel2=regsubsets(`DGS`~ RNUV+PU+PG+AGR+COM+GNPT+NLPG+NUGV+NDGV+Q1+Q2+Q3, data=data,nbest=10)
plot(leapsmodel2,scale="bic")

reg_bic_2 <- lm(`DGS`~ PG+COM+NUGV+Q2, data=data)
summary(reg_bic_2)    #significance of coefficients
plot(ylab='Diesel Gasoline Sale Residuals',ts(reg_bic_2$residuals, start= c(2000, 1),end= c(2006, 4), frequency=4)) #residual analysis
dwtest(reg_bic_2)     #Durbin-Watson statistics

#RMSE
predicted_values2 <- predict(reg_bic_2, newdata = data)
rmse_value2 <- rmse(data$DGS[1:28], predicted_values2[1:28])
rmse_value2

#MAPE
mape_value2 <- mape(data$DGS[1:28], predicted_values2[1:28])
mape_value2

predictions2 <- predict(reg_bic_2, newdata = data[29:32,]) #predictions
predictions2

#visualize
x <- seq_len(nrow(data))
plot(x, data$DGS, type = "l", xlab = "Index", ylab = "DGS", ylim=c(2000000,4400000))
lines(x[29:32], predictions2, col = "red")
legend("topleft", legend = c("Original Data", "Predictions"), col = c("black", "red"), lty = c(1, 1))

