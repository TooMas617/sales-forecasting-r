# =============================================================================
# Monthly sales forecasting
# Course: Business, Economic and Financial Data - University of Padova (A.Y. 2024/25)
# Authors: Tomas Guarini, Arianna Morandi, Beatrice Spagnolo
#
# Data: monthly sales of a local interior design company, Jan 2017 - Dec 2023
#       (84 observations), in data/sales.csv
# Usage: open this folder as the working directory, then run the script.
# =============================================================================

# Libraries ----
library(ggplot2)
library(zoo)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(gam)
library(mgcv)

# Data loading ----
# relative path: run the script from the project folder
data <- read.csv("data/sales.csv", sep = ";")
str(data)
head(data)
colnames(data)

# Data preparation ----
data$SALES <- gsub(",", ".", data$SALES)  
data$SALES <- as.numeric(data$SALES)
data$MONTHS <- as.yearmon(data$MONTHS, "%Y-%m")  
str(data)
sales <- data$SALES

# Exploratory plots ----
plot(sales, type= "b",xlab="Months", ylab="Sales",  pch=16, lty=3, xaxt="n", cex=0.6, main="Sales")
axis(1, labels=data$year)

acf(sales, main="ACF Sales", lwd=2)
pacf(sales, main="PACF Sales", lwd=2)

# Linear models ----
# time series object
sales.ts <- ts(sales, frequency = 12)
ts.plot(sales.ts, type="o", main="Simple Linear Regression")

linear_model<- tslm(sales.ts~trend)
summary(linear_model)

# plot 
plot(sales.ts, ylab="Sales", xlab="Time", main="Simple Linear Regression")
lines(fitted(linear_model), col=2, lwd=2)

# Residuals  
res <- residuals(linear_model)
plot(res, ylab="Residuals", main="Residuals of Linear Regression")
dwtest(linear_model)


# LINEAR MODEL with trend and seasonality
linear_model1 <- tslm(sales.ts~ trend+season)
summary(linear_model1)

#plot
plot(sales.ts, ylab="Sales", xlab="Time", main="Linear Regression with Seasonality")
lines(fitted(linear_model1), col=2, lwd=2)

#residuals 
res_lm1 <- residuals(linear_model1)
plot(res_lm1, ylab="Residuals", main= "Residuals of Linear Regression with Seasonality")
dwtest(linear_model1)


# GRAPHICAL COMPARISON
plot(sales.ts, ylab="Sales", xlab="Time", main="Linear Regression without and with Seasonality")
lines(fitted(linear_model), col=3, lwd=2)
lines(fitted(linear_model1), col=2, lwd=2)


# Generalized additive models (GAM) ----
time<- (1:length(sales))
stagionality <- factor(c(rep(1:12,length(sales)/12)))

# linear regression
GAM0 <- lm(sales~ time+stagionality)
summary(GAM0)

# GAM(s)
GAM1 <- gam(sales ~ s(time)+stagionality)
summary(GAM1)
par(mfrow=c(1,2))
plot(GAM1, se=T)

# GAM(lo)
GAM2 <- gam(sales ~ lo(time)+stagionality)
summary(GAM2)

# AIC comparison
AIC(GAM0)
AIC(GAM1)
AIC(GAM2)

# plot
plot(as.numeric(sales), type="b", ylab="Sales", xlab="Months", main="Generalized Additive Model", lty=3)
lines(fitted(GAM1), col=2, lwd=2)

# residuals
tsdisplay(residuals(GAM1))


# Bass models (BM, GBM, GGM) ----
bm<-BM(sales,display = T)
summary(bm)

# prediction
pred_bm<- predict(bm, newx=c(1:84))
pred.inst<- make.instantaneous(pred_bm)

# residuals
res_BM<- residuals(bm)
plot(res_BM, ylab="Residuals", main= "Residuals of Bass Model")
acf_BM<- acf(residuals(bm), main="ACF Bass Model")

# GENERALIZED BASS MODEL rectangular shock
# shock covid (0 sales on 04/2020)
GBMr1<- GBM(sales,shock = "rett",nshock = 1, prelimestimates = c(9.133136e+07, 5.504895e-04, 4.484345e-03, 35, 45,-0.01))
summary(GBMr1)

# prediction
pred_GBMr1<- predict(GBMr1, newx=c(1:84))
pred_GBMr1.inst<- make.instantaneous(pred_GBMr1)

# plot
plot(sales, type= "b", main = "GBM with rectangular shock", xlab="Months", ylab="Sales",  pch=16, lty=3, cex=0.6, xlim=c(1,84))
lines(pred_GBMr1.inst, lwd=2, col=2)

# residuals
res_GBMr1<- residuals(GBMr1)
plot(res_GBMr1, ylab="Residuals", main= "Residuals of Generalized Bass Model")
acf_GBMr1<- acf(residuals(GBMr1), main="ACF Generalized Bass Model")


# COMPARISON between BM and GBMr1
plot(sales, type= "b", main = "Comparison between BM & GBMr", xlab="Months", ylab="Sales",  pch=16, lty=3, cex=0.6)
lines(pred_GBMr1.inst, lwd=2, col=2)
lines(pred.inst, lwd=2, col=3)


# GUSEO GUIDOLIN MODEL 
GGM<- GGM(sales, prelimestimates=c(9.133136e+07, 0.001, 0.01, 5.504895e-04, 4.484345e-03))
summary(GGM)

# prediction
pred_GGM<- predict(GGM, newx=c(1:84))
pred_GGM.inst<- make.instantaneous(pred_GGM)

# plot
plot(sales, type= "b", main = "Guseo Guidolin Model", xlab="Months", ylab="Sales",  pch=16, lty=3, cex=0.6)
lines(pred_GGM.inst, lwd=2, col=2)

# residuals
res_GGM<- residuals(GGM)
plot(res_GGM, ylab="Residuals", main= "Residuals of Guseo Guidolin Model")
acf<- acf(residuals(GGM))


# ARIMA ----
tsdisplay(sales)

arima101<- Arima(sales, order=c(1,0,1))
summary(arima101)
plot(sales, type="b", ylab="Sales", xlab="Months", main="Arima (1,0,1)", lty=3)
lines(fitted(arima101), col=2, lwd=2)
checkresiduals(arima101)

# AUTO ARIMA (0,0,1)
auto.a<- auto.arima(sales)
summary(auto.a)
plot(sales, type="b", ylab="Sales", xlab="Months", main="Arima (0,0,1)", lty=3)
lines(fitted(auto.a), col=2, lwd=2)
checkresiduals(auto.a)

#ARIMA (1,1,1)
arima111<- Arima(sales, order=c(1,1,1))
summary(arima111)
plot(sales, type="b", ylab="Sales", xlab="Months", main="Arima (1,1,1)", lty=3)
lines(fitted(arima111), col=2, lwd=2)
checkresiduals(arima111)


# SARIMA ----

#SARIMA (0,0,1)(0,1,0)
sarima001.010<- Arima(sales, order=c(0,0,1), seasonal = list(order= c(0,1,0), period =12))
summary(sarima001.010)
plot(sales, type="b", ylab="Sales", xlab="Months", main="Sarima (0,0,1)(0,1,0)", lty=3)
lines(fitted(sarima001.010), col=2, lwd=2)
checkresiduals(sarima001.010)

#SARIMA (0,0,1)(0,1,1)
sarima001.011<- Arima(sales, order=c(0,0,1), seasonal = list(order= c(0,1,1), period =12))
summary(sarima001.011)
plot(sales, type="b", ylab="Sales", xlab="Months", main="Sarima (0,0,1)(0,1,1)", lty=3)
lines(fitted(sarima001.011), col=2, lwd=2)
checkresiduals(sarima001.011)

#SARIMA (1,1,1)(0,1,0)
sarima111.010<- Arima(sales, order=c(1,1,1), seasonal = list(order= c(0,1,0), period =12))
summary(sarima111.010)
plot(sales, type="b", ylab="Sales", xlab="Months", main="Sarima (1,1,1)(0,1,0)", lty=3)
lines(fitted(sarima111.010), col=2, lwd=2)
checkresiduals(sarima111.010)

#SARIMA (1,1,1)(0,1,1)
sarima111.011<- Arima(sales, order=c(1,1,1), seasonal = list(order= c(0,1,1), period =12))
summary(sarima111.011)
plot(sales, type="b", ylab="Sales", xlab="Months", main="Sarima (1,1,1)(0,1,1)", lty=3)
lines(fitted(sarima111.011), col=2, lwd=2)
checkresiduals(sarima111.011)


# ARIMAX with COVID shock ----
data$MONTHS[39]
data$MONTHS[45]

shock <- rep(0, length(sales))
shock_start <- 39
shock_end <- 45
decay_factor <- 0.2
for (i in shock_start:shock_end) {
  shock[i] <- exp(-decay_factor * (i - shock_start))
}
shock_ts <- ts(shock, start=c(2017, 1), frequency=12)

#ARIMAX(1,1,1)
arimax_model <- Arima(sales, xreg=shock_ts, order=c(1, 1, 1))
summary(arimax_model)
checkresiduals(arimax_model)
pres1 <- fitted(arimax_model)
plot(sales, type= "b", main = "Arimax(1,1,1)", xlab="Months", ylab="Sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=1:length(data$MONTHS), labels=data$MONTHS)
lines(pres1, lty=1,lwd=2, col=2)


# SARIMAX(1,1,1)(0,1,1) with COVID shock ----
sarimax_model <- Arima(sales, xreg=shock_ts, order=c(1, 1, 1), seasonal=list(order=c(0,1,1), period=12))
summary(sarimax_model)
checkresiduals(sarimax_model)
pres2 <- fitted(sarimax_model)
plot(sales, type= "b", main = "Sarimax(1,1,1)(0,1,1)", xlab="Months", ylab="Sales",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=1:length(data$MONTHS), labels=data$MONTHS)
lines(pres2, lty=1,lwd=2, col=2)


# Final forecast: train/test evaluation and future sales ----
train_size <- floor(0.7 * nrow(data))
train <- seq(1, train_size)
data.train <- data[train, ]
data.test <- data[(train_size + 1):nrow(data), ]

# train xreg
shock_train <- rep(0, length(data.train$SALES))
shock_start <- 39
shock_end <- 45
decay_factor <- 0.2  
for (i in shock_start:shock_end) {
  shock_train[i] <- exp(-decay_factor * (i - shock_start))
}
shock_train <- ts(shock_train, start=c(2017, 1), frequency=12)

# test xreg
shock_test <- rep(0, length(data.test$SALES))

# model train
sarimax_model <- Arima(data.train$SALES, xreg=shock_train, order=c(1, 1, 1), seasonal=list(order=c(0,1,1), period=12))

# model test
predictions <- forecast(sarimax_model, h = nrow(data.test), xreg = shock_test)
accuracy_values <- accuracy(predictions, data.test$SALES)
print(accuracy_values)

# plot
test_indices <- (nrow(data.train) + 1):(nrow(data))
plot(sales, type = "b", ylab = "Sales", xlab = "Months", main = "Forecast on test set", lty = 3)
lines(fitted(sarimax_model), col = 2, lwd = 2) 
lines(test_indices, predictions$mean, col = 4, lwd = 2)

# FUTURE PREDICTIONS
shock_test_future <- rep(0, 12)
sarimax_model <- Arima(data$SALES, xreg = shock_ts, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
future_forecast <- forecast(sarimax_model, h = 12, xreg = shock_test_future)
autoplot(future_forecast) + ggtitle("Forecast for Future Sales") + ylab("Sales") + xlab("Months")
