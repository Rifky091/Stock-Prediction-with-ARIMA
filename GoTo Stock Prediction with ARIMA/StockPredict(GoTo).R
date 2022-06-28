mydata <-read.csv("C:/Users/Rifky/OneDrive/Documents/Github/GoTo Stock Prediction with ARIMA/GOTO.csv" 
              , header=TRUE)
class(mydata)

tsdata <- ts(mydata)
class(tsdata)
str(tsdata)
tsdata
summary(tsdata)

Stock <- tsdata[,5]
ts.plot(Stock)
plot(diff(diff(Stock)))


library(tseries)
adf.test(Stock)
adf.test(diff(diff(Stock))) 

#differencing pertama (ordo d==2)
par(mfrow=c(2,1))
acf(diff(diff(Stock)))
pacf(diff(diff(Stock)))

#Estimasi Model

library(forecast)
auto.arima(Stock)
Arima.1 <- arima(Stock, order=c(0,2,2))
Arima.2 <- arima(Stock, order=c(2,2,1))
Arima.3 <- arima(Stock, order=c(2,2,2))

summary(Arima.1)
summary(Arima.2)
summary(Arima.3)

library(lmtest)
coeftest(Arima.1)
coeftest(Arima.2)
coeftest(Arima.3)

#Diagnostic checking
tsdiag(Arima.1)
tsdiag(Arima.2)
tsdiag(Arima.3)

resid1 = Arima.1$residuals
resid2 = Arima.2$residuals
resid3 = Arima.3$residuals

t.test(resid1, mu = 0)
t.test(resid2, mu = 0)
t.test(resid3, mu = 0)

#memilih model yang akan digunakan dengan melihat nilai AIC terkecil
library(stargazer)
setwd("C:/Users/Rifky/OneDrive/Documents/Tugas R/Tubes/ArimaFinance")
stargazer(Arima.1,Arima.2,Arima.3, type="html", out = "stock.rtf")

#Prediction
pred.data = forecast(Arima.1, n=10) #prediksi 10 hari kedepan
pred.data
accuracy(Arima.1)
accuracy(Arima.2)
accuracy(Arima.3)

#Visualisasi
fit <- Arima.1
fit <- Arima.2
fit.data = fitted(Arima.1)
fit.data1 = fitted(Arima.2)
par(mfrow = c(1,1))
plot(forecast(fit), lty=1)
lines(fit.data, col="red")
lines(fit.data1, col="yellow")
legend(1, 700, legend=c("Raw Data", "ARIMA","Predict"),
       col=c("Black", "Red","blue"),lty=1, cex=0.8)



