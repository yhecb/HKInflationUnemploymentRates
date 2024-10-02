inflation <- read_csv("inflation rate.csv")[,4]
unemployment <- read.csv("unemployment rate.csv")[,4]

acf(inflation,12,ylim=c(-1,1),main="")
pacf(inflation,12,ylim=c(-1,1),main="")
Box.test(inflation,lag=12,type="Ljung")

acf(unemployment,12,ylim=c(-1,1),main="")
pacf(unemployment,12,ylim=c(-1,1),main="")
Box.test(unemployment,lag=12,type="Ljung")

mod_inflation <- arima(inflation,order=c(2,0,0))
Box.test(mod_inflation$residuals,lag=12,type="Ljung")
mod_s <- arima(inflation, c(2, 0, 0), seasonal = list(order = c(1,0,0),period = 12))
Box.test(mod_s$residuals,lag=12,type="Ljung")
mod_s <- arima(inflation, c(2, 0, 0), seasonal = list(order = c(0,0,1),period = 12))
Box.test(mod_s$residuals,lag=12,type="Ljung")
AIC(mod_inflation)
AIC(mod_s)
BIC(mod_inflation)
BIC(mod_s)

mod_unemployment <- arima(unemployment,order=c(2,0,0))
Box.test(mod_unemployment$residuals,lag=12,type="Ljung")
model_1 <- arima(unemployment,order=c(0,1,2))
Box.test(model_1$residuals,lag=12,type="Ljung")
model_2 <- arima(unemployment,order=c(1,0,1))
Box.test(model_2$residuals,lag=12,type="Ljung")
model_3 <- arima(unemployment,order=c(2,0,0))
Box.test(model_3$residuals,lag=12,type="Ljung")
model_4 <- arima(unemployment,order=c(2,3,0))
Box.test(model_4$residuals,lag=12,type="Ljung")
model_5 <- arima(unemployment,order=c(1,2,0))
Box.test(model_5$residuals,lag=12,type="Ljung")


z <- data.frame(inflation,unemployment)

library(MTS)
mq(z,12)

m <- VARorder(z)

m1 <- VAR(z,2)
resi=m1$residuals
mq(resi,adj=8)

m2 <- refVAR(m1,thres=1.96)
MTSdiag(m2,adj=2)
