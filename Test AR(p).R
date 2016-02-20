#Given an observed time series, test to identify the best AR(p) model


library(tseries);
library(forecast) 
#get the data

MSFT_prices <- get.hist.quote(instrument="msft", start="2010-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(MSFT_prices) <- as.yearmon(index(MSFT_prices))

MSFT_returns <- diff(log(MSFT_prices))
names(MSFT_returns) <- c("ccReturn")
n <-length(MSFT_returns)

#make a plot

plot(MSFT_returns, type="l", xlab="", ylab="Return MSFT", col="blue")


#1- TEST FOR STATIONARITY - use the test statistic Dickey-Fullen

adf.test(MSFT_returns, alternative="stationary", k=0) 
adf.test(MSFT_returns, alternative="explosive", k=0) 


#OUTPUT: the test statistics 1- rejects the null hypotesis "explosive" with p-value 0.01, 2- accept the null hypotesis "stationarity" with p-value=0.99  


#2- ANALYSIS OF ACF
#recall: sample ACF are statistics for the autocorrelation functions 

acf(MSFT_returns, main="ACF MSFT")


#3 ANALYSIS OF PACF
#recall: sample PACF are statistics on the coefficients of an hypotetical
#AR(p) model of the correlation of the obs at time t and the obs at time t-p, given the intermediate observation. 
# PACF(p) are computed fitting a linear regression model with p independent variables


#1st method: with linear regression: we fit a linear model to compute PACF to lag 10

# Check for the pacf outside the confidence interval
pacf(MSFT_returns, main="PACF MSFT")
pacf(MSFT_returns, lag.max=50)$acf<=-0.22 #confidence interval by plot inspection

#it says that there is correlation at lag=4, 21. Test the hypotesis with the function arima of timeseries

fit <-arima(MSFT_returns, order = c(4,0,0))

summary(arima(MSFT_returns, order = c(4,0,0)))
acf(residuals(fit), main="ACF Residuals")
Box.test(residuals(fit), lag=10, fitdf=0, type="Ljung")

#Compare the box statistic with white noise (high values of the p-value are evidence for indipendence)
sim <- arima.sim(model=list(order=c(0,0,0)), n=71)
sim <- ts(sim)
plot(sim)
Box.test(sim, type="Ljung-Box")

#Simulate the model

model_par <- arima(MSFT_returns, order = c(4,0,0))$coef[1:4]
interc <- arima(MSFT_returns, order = c(4,0,0))$coef[5]

MSFT_sim <- arima.sim(model=list(ar=model_par), sd=sqrt(0.003857), n=71, mean=0 )+interc
MSFT_sim <- as.zoo(MSFT_sim)
index(MSFT_sim) <- index(MSFT_returns)

plot(MSFT_returns, type="l", main="MSFT observed/AR(4)",ylab="Microsoft", col="blue")
lines(as.zoo(MSFT_sim), type="l", col="orange")
legend(x="topleft", legend=c("MSFT observed", "AR(4) simulation"), col=c("blue", "orange"), lwd=1, cex=0.7)

#auto.arima

auto.arima(MSFT_returns)

#auto_arima predicts an AR(0) model i.e. white noise
MSFT_sim_2 <- arima.sim(model=list(order=c(0,0,0)), sd=sqrt(0.004339), n=71, mean=0.0118)
MSFT_sim_2 <- as.zoo(MSFT_sim_2)
index(MSFT_sim_2) <- index(MSFT_returns)

plot(MSFT_returns, type="l", main="MSFT observed/White Noise", ylab="Microsoft", col="blue")
lines(as.zoo(MSFT_sim_2), type="l", col="red")
legend(x="topleft", legend=c("MSFT observed", "White Noise simulation"), col=c("blue", "red"), lwd=1, cex=0.7)

pacf(MSFT_sim_2, main="PACF White Noise Simulation")

#Make predictions
MSFT_pred <- predict(fit, n.ahead=50)

plot(MSFT_returns, type="l", main="MSFT Prediction on AR(4)", ylab="Microsoft")
lines(MSFT_pred$pred, col="blue") 
lines(MSFT_pred$pred+2*MSFT_pred$se, col="green") 
lines(MSFT_pred$pred-2*MSFT_pred$se, col="green") 
legend(x="topleft", legend=c("MSFT observed", "MSFT prediction", "95% Confidence Interval"), col=c("black", "blue", "green"), lwd=1, cex=0.7)