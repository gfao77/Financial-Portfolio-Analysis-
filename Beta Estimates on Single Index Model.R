# This code computes the beta of MSFT with respect to the benchmark SP500, NASDAQ Composite


library(tseries);
library(zoo);


#MSFT vs SP500

MSFT_prices <- get.hist.quote(instrument="msft", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(MSFT_prices) <- as.yearmon(index(MSFT_prices))
MSFT_returns <- diff(MSFT_prices)
colnames(MSFT_returns) <- c("MSFT")

SP_500_prices <- get.hist.quote(instrument="^gspc", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(SP_500_prices) <- as.yearmon(index(SP_500_prices))
SP_500_returns <- diff(SP_500_prices)
colnames(SP_500_returns) <- c("SP_500")

returns_MSFT <- merge(MSFT_returns,SP_500_returns)

#Fit linear model:regression

lm_MSFT <-lm(MSFT ~ SP_500, returns_MSFT)
summary(lm_MSFT)

plot(SP_500_returns, MSFT_returns, type="p", xlab="SP500", ylab="Microsoft", main="Linear regression MSFT/SP500")
abline(lm_MSFT$coefficients, col = "red")

plot(SP_500_returns, type="l", col="orange", ylab="returns", main="Observed MSFT/SP500")
lines(MSFT_returns, type="l", col="blue")
legend(x="topleft", legend=c("Microsoft", "SP500"), col=c("blue", "orange"), lwd=1, cex=0.7)



plot(SP_500_returns, type="l", col="orange", ylab="returns", main="Observed MSFT/SP500 with MSFT fitted values")
lines(MSFT_returns, type="l", col="blue")
lines(index(MSFT_returns),lm_MSFT$fitted.values, type="l", col="red")
legend(x="topleft", legend=c("Microsoft", "Microsoft fitted", "SP500"), col=c("blue", "red", "orange"), lwd=1, cex=0.7)

plot(MSFT_returns, type="l", col="blue", ylab="returns", main="Observed MSFT/MSFT fitted values")
lines(index(MSFT_returns),lm_MSFT$fitted.values, type="l", col="red")
legend(x="topleft", legend=c("Microsoft", "Microsoft fitted"), col=c("blue", "red"), lwd=1, cex=0.7)






#Analysis of time independence of beta

n <- length(MSFT_returns)

beta_MSFT_plot <- rep(0, times=91)

for(j in 1:91){
  u <- j+100
  r <- returns_MSFT[j:u, ]
  lm <-lm(MSFT ~ SP_500, r)
  beta_MSFT_plot[j] <- lm$coefficients[2]
}

beta_MSFT <- as.zoo(append(rep(0, times=100), beta_MSFT_plot))
beta_MSFT_plot <- as.zoo(beta_MSFT_plot)
index(beta_MSFT_plot) <- as.yearmon(index(MSFT_returns[101:191]))


plot(beta_MSFT_plot, type="l", col="orange", ylab="beta MSFT", main="Time analysis of beta MSFT", ylim=c(0.015,0.03) )
abline(h=max(beta_MSFT_plot), col="purple")
abline(h=min(beta_MSFT_plot), col="purple")
text(x=index(beta_MSFT_plot)[1], y=max(beta_MSFT_plot), labels="Max beta", pos=4, cex=0.75)
text(x=index(beta_MSFT_plot)[73], y=min(beta_MSFT_plot), labels="Min beta", pos=4, cex=0.75)

#Analysis of residuals

par(mfrow=c(2,2))
hist(lm_MSFT$residuals, xlab="MSFT residuals", main="", col="purple")
plot(density(lm_MSFT$residuals), xlab="MSFT residuals", main="", col="purple")
acf(lm_MSFT$residuals, lag.max=50, main="ACF Residuals")
qqnorm(lm_MSFT$residuals, main="QQ Normal/Residuals")
qqline(lm_MSFT$residuals, col ="purple")



#Same analysis for MSFT with benchmark NASDAQ Composite



NASDAQ_prices <- get.hist.quote(instrument="^ixic", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(NASDAQ_prices) <- as.yearmon(index(NASDAQ_prices))
NASDAQ_returns <- diff(NASDAQ_prices)
colnames(NASDAQ_returns) <- c("NASDAQ")

returns_MSFT <- merge(MSFT_returns,NASDAQ_returns)

#Fit linear model:regression

lm_MSFT_2 <-lm(MSFT ~ NASDAQ, returns_MSFT)
summary(lm_MSFT_2)

plot(NASDAQ_returns, MSFT_returns, type="p", xlab="NASDAQ", ylab="Microsoft", main="Linear regression MSFT/NASDAQ")
abline(lm_MSFT_2$coefficients, col = "red")

plot(NASDAQ_returns, type="l", col="green", ylab="returns", main="Observed MSFT/NASDAQ")
lines(MSFT_returns, type="l", col="blue")
legend(x="topleft", legend=c("Microsoft", "NASDAQ"), col=c("blue", "green"), lwd=1, cex=0.7)



plot(NASDAQ_returns, type="l", col="green", ylab="returns", main="Observed MSFT/NASDAQ with MSFT fitted values")
lines(MSFT_returns, type="l", col="blue")
lines(index(MSFT_returns),lm_MSFT_2$fitted.values, type="l", col="brown")
legend(x="topleft", legend=c("Microsoft", "Microsoft fitted", "NASDAQ"), col=c("blue", "brown", "green"), lwd=1, cex=0.7)

plot(MSFT_returns, type="l", col="blue", ylab="returns", main="Observed MSFT/MSFT fitted values")
lines(index(MSFT_returns),lm_MSFT_2$fitted.values, type="l", col="brown")
legend(x="topleft", legend=c("Microsoft", "Microsoft fitted"), col=c("blue", "brown"), lwd=1, cex=0.7)


#Analysis of time independence of beta

n <- length(MSFT_returns)

beta_MSFT_2_plot <- rep(0, times=91)

for(j in 1:91){
  u <- j+100
  r <- returns_MSFT[j:u, ]
  lm <-lm(MSFT ~ NASDAQ, r)
  beta_MSFT_2_plot[j] <- lm$coefficients[2]
}

beta_MSFT_2 <- as.zoo(append(rep(0, times=100), beta_MSFT_2_plot))
beta_MSFT_2_plot <- as.zoo(beta_MSFT_2_plot)
index(beta_MSFT_2_plot) <- as.yearmon(index(MSFT_returns[101:191]))


plot(beta_MSFT_2_plot, type="l", col="yellow", ylab="beta MSFT", main="Time analysis of beta MSFT")
abline(h=max(beta_MSFT_2_plot), col="purple")
abline(h=min(beta_MSFT_2_plot), col="purple")
text(x=index(beta_MSFT_2_plot)[1], y=max(beta_MSFT_2_plot), labels="Max beta", pos=4, cex=0.75)
text(x=index(beta_MSFT_2_plot)[73], y=min(beta_MSFT_2_plot), labels="Min beta", pos=4, cex=0.75)

#Analysis of residuals

par(mfrow=c(2,2))
hist(lm_MSFT_2$residuals, xlab="MSFT residuals", main="", col="yellow")
plot(density(lm_MSFT_2$residuals), xlab="MSFT residuals", main="", col="yellow")
acf(lm_MSFT_2$residuals, lag.max=50, main="ACF Residuals")
qqnorm(lm_MSFT_2$residuals, main="QQ Normal/Residuals")
qqline(lm_MSFT_2$residuals, col ="yellow")


#Comparison SP500 vs NASDAQ

par(mfrow=c(2,1))

plot(MSFT_returns, type="l", col="blue", ylab="returns", main="Observed MSFT/MSFT vs SP500 fitted values")
lines(index(MSFT_returns),lm_MSFT$fitted.values, type="l", col="red")
legend(x="topleft", legend=c("Microsoft", "Microsoft fitted"), col=c("blue", "red"), lwd=1, cex=0.3)

plot(MSFT_returns, type="l", col="blue", ylab="returns", main="Observed MSFT/MSFT vs NASDAQ fitted values")
lines(index(MSFT_returns),lm_MSFT_2$fitted.values, type="l", col="brown")
legend(x="topleft", legend=c("Microsoft", "Microsoft fitted"), col=c("blue", "brown"), lwd=1, cex=0.3)
















#APPLE vs SP500

APPLE_prices <- get.hist.quote(instrument="aapl", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(APPLE_prices) <- as.yearmon(index(APPLE_prices))
APPLE_returns <- diff(APPLE_prices)
colnames(APPLE_returns) <- c("APPLE")

SP_500_prices <- get.hist.quote(instrument="^gspc", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(SP_500_prices) <- as.yearmon(index(SP_500_prices))
SP_500_returns <- diff(SP_500_prices)
colnames(SP_500_returns) <- c("SP_500")

returns_APPLE <- merge(APPLE_returns,SP_500_returns)

#Fit linear model:regression

lm_APPLE <-lm(APPLE ~ SP_500, returns_APPLE)
summary(lm_APPLE)

plot(SP_500_returns, APPLE_returns, type="p", xlab="SP500", ylab="Apple", main="Linear regression APPLE/SP500")
abline(lm_APPLE$coefficients, col = "red")

plot(SP_500_returns, type="l", col="orange", ylab="returns", main="Observed APPLE/SP500")
lines(APPLE_returns, type="l", col="blue")
legend(x="topleft", legend=c("Apple", "SP500"), col=c("blue", "orange"), lwd=1, cex=0.7)



plot(SP_500_returns, type="l", col="orange", ylab="returns", main="Observed APPLE/SP500 with APPLE fitted values")
lines(APPLE_returns, type="l", col="blue")
lines(index(APPLE_returns),lm_APPLE$fitted.values, type="l", col="red")
legend(x="topleft", legend=c("Apple", "Apple fitted", "SP500"), col=c("blue", "red", "orange"), lwd=1, cex=0.7)

plot(APPLE_returns, type="l", col="blue", ylab="returns", main="Observed APPLE/APPLE fitted values")
lines(index(APPLE_returns),lm_APPLE$fitted.values, type="l", col="red")
legend(x="topleft", legend=c("Apple", "Apple fitted"), col=c("blue", "red"), lwd=1, cex=0.7)


#Analysis of time independence of beta

n <- length(APPLE_returns)

beta_APPLE_plot <- rep(0, times=91)

for(j in 1:91){
  u <- j+100
  r <- returns_APPLE[j:u, ]
  lm <-lm(APPLE ~ SP_500, r)
  beta_APPLE_plot[j] <- lm$coefficients[2]
}

beta_APPLE <- as.zoo(append(rep(0, times=100), beta_APPLE_plot))
beta_APPLE_plot <- as.zoo(beta_APPLE_plot)
index(beta_APPLE_plot) <- as.yearmon(index(APPLE_returns[101:191]))


plot(beta_APPLE_plot, type="l", col="orange", ylab="beta APPLE", main="Time analysis of beta APPLE",)
abline(h=max(beta_APPLE_plot), col="purple")
abline(h=min(beta_APPLE_plot), col="purple")
text(x=index(beta_APPLE_plot)[1], y=max(beta_APPLE_plot), labels="Max beta", pos=4, cex=0.75)
text(x=index(beta_APPLE_plot)[73], y=min(beta_APPLE_plot), labels="Min beta", pos=4, cex=0.75)

#Analysis of residuals

par(mfrow=c(2,2))
hist(lm_APPLE$residuals, xlab="APPLE residuals", main="", col="purple")
plot(density(lm_APPLE$residuals), xlab="APPLE residuals", main="", col="purple")
acf(lm_APPLE$residuals, lag.max=50, main="ACF Residuals")
qqnorm(lm_APPLE$residuals, main="QQ Normal/Residuals")
qqline(lm_APPLE$residuals, col ="purple")


