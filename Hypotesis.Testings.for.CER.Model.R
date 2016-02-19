#Test Hypotesis of CER model and built portfolio frontiers

library(tseries);
library(zoo);
library(moments)


MSFT_prices <- get.hist.quote(instrument="msft", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
APPLE_prices <- get.hist.quote(instrument="aapl", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
index(MSFT_prices) <- as.yearmon(index(MSFT_prices))
index(APPLE_prices) <- as.yearmon(index(APPLE_prices))

MSFT_sreturns <- diff(MSFT_prices)
APPLE_sreturns <- diff(APPLE_prices)

colnames(MSFT_sreturns) <- c("MSFT")
colnames(APPLE_sreturns) <- c("APPLE")

#compute statistics

mu_MSFT <- mean(MSFT_sreturns)
sd_MSFT <- sd(MSFT_sreturns)
mu_APPLE <- mean(APPLE_sreturns)
sd_APPLE <- sd(APPLE_sreturns)
cor <- cor(MSFT_sreturns, APPLE_sreturns)

#Hypotesis: simple returns distribute normal

par(mfrow=c(2,2))
hist(MSFT_sreturns, main ="", xlab="Microsoft", col="blue")
hist(APPLE_sreturns, main ="", xlab="Apple", col="orange")
plot(density(MSFT_sreturns), main ="", xlab="Microsoft", col="blue")
plot(density(APPLE_sreturns), main ="", xlab="Apple", col="orange")

#Test for normality
n <- length(MSFT_sreturns)
jarque.bera.test(MSFT_sreturns)
jarque.bera.test(APPLE_sreturns)

JB_MSFT <- n/6*((skewness(MSFT_sreturns)^2)+((kurtosis(MSFT_sreturns)^2)/4))
JB_MSFT

JB_APPLE <- n/6*((skewness(APPLE_sreturns)^2)+((kurtosis(APPLE_sreturns)^2)/4))
JB_APPLE

#Our data are not normally distributed, we still plot the mean variance frontier of the portfolio

x_M <- seq(from= -0.4, to= 1, by=0.05)
x_V <- 1-x_M

mu_p <- x_M*mu_MSFT + x_V*mu_APPLE
sd_p <- (x_M^2)*sd_MSFT+(x_V^2)*sd_APPLE+2*cor*sd_MSFT*sd_APPLE*x_M*x_V


plot(sd_p, mu_p, ylab="Portfolio Expected Value", xlab="Portfolio Volatility", main="Mean-Variance frontier", type='l' )
par(mfrow=c(2,2))
plot(x_M, mu_p, ylab="Portfolio Expected Value", xlab="Shares in MSFT percentage", type='l' )
plot(x_M, sd_p, ylab="Portfolio Volatility", xlab="Shares in MSFT percentage", type='l' )
plot(x_V, mu_p, ylab="Portfolio Expected Value", xlab="Shares in APPLE percentage", type='l' )
plot(x_V, sd_p, ylab="Portfolio Volatility", xlab="Shares in APPLE percentage", type='l' )

#Plotting different portfolios frontiers based on different correlations values

pho_sim <- seq(from=-1, to=1, by=0.2)
sd_sim <-matrix(0, ncol=11, nrow=length(x_M) )

for(i in 1:11){
  sd_sim[,i] <- (x_M^2)*sd_MSFT+(x_V^2)*sd_APPLE+2*pho_sim[i]*sd_MSFT*sd_APPLE*x_M*x_V
}



for (i in 1:11){
  if (i==1) plot(sd_sim[,1], mu_p, ylab="Portfolio Expected Value", xlab="Portfolio Standard Deviation", main="Mean-Variance frontier", type='l', color=rainbow(11)[1]) else
    lines(x = sd_sim[,i], y = mu_p,col=i)
}




