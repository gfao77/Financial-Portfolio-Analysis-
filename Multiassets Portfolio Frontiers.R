#This code computes the efficient portfolio frontier, plots random generated portfolios to compare with the frontier,
#find the minimum variance portfolio, the tangency portfolio and the efficient frontier. Protfolios are based on 3 assets
#(MSFT, APPLE and AMAZON). Data acquired from Yahoo Finance

library(tseries);
library(zoo);
library(quadprog);  

MSFT_prices <- get.hist.quote(instrument="msft", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
APPLE_prices <- get.hist.quote(instrument="aapl", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)
AMAZON_prices <- get.hist.quote(instrument="amzn", start="2000-01-01",end="2015-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

index(AMAZON_prices) <- as.yearmon(index(AMAZON_prices))
index(MSFT_prices) <- as.yearmon(index(MSFT_prices))
index(APPLE_prices) <- as.yearmon(index(APPLE_prices))

MSFT_sreturns <- diff(MSFT_prices)
APPLE_sreturns <- diff(APPLE_prices)
AMAZON_sreturns <- diff(AMAZON_prices)

colnames(MSFT_sreturns) <- c("MSFT")
colnames(APPLE_sreturns) <- c("APPLE")
colnames(AMAZON_sreturns) <- c("AMAZON")


returns <- merge(MSFT_sreturns,APPLE_sreturns, AMAZON_sreturns )

#compute statistics

mu <- apply(returns, 2, mean)
sigma <- cov(returns)

#write the matrix for the lagrangian to describe efficient portfolios frontier

#set mu_0 to mu_MSFT

top_mat <- cbind(2*sigma, rep(1,times=3), mu)
mid_row <- c(rep(1, times=3), 0, 0)
bot_row <- c(t(mu), 0, 0)

A <- rbind(top_mat, mid_row, bot_row)


b <- solve(A)%*%c(rep(0, times=3), 1, mu[1])
x <- b[1:3]

#Plot the efficient frontier

mu_val <- seq(from= -4, to=4, by=0.005)
sd_val <- rep(0, times=length(mu_val))
X <-matrix(0, ncol=3, nrow=length(mu_val))

for(i in 1:length(mu_val)){
  
  b <- solve(A)%*%c(rep(0, times=3), 1, mu_val[i])
  X[i,] <- b[1:3]
  sd_val[i] <- sqrt(t(X[i,])%*%sigma%*%X[i,])
}

plot(sd_val, mu_val, type="l", xlab="SD Portfolio", ylab="Expected Return Portfolio")
legend(x="topleft", legend="Portfolio Frontier", col=1, lwd=1, cex=0.7)

#random generation of 100 portfolios

X_ran <- matrix(0, ncol=3,nrow=1000)
for(i in 1:1000){
  X_ran[i,] =runif(3, min=0, max=1)
}

X_ran <- X_ran/apply(X_ran, 1, sum)
sd_ran <- rep(0, times=1000)
mu_ran <- X_ran%*%mu

for(i in 1:1000){
  sd_ran[i]= sqrt(t(X_ran[i,])%*%sigma%*%X_ran[i,])
}

plot(sd_val, mu_val, type="l", xlab="SD Portfolio", ylab="Expected Return Portfolio")
points(sd_ran, mu_ran, type="p", col="yellow")
legend(x="topleft", legend=c("Portfolio Frontier", "Generic Portfolio"), col=c(1, "yellow"), lwd=1, cex=0.7)

#solve the minimum variance portfolio

top_mat <- cbind(2*sigma, rep(1,times=3))
mid_row <- c(rep(1, times=3), 0)


B <- rbind(top_mat, mid_row)


c <- solve(B)%*%c(rep(0, times=3), 1)
x_min <- c[1:3]

mu_min <- t(x_min)%*%mu
sd_min <- sqrt(t(x_min)%*%sigma%*%x_min)

c <- mu_val<=as.numeric(mu_min)

plot(sd_val, mu_val, type="l", xlab="SD Portfolio", ylab="Expected Return Portfolio")
legend(x="topleft", legend=c("Portfolio Frontier", "Inefficient Frontier"), col=c(1, "red"), lwd=1, cex=0.7)
lines(sd_val[c],mu_val[c], type="l", col="red")
points(sd_ran, mu_ran, type="p", col="yellow")
points(sd_min, mu_min, type="p", col="purple")
text(sd_min, mu_min, labels="Minimum Variance", pos=4, cex=0.7)



as <- matrix(c(1,0,0,0,1,0,0,0,1), ncol=3, nrow=3)

mu_as <- rep(0, times=3)
sd_as <- rep(0, times=3)
for(i in 1:3){
  mu_as[i] <- t(as[i,])%*%mu
  sd_as[i] <- sqrt(t(as[i,])%*%sigma%*%as[i,])
}


plot(sd_val, mu_val, type="l", xlab="SD Portfolio", ylab="Expected Return Portfolio", xlim=c(2, 20), ylim=c(0,3.4))
points(sd_ran, mu_ran, type="p", col="yellow")
points(sd_min, mu_min, type="p", col="purple")
points(sd_as, mu_as, type="p", col=rainbow(3))
text(sd_min, mu_min, labels="Minimum Variance", pos=4, cex=0.7)
text(sd_as, mu_as, labels=colnames(returns), pos=1, cex=0.7)




#Find the tangency portfolio with rf<mu_min
r_f <- 0.03
I <- rep(1, times=length(mu))

x_tan <- (solve(sigma)%*%(mu-r_f*I))/(as.numeric(t(I)%*%solve(sigma)%*%(mu-r_f*I)))
mu_tan <- t(x_tan)%*%mu
sd_tan <- sqrt(t(x_tan)%*%sigma%*%x_tan)

sh_tan <- (mu_tan-r_f)/sd_tan

plot(sd_val, mu_val, type="l",xlab="SD Portfolio", ylab="Expected Return Portfolio", xlim=c(2, 20), ylim=c(-3.4,3.4))
lines(sd_val[c],mu_val[c], type="l", col="red")
abline(a=r_f, b=sh_tan, col="green")
legend(x="topleft", legend=c("Portfolio Frontier", "Efficient Frontier with risk free"), col=c(1, "green"), lwd=1, cex=0.7)

#Find the tangency portfolio with rf>mu_min

r_f <- 0.15
I <- rep(1, times=length(mu))

x_tan <- (solve(sigma)%*%(mu-r_f*I))/(as.numeric(t(I)%*%solve(sigma)%*%(mu-r_f*I)))
mu_tan <- t(x_tan)%*%mu
sd_tan <- sqrt(t(x_tan)%*%sigma%*%x_tan)

sh_tan <- (mu_tan-r_f)/sd_tan

plot(sd_val, mu_val, type="l",xlab="SD Portfolio", ylab="Expected Return Portfolio", xlim=c(2, 20), ylim=c(-3.4,3.4))
lines(sd_val[c],mu_val[c], type="l", col="red")
abline(a=r_f, b=sh_tan, col="green")
legend(x="topleft", legend=c("Portfolio Frontier", "Efficient Frontier with risk free"), col=c(1, "green"), lwd=1, cex=0.7)



#Efficient frontier with no short sales

n <-3
mu_0 <- mu[1]
A <- rbind(mu, I, diag(n))
b <- c(mu_0,1,rep(0, times=n))
d <-  rep(0, times=n)

solve.QP(sigma, d, t(A), b, meq=2, FALSE)

#draw efficient frontier no short sale

#Find range of ammisible expected returns: to select the range, recall that mu of a portfolio no short has to be always greater then the minimum of the assets means

l <-min(mu)
u <-max(mu)

mu_val_n <- seq(from= l, to=u, by=0.005)
sd_val_n <- rep(0, times=length(mu_val_n))
Y <-matrix(0, ncol=3, nrow=length(mu_val_n))

for(i in 1:length(mu_val_n)){
  
  b_n <- c(mu_val_n[i],1,rep(0, times=n))
  Y[i,] <- solve.QP(sigma, d, t(A), b_n, meq=2, FALSE)$solution
  sd_val_n[i] <- sqrt(2*solve.QP(sigma, d, t(A), b_n, meq=2, FALSE)$value)
}

plot(sd_val, mu_val, type="l",  xlab="SD Portfolio", ylab="Expected Return Portfolio", xlim=c(2, 20), ylim=c(0,3.4))
lines(sd_val_n, mu_val_n, type="l", col="orange")
legend(x="topleft", legend=c("Portfolio Frontier", "Portfolio Frontier no short sales"), col=c(1, "orange"), lwd=1, cex=0.7)



