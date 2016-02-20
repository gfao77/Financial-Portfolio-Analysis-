#This code computes bootstraps efficient portfolio frontier, with or withour short sales, and minimum variance portfolio. 
# Protfolios are based on 3 assets (MSFT, APPLE and AMAZON). Data acquired from Yahoo Finance


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

#bootstrap returns 

m=20 #number of simulations

n=length(returns[,"MSFT"]) #number of observations in each sample
returns_sim <- list() #list of matrix returns, returns_sim[[j]] is a matrix rows=observations of assets at time t, cols=obsertaion of the asset
p <- matrix(0, ncol=3, nrow=n)

for(j in 1:m){
  for(k in 1:3){
    p[,k] <- sample(returns[,k], size=n, replace=TRUE)
    returns_sim[[j]] <- p
  }
}

#bootstrap mean vector and sigma
mu_sim <- list()
sigma_sim <-list()
for(j in 1:m){
  mu_sim[[j]] <- apply(returns_sim[[j]], 2, mean)
  sigma_sim[[j]] <- cov(returns_sim[[j]])
}

#plotting histograms

mu_sim_matrix <- matrix(unlist(mu_sim), ncol=3, byrow=TRUE)

par(mfrow=c(2,3))
hist(mu_sim_matrix[,1], col="blue", xlab="Microsoft", main="")
hist(mu_sim_matrix[,2], col="orange", xlab="Apple", main="")
hist(mu_sim_matrix[,3], col="brown", xlab="Amazon", main="")
plot(density(mu_sim_matrix[,1]), col="blue", xlab="Microsoft", main="")
plot(density(mu_sim_matrix[,2]), col="orange", xlab="Apple", main="")
plot(density(mu_sim_matrix[,3]), col="brown", xlab="Amazon",main="" ) 

#bootstrap efficient portfolio frontier

mu_val <- seq(from= 0, to=0.6, by=0.005)
sd_val_sim <- list()
X_ef <- list()
A_ef <- list()
t_ef <- list()

for(j in 1:m){
  
  top_mat <- cbind(2*sigma_sim[[j]], rep(1,times=3), mu_sim[[j]])
  mid_row <- c(rep(1, times=3), 0, 0)
  bot_row <- c(t(mu_sim[[j]]), 0, 0)
  
  A_ef[[j]] <- rbind(top_mat, mid_row, bot_row)
  
  Y <-matrix(0, ncol=3, nrow=length(mu_val))
  t_ef[[j]] <- rep(0, times=length(mu_val))
  
  
  for(i in 1:length(mu_val)){
    
    b <- solve(A_ef[[j]])%*%c(rep(0, times=3), 1, mu_val[i])
    Y[i,] <- b[1:3]
    sd_val[i] <- sqrt(t(Y[i,])%*%sigma_sim[[j]]%*%Y[i,])
    
  }
  
  sd_val_sim[[j]] <- sd_val
  X_ef[[j]] <- Y
  plot(sd_val_sim[[j]], mu_val, type="l")
  
} 


#bootstrap minimum variance portfolio

X_min <- list()
mu_min <-list()
sd_min <- list()


for(j in 1:m){
  
  top_mat <- cbind(2*sigma_sim[[j]], rep(1,times=3))
  mid_row <- c(rep(1, times=3), 0)
  
  
  B <- rbind(top_mat, mid_row)
  
  
  c <- solve(B)%*%c(rep(0, times=3), 1)
  
  X_min[[j]] <- c[1:3]
  
  mu_min[[j]] <- t(X_min[[j]])%*%mu_sim[[j]]
  sd_min[[j]] <- sqrt(X_min[[j]]%*%sigma_sim[[j]]%*%X_min[[j]])
  
  plot(sd_val_sim[[j]], mu_val, type="l")
  points(sd_min[[j]],  mu_min[[j]], type="p", col="red") 
}

#bootstrap tangency portfolio

r_f <- 0.03
I <- rep(1, times=length(mu))

x_tan <- list()
mu_tan <- list()
sd_tan <- list()
sh_tan <- list()


for(j in 1:m){
  
  x_tan[[j]] <- (solve(sigma_sim[[j]])%*%(mu_sim[[j]]-r_f*I))/(as.numeric(t(I)%*%solve(sigma_sim[[j]])%*%(mu_sim[[j]]-r_f*I)))
  mu_tan[[j]] <- t(x_tan[[j]])%*%mu_sim[[j]]
  sd_tan[[j]] <- sqrt(t(x_tan[[j]])%*%sigma_sim[[j]]%*%x_tan[[j]])
  
  sh_tan[[j]] <- (mu_tan[[j]]-r_f)/sd_tan[[j]]
  
  plot(sd_val_sim[[j]], mu_val, type="l")
  lines(c(sd_tan[[j]]),c(mu_tan[[j]]), type="p", col="blue")
  abline(a = r_f, b =sh_tan[[j]], col="yellow")
  lines(c(0,sd_tan[[j]]),c(r_f,mu_tan[[j]]), type="l", col="green")
}


#bootstrap with no short sales

mu_val_ns <- list()
sd_val_ns <- list()
Y_ns <- list()
A_ns <- list()


I <- rep(1, times=length(mu))

for(j in 1:m){
  
  l <-min(mu_sim[[j]])
  u <-max(mu_sim[[j]])
  
  mu_val_ns[[j]] <- seq(from= l, to=u, by=0.005)
  s <- rep(0, times=length(mu_val_ns[[j]]))
  U <-matrix(0, ncol=3, nrow=length(mu_val_ns[[j]]))
  
  
  V <- rbind(mu_sim[[j]], I, diag(3))
  A_ns[[j]] <- V
  d_ns <-  rep(0, times=3)
  
  
  for(i in 1:length(mu_val_ns[[j]])){
    
    b_ns <- c(mu_val_ns[[j]][i],1,rep(0, times=3))
    U[i,] <- solve.QP(sigma_sim[[j]], d_ns, t(A_ns[[j]]), b_ns, meq=2, FALSE)$solution
    s[i] <- sqrt(2*solve.QP(sigma_sim[[j]], d_ns, t(A_ns[[j]]), b_ns, meq=2, FALSE)$value)
  }
  
  Y_ns[[j]] <- U
  sd_val_ns[[j]] <- s
  plot(sd_val_sim[[j]], mu_val, type="l")
  lines(sd_val_ns[[j]], mu_val_ns[[j]], type="l", col="red")
  
}