# Libraries ####
library(qrmtools)
library(readxl)
library(tidyverse)
library(mixtools) # mixture models
library(tseries)
library(rugarch) # GARCH
library(qrmtools)
library(QRM)
library(copula)
#library(VGAM)
#library(Kendall)

setwd('/Users/maxim_anisimov/Desktop/ERASMUS/Studying/2 module/Risk Management/HA/')

# Functions ####
VaR.EVT <- function(data, p, k, alpha){
  data <- sort(data)
  n <- length(data)
  VaR.p_estimate <- data[n-k] * ( k/(n*(1-p)) )^(1/alpha)
  return(VaR.p_estimate)
}

alpha.Hill <- function(data, k){
  data <- sort(data)
  n <- length(data)
  alpha <- c()
  for (i in 1:length(k)){
    cur_k <- k[i]
    peaks <- data[(n-cur_k+1):length(data)]
    threshold <- data[n-cur_k]
    if (threshold <= 0) stop('threshold must be non-negative!')
    
    alpha[i] <- 1 / (1/cur_k * sum(log(peaks)) - log(threshold))
  }
  return(alpha)
} 

lower.tail.dependence <- function(X1, X2, k){
  
  X1 <- as.vector(X1)
  X2 <- as.vector(X2)
  n <- length(X2)
  
  X1.sorted <- sort(X1)
  X2.sorted <- sort(X2)
  
  lambda_u <- c()
  for (i in 1:length(k)){
    cur_k <- k[i]
    
    sum <- 0
    for (t in 1:n){
      X1.bool <- X1[t] > X1.sorted[n-cur_k]
      X2.bool <-  X2[t] > X2.sorted[n-cur_k]
      sum <- sum + X1.bool * X2.bool
    }
    
    lambda_u[i] <- 1/cur_k * sum
    
  }

  return(lambda_u)
}

# Settings ####
"
Be aware that the data series are loss returns. 
We are interested in the right tail!
loss returns unit: percentage! 
"
p <- 0.99
w_1 <- 0.7
w_2 <- 0.3

# Data #### 
loss_returns <- read_excel('527144_546909.xlsx', sheet='Data')
loss_returns <- as.data.frame(loss_returns[,1:2])

portfolio_loss_returns <- w_1*loss_returns$Stock1 + w_2*loss_returns$Stock2

# 1. Variance–covariance approach based on the two series ####

means <- colMeans(loss_returns)
portfolio_mean <- w_1*means[1] + w_2*means[2]
cov_matrix <- var(loss_returns)
portfolio_var <- w_1^2*cov_matrix[1,1] + w_2^2*cov_matrix[2,2] + 
  2*w_1*w_2*cov_matrix[1,2]

loss_return_VaR.VCOV <- qnorm(p=p, mean=portfolio_mean, sd=sqrt(portfolio_var))

# VaR(99%)
loss_return_VaR.VCOV # in %


# 2. Historical Simulation based on the portfolio loss returns ####
plr.ordered <- portfolio_loss_returns %>% sort()
loss_return_VaR.HS <- plr.ordered[0.99*length(plr.ordered)]

# VaR(99%)
loss_return_VaR.HS # in %

# 3. A normal mixture model mixing two normal distributions ####
# Hint: Conduct a JB test to check the normality of the portfolio loss return

jarque.bera.test(portfolio_loss_returns) # returns are not normal!

res <- normalmixEM(portfolio_loss_returns, k=2)

res$mu # two means
res$sigma # two STANDARD DEVIATIONS
res$lambda # the probabilities of the two scenarios 

# Simulate portfolio loss returns
simGM <- rnormmix(10^5, res$lambda, res$mu, res$sigma)
simGM.ordered <- simGM %>% sort()
loss_return_VaR.NMM <- simGM.ordered[0.99*length(simGM.ordered)]

# VaR(99%)
loss_return_VaR.NMM # in %

# 4. An EVT approach ####
# Hint: Use the same k for estimating the tail index and the VaR

k_vector <- seq.int(10, 2000-1020, 1)
alpha_vector <- alpha.Hill(portfolio_loss_returns, k=k_vector)

plot(k_vector, alpha_vector, type='l')
plot(k_vector[10:200], alpha_vector[10:200], type='l')
k <- 40 # consider k such that trend is not clear but k is not too small
alpha <- alpha_vector[k_vector==k]
loss_return_VaR.EVT <- VaR.EVT(portfolio_loss_returns, p=0.99, 
                               k=k, alpha=alpha)

# VaR(99%)
loss_return_VaR.EVT # in %

# 5. Fitting a GARCH model with normal innovations (QMLE) ####
# and then predict the next day VaR using the dynamic historical simulation method
spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE))

garch.fit <- ugarchfit(spec, portfolio_loss_returns)
garch.sigma <- garch.fit@fit[["sigma"]]
sum(round(garch.fit@fit[["var"]],2) != round(garch.fit@fit[["sigma"]]^2,2))
summary(garch.fit)
#garch.residuals <- residuals(garch.fit) %>% as.numeric() %>% sort()
#
# filtered Z_t = (X_t-mean)/sigma_t
garch.residuals_filtered <- sort( (portfolio_loss_returns-(-0.078099))/garch.sigma )
plot(garch.residuals_filtered)
mean(garch.residuals_filtered)
resid_q99 <- garch.residuals_filtered[0.99*length(garch.residuals_filtered)]

garch.fcst <- ugarchforecast(garch.fit, n.ahead=1, 
                             data=portfolio_loss_returns)
#sqrt( 0.178889 + 0.058498*portfolio_loss_returns[length(portfolio_loss_returns)]^2 +
#      0.932551 *sigma_{n}^{2} )

garch.fcst
sigma.fcst <- as.numeric(sigma(garch.fcst))

# VaR(99%) ???
loss_return_VaR.GARCH <- sigma.fcst * resid_q99  + (-0.078099) # in %
loss_return_VaR.GARCH

# 6. Fitting the two series to a bivariate distribution ####
#  with the marginals follow (different) Student–t distributions and 
#  the dependence is modeled by a reverse Clayton copula. Here the reverse
#   means that (1 − F1(X1), 1 − F2(X2)) follows the Clayton copula. 
#   In that way, (X1, X2) possesses upper tail dependence.
# Hint:
# 1) To fit the Clayton copula, use the moment method based on the Kendall’s τ 
# 2) Estimate the upper tail dependence from the data to validate the model


# fit distribution marginals as t for each stock
tfit.1 <- fit.st(loss_returns$Stock1)
tfit.2 <- fit.st(loss_returns$Stock2)
# Define tpars, nu, mu, and sigma
tpars.1 <- tfit.1$par.ests
tpars.2 <- tfit.2$par.ests


# calculate Kendell's tau
cor(loss_returns$Stock1, loss_returns$Stock2, method='kendall')
kendall_tau <- QRM::Kendall(loss_returns$Stock1, loss_returns$Stock2)

# theta to calculate tail dependece
clayton.theta <- 2/(1-kendall_tau) - 2

# calculate tail dependence (see your calculations)

clayton.lower_tail_dep <- 1 / 2^(1/clayton.theta)

# choose k accoring to plot -> calculate tail dependence
k_td_vector <- seq.int(10, 1990, 1)
lambda_u_vector <- lower.tail.dependence(loss_returns$Stock1, loss_returns$Stock2, 
                                         k=k_td_vector)

plot(k_td_vector, lambda_u_vector, type='l')
plot(k_td_vector[10:500], lambda_u_vector[10:500], type='l') # 300?
lambda_u <- lambda_u_vector[k_td_vector==300]

theta <-  


# Monte-Carlo simulation from Clayton copula  