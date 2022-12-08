po_prepare <- function(gross_returns) {
  
  Sigma <- cov(gross_returns) # covariance matrix of gross returns (NxN matrix)
  Sigma_inverted <- solve(Sigma) # NxN matrix
  stopifnot(sum(near(Sigma%*%Sigma_inverted, diag(dim(Sigma)[1])) == FALSE) == 0) # assert with the inverse def
  
  ones_vector <- as.matrix(c(rep(1, dim(Sigma)[1]))) # ones vector (N+1)x1
  
  A <- (t(mu)%*%Sigma_inverted%*%mu)[1,1]
  B <- (t(mu)%*%Sigma_inverted%*%ones_vector)[1,1]
  C <- (t(ones_vector)%*%Sigma_inverted%*%ones_vector)[1,1]
  
  prep = list(
    Sigma_inverted = Sigma_inverted,
    ones_vector = ones_vector,
    A = A,
    B = B,
    C = C
  )
  
  return(prep)
  
}

calculate_gmv_portfolio <- function(gross_returns) {
  
  #' Computes the Global Minimum Variance (GMV) portfolio
  
  prep = po_prepare(gross_returns)
  Sigma_inverted = prep$Sigma_inverted
  ones_vector = prep$ones_vector
  A = prep$A
  B = prep$B
  C = prep$C
  
  pi_gmv <- (1/C)*Sigma_inverted%*%ones_vector # GMV portfolio weights
  stopifnot(near(sum(pi_gmv), 1) == TRUE) # check constraint
  return_gmv <- B/C # average GMV portfolio gross return
  sigma2_gmv <- 1/C # return variance of GMV portfolio (in decimals!)
  
  result <- list(weights = pi_gmv, average_return = return_gmv, variance = sigma2_gmv)
  
  return(result)
}

calculate_mu_portfolio <- function(gross_returns){
  #' Computes the mu portfolio
  
  prep = po_prepare(gross_returns)
  Sigma_inverted = prep$Sigma_inverted
  ones_vector = prep$ones_vector
  A = prep$A
  B = prep$B
  C = prep$C
  
  mu <- as.matrix(colMeans(gross_returns)) # average gross returns for each asset ((N+1)x1 vector)
  
  pi_mu <- (1/B)*Sigma_inverted%*%mu # mu portfolio weights
  stopifnot(near(sum(pi_mu), 1)) # check constraint
  return_mu <- A/B # mu portfolio expected gross return
  sigma2_mu <- A/(B^2) # return variance of mu portfolio (in decimals!)
  
  result <- list(weights = pi_mu, average_return = return_mu, variance = sigma2_mu)
  
  return(result)
  
}

calculate_tangency_portfolio <- function(gross_returns, risk_free_returns){
  
  excess_returns <- gross_returns - (1+risk_free_returns)
  excess_mu <- as.matrix(colMeans(excess_returns))
  
  Sigma <- cov(gross_returns) # covariance matrix of gross returns (NxN matrix)
  Sigma_inverted <- solve(Sigma) # NxN matrix
  
  prep = po_prepare(gross_returns)
  Sigma_inverted = prep$Sigma_inverted
  ones_vector = prep$ones_vector
  A = prep$A
  B = prep$B
  C = prep$C
  
  pi_tang <- (1/(t(ones_vector)%*%Sigma_inverted%*%excess_mu)[1,1])*Sigma_inverted%*%excess_mu
  stopifnot(near(sum(pi_tang), 1))
  return_tang <- (mean(risk_free_returns) + t(pi_tang)%*%excess_mu)[1,1] # mean Rf + excess tang portf return 
  excess_return_tang <- ((t(excess_mu)%*%Sigma_inverted%*%excess_mu)/(t(ones_vector)%*%Sigma_inverted%*%excess_mu))[1,1]
  stopifnot(near(excess_return_tang + mean(risk_free_returns), return_tang)) # check
  std_tang <- (abs(excess_return_tang)/sqrt(t(excess_mu)%*%Sigma_inverted%*%excess_mu))[1,1]
  
  result <- list(
    weights = pi_tang,
    average_return = return_tang,
    average_excess_return = excess_return_tang,
    std = std_tang
  )
  
  return(result)
  
}


#### GRS tests ####
finite_sample_grs <- function(market_mean, market_std, T_period, alpha_hat, Sigma_tilde){
  market_sharpe <- market_mean/market_std
  n <- dim(Sigma_tilde)[1]
  z <- (T_period-n-1)/n * (1 + market_sharpe^2)^(-1) * (t(as.matrix(alpha_hat))%*%solve(Sigma_tilde)%*%as.matrix(alpha_hat))[1,1]
  pvalue <- 1-pf(z, df1=n, df2=T_period-n-1) # TODO: double-check dfs
  
  result <- list(statistic = z, pvalue = pvalue)
  
  return(result)
}

finite_sample_grs2 <- function(factors_mean, factors_cov, T_period, alpha_hat, Sigma_tilde){
  n <- dim(Sigma_tilde)[1]
  k <- dim(factors_mean)[1]
  sharpe_squared <- as.numeric(t(as.matrix(factors_mean))%*%solve(factors_cov)%*%as.matrix(factors_mean))
  z = (T_period-n-k)/n * (1 + sharpe_squared)^(-1) * (t(as.matrix(alpha_hat))%*%solve(Sigma_tilde)%*%as.matrix(alpha_hat))[1,1]
  pvalue <- 1-pf(z, df1=n, df2=T_period-n-k) 
  
  result <- list(statistic = z, pvalue = pvalue)
  
  return(result)
}

asymptotic_grs <- function(market_mean, market_std, T_period, alpha_hat, Sigma_tilde){
  market_sharpe <- market_mean/market_std
  z <- T_period/(1 + market_sharpe^2) * (t(as.matrix(alpha_hat))%*%solve(Sigma_tilde)%*%as.matrix(alpha_hat))[1,1]
  n <- dim(Sigma_tilde)[1]
  pvalue <- 1-pchisq(z, df=n) # TODO: double-check dfs
  
  result <- list(statistic = z, pvalue = pvalue)
  
  return(result)
}

asymptotic_grs2 <- function(factors_mean, factors_cov, T_period, alpha_hat, Sigma_tilde){
  sharpe_squared <- as.numeric(t(as.matrix(factors_mean))%*%solve(factors_cov)%*%as.matrix(factors_mean))
  z <- T_period/(1 + sharpe_squared) * (t(as.matrix(alpha_hat))%*%solve(Sigma_tilde)%*%as.matrix(alpha_hat))[1,1]
  n <- dim(Sigma_tilde)[1]
  pvalue <- 1-pchisq(z, df=n)
  
  result <- list(statistic = z, pvalue = pvalue)
  
  return(result)
}

fama_macbeth_test <- function(factor_exposures){
  # test
  avg_factor_exposure <- mean(factor_exposures)
  T_period <- length(factor_exposures)
  var_factor_exposure <- sum((factor_exposures - avg_factor_exposure)^2)/T_period
  t_stat <- avg_factor_exposure/sqrt(var_factor_exposure)
  pvalue <- ( 1 - pt(t_stat, df=T_period-1) ) / 2

  result <- list(t_stat = t_stat, pvalue = pvalue)
  
  return(result)
  
}