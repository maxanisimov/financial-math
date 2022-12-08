call_price <- function(S, K, r, T, sigma) {
  d1  <-  (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2  <-  d1 - sigma*sqrt(T)
  return( S * pnorm(d1)  - K*exp(-r*T)*pnorm(d2) )
}

put_price <- function(S, K, r, T, sigma) {
  d1  <-  (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2  <-  d1 - sigma*sqrt(T)
  return( -S * pnorm(-d1) + K*exp(-r*T)*pnorm(-d2) )
}