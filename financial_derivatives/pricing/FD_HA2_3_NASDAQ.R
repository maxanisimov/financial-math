library(WriteXLS)
library(readxl)
library(tidyverse)
library(zoo)
library(plotly)
library(reshape2)
library(plyr)
library(stats)
library(derivmkts) # BS formulas
theme_set(theme_light())

source("utils.R")

# Open data ####
nasdaq_path <- 'data/NASDAQ_options.csv'
nasdaq_data <- read.csv(nasdaq_path)

nasdaq_data$date <- as.Date(as.character(nasdaq_data$date), '%Y%m%d')
nasdaq_data$exdate <- as.Date(as.character(nasdaq_data$exdate), '%Y%m%d')
nasdaq_data$strike_price <- nasdaq_data$strike_price/1000 # 1000*K/1000 = K
nasdaq_data$price <- (nasdaq_data$best_bid + nasdaq_data$best_offer)/2 # proxy for option price

stopifnot(unique(nasdaq_data$exercise_style) == 'E') # only European options :)


nasdaq_data <- nasdaq_data %>% select(date, exdate, cp_flag, strike_price, price) %>%
  filter(date == '2015-06-01')
nasdaq_data$maturity <- (as.numeric(as.Date(nasdaq_data$exdate, '%Y-%m-%d') - 
                                as.Date(nasdaq_data$date, '%Y-%m-%d')))/365 # in years
nasdaq_data <- nasdaq_data %>% select(-c(date, exdate))

S_0 <- 4521.85 # https://www.investing.com/indices/nq-100-historical-data
r <- 0.01/100 # United States 3-Month Bond Yield
q <- 1.375/100 # https://www.dividend.com/how-to-invest/the-nasdaq-a-dividend-overview/

# Task 1 ####
nasdaq_call <- nasdaq_data %>% filter(cp_flag == 'C') %>% select(-cp_flag)
nasdaq_put <- nasdaq_data %>% filter(cp_flag == 'P') %>% select(-cp_flag)
parity_result <- PutCallParity.1(call_data=nasdaq_call, put_data=nasdaq_put, 
                stock=S_0, div_yield=q, risk_free=r)
parity_result[[1]]
parity_result[[2]]

all_parity_results <- merge(parity_result[[1]], parity_result[[2]],
                            by=c('strike_price', 'maturity'))
colnames(all_parity_results)[c(3:6)] <- c('call observed', 'put (parity)',
                                           'put observed', 'call (parity)')

# Call
call_parity <- all_parity_results %>% 
               select(strike_price, maturity, `call observed`, `call (parity)`) %>%
               melt(id.vars = c('strike_price', 'maturity'))
colnames(call_parity)[3:4] <- c('Call', 'Price')

call_price_plot <- plot_ly(call_parity, x = ~maturity, y = ~strike_price, z = ~Price,
                           color = ~Call, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Maturity, years'),
                      yaxis = list(title = 'Strike'),
                      zaxis = list(title = 'Price')))
call_price_plot
filter(call_parity, Price < 0) 

# Put
put_parity <- all_parity_results %>% 
              select(strike_price, maturity, `put observed`, `put (parity)`) %>%
              melt(id.vars = c('strike_price', 'maturity'))
colnames(put_parity)[3:4] <- c('Put', 'Price')

put_price_plot <- plot_ly(put_parity, x = ~maturity, y = ~strike_price,
                          z = ~Price, color = ~Put, 
                  colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Maturity, years'),
                      yaxis = list(title = 'Strike'),
                      zaxis = list(title = 'Price')))
put_price_plot

filter(put_parity, Price < 0) 

# Task 2 ####

# Call
bscallimpvol.mine <- function(s, k, r, tt, d, price) {
  ## this function is not vectorized
  f <- function(v, s, k, r, tt, d, price) {
    return(derivmkts::bscall(s, k, v, r, tt, d) - price)
  }
  "The function uniroot searches the interval from lower to upper for a root (i.e., zero) 
  of the function f with respect to its first argument."
  x <- uniroot(f, c(0.0001,10000), extendInt = 'yes', s, k, r, tt, d, price)
  return(x$root)
}

BS_call_IV <- c()
for (row in 1:nrow(nasdaq_call)){
  IV <- bscallimpvol.mine(s=S_0, k=nasdaq_call$strike_price[row], r=r,
             tt=nasdaq_call$maturity[row], d=q, price=nasdaq_call$price[row])
  print(IV)
  if (is.character(IV) == TRUE) {
    IV <- NA
  }
  BS_call_IV <- c(BS_call_IV, IV)
}

sum(is.na(BS_call_IV))
nasdaq_call$IV <- BS_call_IV

IV_loess.call <- loess(data=nasdaq_call, IV ~ strike_price + maturity)

strikes_sorted.call <- sort(unique(nasdaq_call$strike_price))
maturities_sorted.call <- sort(unique(nasdaq_call$maturity))

call_IV_3D <- matrix(NA_real_, nrow=length(strikes_sorted.call), 
                               ncol=length(maturities_sorted.call))
for (strike_num in 1:length(strikes_sorted.call)){
  for (mat_num in 1:length(maturities_sorted.call)){
    cur_df <- nasdaq_call %>% filter(strike_price==strikes_sorted.call[strike_num],
                                     maturity==maturities_sorted.call[mat_num])
    if (dim(cur_df)[1] == 1){
      call_IV_3D[strike_num, mat_num] <- as.numeric(cur_df$IV[1])
      if (as.numeric(cur_df$IV[1]) < 0){
        print(cur_df)
      }
    }
    else {
      newdata = data.frame(strike_price=strikes_sorted.call[strike_num],
                           maturity=maturities_sorted.call[mat_num])
      IV.pred = predict(IV_loess.call, newdata)
      call_IV_3D[strike_num, mat_num] <- as.numeric(IV.pred)
    }
  }  
}

plot_ly(x=maturities_sorted.call, y=strikes_sorted.call, z=call_IV_3D, type="surface") %>%
  plotly::layout(scene=list(
      xaxis=list(title='Maturity, years'), yaxis=list(title='Strike'),
      zaxis=list(title='Call IV')))


# Put
bsputimpvol.mine <- function(s, k, r, tt, d, price) {
  ## this function is not vectorized
  f <- function(v, s, k, r, tt, d, price) {
    return(derivmkts::bsput(s, k, v, r, tt, d) - price)
  }
  x <- uniroot(f, c(0.0001,10000), extendInt = 'yes', s, k, r, tt, d, price)
  return(x$root)
}

BS_put_IV <- c()
for (row in 1:nrow(nasdaq_put)){
  IV <- bsputimpvol.mine(s=S_0, k=nasdaq_put$strike_price[row], r=r,
                     tt=nasdaq_put$maturity[row], d=q, price=nasdaq_put$price[row])
  if (is.character(IV) == TRUE) {
    print(IV)
    IV <- NA
  }
  BS_put_IV <- c(BS_put_IV, IV)
}

sum(is.na(BS_put_IV))

nasdaq_put$IV <- BS_put_IV
IV_loess.put <- loess(data=nasdaq_put, IV ~ strike_price + maturity)

strikes_sorted.put <- sort(unique(nasdaq_put$strike_price))
maturities_sorted.put <- sort(unique(nasdaq_put$maturity))

put_IV_3D <- matrix(NA_real_, nrow=length(strikes_sorted.put), 
                              ncol=length(maturities_sorted.put))
for (strike_num in 1:length(strikes_sorted.put)){
  for (mat_num in 1:length(maturities_sorted.put)){
    cur_df <- nasdaq_put %>% filter(strike_price==strikes_sorted.put[strike_num],
                                     maturity==maturities_sorted.put[mat_num])
    if (dim(cur_df)[1] == 1){
      put_IV_3D[strike_num, mat_num] <- as.numeric(cur_df$IV[1])
    }
    else {
      newdata = data.frame(strike_price=strikes_sorted.put[strike_num],
                           maturity=maturities_sorted.put[mat_num])
      IV.pred = max(0, predict(IV_loess.put, newdata))
      put_IV_3D[strike_num, mat_num] <- as.numeric(IV.pred)
    }
  }  
}

plot_ly(x=maturities_sorted.put, y=strikes_sorted.put, z=put_IV_3D, type="surface") %>%
  plotly::layout(scene=list(
    xaxis=list(title='Maturity, years'), yaxis=list(title='Strike'),
    zaxis=list(title='Put IV')))


# Task 3 ####
# a
"It is known that Black-Scholes model assumes a constant volatility parameter. 
Propose an estimator for the Black-Scholes volatility parameter using the implied
volatilities obtained in point 2."

mean.sigma <- mean(c(nasdaq_call$IV, nasdaq_put$IV)) 

# b
"Use the estimated volatility parameter in point (a) to 
price the European calls and puts with the same strikes and maturities as the observed 
options. Plot the pricing errors between the Black-Scholes prices and the empirical prices
in a 3D-scatter plot, separately for the puts and calls. 
How do you assess the quality of estimation?"

# Call
call_prices_3b <- bscall(s=S_0, k=nasdaq_call$strike_price, r=r,
                         tt=nasdaq_call$maturity, v=mean.sigma, d=q)
nasdaq_call$mean_sigma_error <- call_prices_3b - nasdaq_call$price
call_error_plot.3b <- plot_ly(nasdaq_call, x = ~maturity, y = ~strike_price, 
                           z = ~mean_sigma_error) %>% 
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Maturity, years'),
                      yaxis = list(title = 'Strike'),
                      zaxis = list(title = 'Pricing error')))
call_error_plot.3b
call_max_error_ind <- which(nasdaq_call$mean_sigma_error == 
                              min(nasdaq_call$mean_sigma_error))
nasdaq_call$strike_price[call_max_error_ind]
S_0

# Put
put_prices_3b <- bsput(s=S_0, k=nasdaq_put$strike_price, r=r,
                         tt=nasdaq_put$maturity, v=mean.sigma, d=q)
nasdaq_put$mean_sigma_error <- put_prices_3b - nasdaq_put$price
put_error_plot.3b <- plot_ly(nasdaq_put, x = ~maturity, y = ~strike_price, 
                              z = ~mean_sigma_error) %>% 
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Maturity, years'),
                      yaxis = list(title = 'Strike'),
                      zaxis = list(title = 'Pricing error')))
put_error_plot.3b
put_max_error_ind <- which(nasdaq_put$mean_sigma_error == 
                              min(nasdaq_put$mean_sigma_error))
nasdaq_put$strike_price[call_max_error_ind]
S_0

# c
"We take data for previous year and estimate variance of log returns"
nasdaq_quotes <- read.csv('NDX.csv')
stock_sd <- sd(log(nasdaq_quotes$Adj.Close) - 
      log(lag(nasdaq_quotes$Adj.Close, 1)), na.rm = T)
stock_sigma <- stock_sd * sqrt(nrow(nasdaq_quotes))

# Call
call_prices_3c <- bscall(s=S_0, k=nasdaq_call$strike_price, r=r,
                       tt=nasdaq_call$maturity, v=0.1, d=q)
nasdaq_call$historical_sigma_error <- call_prices_3c - nasdaq_call$price
call_error_plot.3c <- plot_ly(nasdaq_call, x = ~maturity, y = ~strike_price, 
                           z = ~historical_sigma_error) %>% 
                           add_markers() %>%
                            layout(scene = list(xaxis = list(title = 'Maturity, years'),
                                                yaxis = list(title = 'Strike'),
                                                zaxis = list(title = 'Pricing error')))
call_error_plot.3c
call_max_error_ind <- which(nasdaq_call$historical_sigma_error == 
                          min(nasdaq_call$historical_sigma_error))
nasdaq_call$strike_price[call_max_error_ind]
S_0

# Put
put_prices_3c <- bsput(s=S_0, k=nasdaq_put$strike_price, r=r,
                       tt=nasdaq_put$maturity, v=stock_sigma, d=q)
nasdaq_put$historical_sigma_error <- put_prices_3c - nasdaq_put$price

put_error_plot.3c <- plot_ly(nasdaq_put, x = ~maturity, y = ~strike_price, 
                           z = ~historical_sigma_error) %>% 
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Maturity, years'),
                      yaxis = list(title = 'Strike'),
                      zaxis = list(title = 'Pricing error')))
put_error_plot.3c

put_max_error_ind <- which(nasdaq_put$historical_sigma_error == 
                              min(nasdaq_put$historical_sigma_error))
nasdaq_put$strike_price[put_max_error_ind]
S_0

# Task 4 ####
S_0
nasdaq_put[which.min(abs(S_0-nasdaq_put$strike_price)/S_0 +
                       abs(1/3-nasdaq_put$maturity)*3),]

sigma_task4 <- bsputimpvol.mine(s=S_0, k=4525, r=r, tt=0.2986301, d=q, price=150.95)
d_1q <- (( r-q + 0.5*sigma_task4^2)*1/3)/(sigma_task4*sqrt(1/3))
P_asset <- S_0 * exp(-q*1/3) * pnorm(-d_1q)
