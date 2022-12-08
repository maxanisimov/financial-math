library(WriteXLS)
library(readxl)
library(tidyverse)
library(zoo)
library(plotly)
theme_set(theme_light())

source("utils.R")

# Creating risk-free ####
# risk.free_path <- 'Netherlands 6-Month Bond Yield Historical Data.csv'
# risk.free <- read.csv(risk.free_path)
# risk.free <- risk.free %>% rename(Date=X...Date) %>% select(Date, Price)
# risk.free$Price <- risk.free$Price/100 # to decimals
# risk.free$Date <- as.Date(risk.free$Date, '%b %d, %Y')
# risk.free <- risk.free[order(nrow(risk.free):1),]
# WriteXLS(risk.free, 'risk_free.xlsx', 'rf in decimals')

# Open data ####
Shell_path <- 'data/Shell.xlsx'
shell.options <- as.data.frame(read_excel(Shell_path, skip=3, sheet = 'Options'))
shell.options <- shell.options[,c(1:2, 6)]
colnames(shell.options)
# TODO: aes does not work
# ggplot() + geom_line(shell.options, aes(x=Date, `RDAS122016C(MP)`))

shell.stock <- as.data.frame(read_excel(Shell_path, skip=4, sheet = 'Stock',
                          col_names = c('Date', 'Stock price', 'div yield')))
shell.stock$`div yield` <- shell.stock$`div yield`/100 # to decimals

shell.risk_free <- as.data.frame(read_excel(Shell_path, sheet = 'risk-free rate', skip=1,
                                            col_names=c('Date', 'risk-free')))

shell.df <- Reduce(function(x, y) merge(x, y, by='Date'),
                   list(shell.options, shell.stock, shell.risk_free))

put_call <- PutCallParity(
  Dates=shell.df$Date,
  call=shell.df$`RDAS122016C(MP)`,
  put=shell.df$`RDAS122016P(MP)`,
  strike=16,
  exercise_date='2020-12-18',
  stock=shell.df$`Stock price`,
  div_yield=shell.df$`div yield`, 
  risk_free=0
)

put_call_0 <- PutCallParity(
  Dates=shell.df$Date,
  call=shell.df$`RDAS122016C(MP)`, 
  put=shell.df$`RDAS122016P(MP)`,
  strike=16, 
  exercise_date='2020-12-18',
  stock=shell.df$`Stock price`,
  div_yield=0,
  risk_free=0
)


ggplot(put_call_0) + geom_point(aes(Date, Call_obs, color='observed')) + 
  geom_point(aes(Date, Call_parity, color='implied by parity')) + 
  labs(x="Date", y="Price", title='Call')

ggplot(put_call_0) + geom_point(aes(Date, Put_obs, color='observed')) + 
  geom_point(aes(Date, Put_parity, color='implied by parity')) + 
  labs(x="Date", y="Price", title='Put')


# Find the implied volatility ####
maturity <- as.numeric(as.Date('2020-12-18')-as.Date('2019-10-01'))/365
C <- call_price(S=26.9, K=16, r=0, T=maturity, sigma=0.1)
print(
  paste("Call price =", C)
)
P <- put_price(S=26.9, K=16, r=0, T=maturity, sigma=0.1)
print(
  paste("Put price =", P)
)

# TODO
tau <- (as.numeric(as.Date('2020-12-18') - as.Date(shell.df$Date)))/365 # in years
min_deviation <- Inf
IV <- NaN
for (sigma in seq(0.01, 0.50, 0.005)){
  theo_call_price <- call_price(S=shell.df$`RDAS122016C(MP)`, K=16, r=0, T=tau, sigma=sigma)
  cur_deviation <- abs(theo_call_price - C)
  if (cur_deviation < min_deviation){
    min_deviation <-cur_deviation
    IV <- sqrt(sigma)
  }
}
# TODO: old code, remove?
# VperSigma <- call_price(S=shell.df$`RDAS122016C(MP)`, K=16, r=0, T=tau, sigma=seq(0.01,0.50,0.005))
# IV <- which.min(abs(VperSigma-C))/2
print(paste("Implicit Volatility is around", IV, "%"))

