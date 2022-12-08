## Download libraries ####
library(tidyverse)
library(forecast)
library(tseries)
library(xts)
library(ggplot2)
library(reshape2)
library(corrplot)
library(xts)
library(readxl)
library(plotly)
library(Metrics)
library(matrixStats)
library(ggpubr) # for the best grids
library(WriteXLS)
theme_set(theme_light())

source("utils.R")

## Set parameters ####
file_path <- 'data/EM_YC_values.xlsx'
dns_lambda <- 0.0609
maturity_vector <- c(3, 6, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120)

# ANALYSIS #######

# Open and preprocess data ####
yields <- read_excel('data/German_YC.xlsx', skip = 1,
                         col_names = c('Date', paste0('M', maturity_vector)))
yields.xts <- xts::xts(yields[,-1], order.by=as.Date(yields$Date))

( 1-exp(-dns_lambda*maturity_vector) ) / ( dns_lambda*maturity_vector) 

NS.estimate(yields.xts[1,], maturity_vector, dns_lambda, include_curvature=TRUE)

# Provide plot(s) of the data ####
# Plot 3D yield curve
yc3d <- plot_3D_YC(yields.xts, maturity_vector)
yc3d
#ggsave('yc3d.png', dpi=300, width=10, height=5)

# Plot some yields (most representative and interesting)
yields_to_plot <- melt(yields[,c('Date', 'M3', 'M12', 'M24', 'M60', 'M84', 'M120')], 
                       id.vars = 'Date', variable.name = 'maturity')

maximum.y <- 1.05*max(yields_to_plot$value)
minimum.y <- 1.1*min(yields_to_plot$value) # because it is negative

yields_plot <- ggplot(yields_to_plot, aes(as.Date(Date), value)) + 
  geom_line(aes(colour = maturity), size=1) +
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5)) + 
  coord_cartesian(ylim = c(minimum.y, maximum.y), expand = FALSE) + 
  labs(x='Date', y='Yield, %') + guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  scale_x_date(date_breaks = '5 years', date_labels = "%Y") +
  scale_color_brewer(palette="Dark2")
yields_plot
#ggsave('yields_plot.png', dpi=300, width=10, height=5)


# Summary statistics ####
# Average YC
stats_yc <- plot_stat_yc(yields=yields.xts, maturities=maturity_vector,
                            country_name = 'Germany')
stats_yc
#ggsave('stats_yc.png', dpi=300, width=7, height=5)

# Get table with stats
means <- round(colMeans(yields.xts),2)
medians <- round(colMedians(yields.xts),2)
q_25 <- round(colQuantiles(yields.xts, probs=0.25),2)
q_75 <- round(colQuantiles(yields.xts, probs=0.75),2)
st_devs <- round(sqrt(diag(var(yields.xts))),2)

acf.matrix <- c()
for (col in colnames(yields.xts)){
  current_acf <- acf(yields.xts[,col], lag.max=30, plot=FALSE)[c(1:12, 30)]$acf
  acf.matrix <- rbind(acf.matrix, current_acf)
}
row.names(acf.matrix) <- colnames(yields.xts)
colnames(acf.matrix) <- paste0('rho_', c(1:12, 30))
acf.matrix <- round(acf.matrix,3)

ACF_plot <- corrplot(acf.matrix, method='color', type="full",
         addCoef.col="white", tl.col="black", tl.srt=30,
         number.cex=0.7, tl.cex = .6)
ACF_plot
#ggsave('ACF_plot.png', dpi=300, width=10, height=5)

# Here it is
stats_table <- cbind(means, medians, st_devs, q_25, q_75, acf.matrix[,c(1,12,13)])
#WriteXLS(as.data.frame(stats_table), ExcelFileName = "stats_table.xls", row.names=T)

# Cross-sectional correlations
cor_matrix <- cor(yields.xts)
#WriteXLS(as.data.frame(cor_matrix), ExcelFileName = "cor_matrix.xls", row.names=T)

corr_plot <- corrplot(cor_matrix, method='color', type="full",
                      addCoef.col="white", tl.col="black", tl.srt=30,
                      number.cex=0.7, tl.cex = .6)
corr_plot

# Does the yield curve satisfy the stylized facts mentioned in class? ####

"
(i) The average yield curve over time is increasing and concave?
NO! increasing but no that concave
"
stats_yc

"
(ii) The yield curve can take on a variety of shapes (upward sloping, 
downward sloping, humped, inverted humped, S-shapes)?

YES! Compare 1994-12-30 and 2003-03-31 below!
"
n_row <- 1
some_yc <- as.data.frame(t(yields.xts))[n_row]
some_yc$maturity <- maturity_vector
date <- colnames(some_yc)[1]
colnames(some_yc)[1] <- 'yield_dec1994'
n_row <- 100
some_yc[,'yield_march2003'] <- as.vector(yields.xts[n_row,])

yc_different_dates <- ggplot(data=some_yc) + 
  geom_line(aes(x=maturity, y=yield_dec1994, col='Dec 1994'), size=1) + 
  geom_line(aes(x=maturity, y=yield_march2003, col='March 2003'), size=1) + 
  labs(x='Maturity, months', y='Yield, %') + 
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(title="Date"))
yc_different_dates
#ggsave('yc_different_dates.png',  dpi=300, width=10, height=5)

for (n_row in seq.int(1, nrow(yields.xts), by=20)) {
  cur_yc <- as.data.frame(t(yields.xts))[n_row]
  cur_yc$maturity <- maturity_vector
  date <- colnames(cur_yc)[1]
  colnames(cur_yc)[1] <- 'yield'
  
  cur_plot <- ggplot(data=cur_yc) + 
    geom_line(aes(x=maturity, y=yield), size=1) + labs(title=date)
  print(cur_plot)
  #labs(x='Maturity, months', y='Yield, %') + 
  #theme(legend.position = 'bottom') + 
  #guides(color=guide_legend(title="Date"))
 
}

"
(iii) Yield dynamics are (very) persistent (high auto-correlations)
YES! All first-order AC >= 0.98, AC with lag of 12 months >= 0.79. 
Even for the lag of 30 months AC >= 0.57.
"
stats_table[,c('ro_1', 'ro_12', 'ro_30')]


"
(iv) Yields for long maturities are more persistent than yields
for shorter maturities
Debatable
"
ACF_plot

"
(v) The short end of the yield curve is more volatile than the
long end of the curve
NO! The most volatile yields have maturity of 6-8 years. ST yield
are the most stable!
"
stats_table[,c('st_devs')]

std_plot <- ggplot(data=data.frame(std=stats_table[,c('st_devs')],
                   maturity=maturity_vector), aes(x=maturity, y=std)) + 
                   geom_line(size=1) + labs(x='Maturity, months',
                                            y='Standard Deviaton, pp')
std_plot
#ggsave('std_plot.png', dpi=300, width=10, height=5)

"
(vi) Yields for different maturities have high cross-correlations
YES!
"
corr_plot


# Report the estimates of the parameters of the DNS model ####
# and a discussion
# of whether the model at the estimated parameter values is able to capture the
#stylized facts of the yield curve.

NS_betas <- NS.fixed_lambda(rate=yields.xts, maturity_vector, dns_lambda)

round(colMeans(NS_betas), 3)
round(sqrt(diag(var(NS_betas))),3)

level.plot <- ggplot(NS_betas) + geom_line(aes(x=Index, y=level)) + 
              labs(x='Date', y='Level', 'Level')
slope.plot <- ggplot(NS_betas) + geom_line(aes(x=Index, y=slope)) + 
             labs(x='Date', y='Slope', 'Slope')
curvature.plot <- ggplot(NS_betas) + geom_line(aes(x=Index, y=curvature)) + 
  labs(x='Date', y='Curvature', 'Curvature')

ggpubr::ggarrange(level.plot, slope.plot, curvature.plot, nrow=3,
                  align='hv', label.x='Date')
# ggsave('factors.png')

# no dynamics: use pure estimates
NS_yc <- NS_curve(dates=yields$Date, 
                  level=NS_betas$level, slope=NS_betas$slope, curvature=NS_betas$curvature,
                  lambda=dns_lambda, maturities=maturity_vector, prefix='M')
round(colMeans(NS_yc), 3)
round(sqrt(diag(var(NS_yc))),3)

stats_yc.dns <- plot_stat_yc(yields=NS_yc, maturities=maturity_vector,
                         country_name = 'Germany')
stats_yc.dns


# Forecast factors 1 step ahead -> DNS model with AR(1)
ar1.estimates <- matrix(NA, 2,3)
colnames(ar1.estimates) <- c('level', 'slope', 'curvature')
row.names(ar1.estimates) <- c('drift', 'ar1_coef')
for (factor in c('level', 'slope', 'curvature')){
  
  y <- as.matrix(NS_betas[,factor])
  const <- rep(1, dim(y)[1]-1)
  y_ar <- lag(NS_betas[,factor],1)
  y_ar <- as.vector(y_ar[2:dim(y_ar)[1],])
  X <- cbind(const, y_ar)
  
  ar1.estimates[,factor] <- solve(t(X) %*% X) %*% t(X) %*% y[2:dim(y)[1],]
}

ar1.estimates

level.ar1 <- ar1.estimates['drift', 'level'] + 
             ar1.estimates['ar1_coef', 'level']*NS_betas$level[1:(nrow(NS_betas)-1)]
slope.ar1 <- ar1.estimates['drift', 'slope'] + 
  ar1.estimates['ar1_coef', 'slope']*NS_betas$slope[1:(nrow(NS_betas)-1)]
curvature.ar1 <- ar1.estimates['drift', 'curvature'] + 
  ar1.estimates['ar1_coef', 'curvature']*NS_betas$curvature[1:(nrow(NS_betas)-1)]

DNS.ar1_betas <- cbind(level.ar1, slope.ar1, level.ar1)
DNS.ar1_betas

DNS.ar1_yc <- NS_curve(dates=yields$Date[2:nrow(yields)], 
                  level=level.ar1, slope=slope.ar1, curvature=curvature.ar1,
                  lambda=dns_lambda, maturities=maturity_vector, prefix='M')

# Estimation: DNS without curvature ####
NS_betas.no_curv <- NS.fixed_lambda(rate=yields.xts, maturity_vector, dns_lambda, 
                            include_curvature = FALSE)


level_no_curv.plot <- ggplot(NS_betas.no_curv) + geom_line(aes(x=Index, y=level)) + 
  labs(x='Date', y='Level', 'Level')
slope_no_curv.plot <- ggplot(NS_betas.no_curv) + geom_line(aes(x=Index, y=slope)) + 
  labs(x='Date', y='Slope', 'Slope')

ggpubr::ggarrange(level_no_curv.plot, slope_no_curv.plot, nrow=3,
                  align='hv', label.x='Date')
# ggsave('factors.png')

# no dynamics: use pure estimates
NS_no_curv_yc <- NS_curve(dates=yields$Date, 
                  level=NS_betas.no_curv$level, slope=NS_betas.no_curv$slope, 
                  curvature = NULL,
                  lambda=dns_lambda, maturities=maturity_vector, prefix='M')

round(colMeans(NS_no_curv_yc), 3)
round(sqrt(diag(var(NS_no_curv_yc))),3)

# Forecast factors 1 step ahead -> DNS model with AR(1)
ar1.estimates_nocurv <- matrix(NA, 2,2)
colnames(ar1.estimates_nocurv) <- c('level', 'slope')
row.names(ar1.estimates_nocurv) <- c('drift', 'ar1_coef')
for (factor in c('level', 'slope')){
  
  y <- as.matrix(NS_betas.no_curv[,factor])
  const <- rep(1, dim(y)[1]-1)
  y_ar <- lag(NS_betas.no_curv[,factor],1)
  y_ar <- as.vector(y_ar[2:dim(y_ar)[1],])
  X <- cbind(const, y_ar)
  
  ar1.estimates_nocurv[,factor] <- solve(t(X) %*% X) %*% t(X) %*% y[2:dim(y)[1],]
}

ar1.estimates_nocurv

level.ar1.no_curv <- ar1.estimates_nocurv['drift', 'level'] + 
  ar1.estimates_nocurv['ar1_coef', 'level'] * 
  NS_betas.no_curv$level[1:(nrow(NS_betas.no_curv)-1)]

slope.ar1.no_curv <- ar1.estimates_nocurv['drift', 'slope'] + 
  ar1.estimates_nocurv['ar1_coef', 'slope'] *
  NS_betas.no_curv$slope[1:(nrow(NS_betas.no_curv)-1)]

DNS.ar1.nocurv_betas <- cbind(level.ar1.no_curv, slope.ar1.no_curv)
DNS.ar1.nocurv_betas

DNS.ar1.nocurv_yc <- NS_curve(dates=yields$Date[2:nrow(yields)], 
                       level=level.ar1.no_curv, slope=slope.ar1.no_curv,
                       curvature=NULL,
                       lambda=dns_lambda, maturities=maturity_vector, prefix='M')

# Estimation: DNS with Kalman filter and smoother ####
kf_xi <- read_excel('/Users/maxim_anisimov/Dropbox/QMFI/Code/Factors_MLparameters.xlsx',
                    sheet='xi', col_types = c('numeric', 'numeric', 'numeric'))
kf_xi <- as.matrix(kf_xi)
kf_smoothedxi <- read_excel('/Users/maxim_anisimov/Dropbox/QMFI/Code/Factors_MLparameters.xlsx',
                                        sheet='smoothedxi', col_types = c('numeric', 'numeric', 'numeric'))
kf_smoothedxi <- as.matrix(kf_smoothedxi)
#ML_params <- as.data.frame(read_excel('/Users/maxim_anisimov/Dropbox/QMFI/Code/Factors_MLparameters.xlsx',
#                                      sheet='ML_parameters', col_names=FALSE))
#c <- as.numeric(ML_params[1:3,])
#F_ <- diag(as.numeric(ML_params[4:6,])) 
level_looading <- rep(1,12)
slope_loading <- ( 1-exp(-dns_lambda*maturity_vector) ) /( dns_lambda*maturity_vector ) 
curvature_loading <- slope_loading - exp(-dns_lambda*maturity_vector)
H <- as.matrix(rbind(level_looading, slope_loading, curvature_loading))

kfs_yc <- t(H) %*% t(kf_smoothedxi) %>% t() %>% as.xts(order.by=yields$Date)
colnames(kfs_yc) <- colnames(yields.xts)

round(colMeans(kfs_yc), 3)
round(sqrt(diag(var(kfs_yc))),3)

# COMPARISON of DNS models (stylized facts) ####

# Fact 1
avg_yields.xts <- colMeans(yields.xts)
avg_NS_yc <- colMeans(NS_yc)
avg_NS_yc_nocurv <- colMeans(NS_no_curv_yc)
avg_kfs_yc <- colMeans(kfs_yc)

avg_yc <- data.frame(maturity=maturity_vector,
                     data=as.vector(avg_yields.xts),
                     DNS=as.vector(avg_NS_yc),
                     DNS_KF = as.vector(avg_kfs_yc),
                     DNS_nocurv = as.vector(avg_NS_yc_nocurv))

ggplot(data=avg_yc) + geom_line(aes(x=maturity, y=data, col='empirical'), size=1) +
  geom_line(aes(x=maturity, y=DNS, col='simple DNS'), size=1) + 
  geom_line(aes(x=maturity, y=DNS_KF, col='Kalman DNS'), size=1) + 
  geom_line(aes(x=maturity, y=DNS_nocurv, col='simple DNS w/o curvature'), size=1) +
  guides(color=guide_legend(title="")) + labs(x='Maturity, months', y='Yield, %') + 
  theme(legend.position = 'bottom') +
  scale_color_brewer(palette = "Dark2")
#ggsave('DNS_avg_yield_curves.png', dpi=300,  width=10, height=5)

# Fact 2
get_yc_plot <- function(yield_curve_df, date){
  yc.plot <- ggplot(data=yield_curve_df) + 
    geom_line(aes(x=maturity, y=data, col='empirical'), size=1) + 
    geom_line(aes(x=maturity, y=DNS, col='simple DNS'), size=1) + 
    geom_line(aes(x=maturity, y=DNS_KF, col='Kalman DNS'), size=1) + 
    geom_line(aes(x=maturity, y=DNS_nocurv, col='simple DNS w/o curvature'), size=1) + 
    labs(title = as.yearmon(date), x='Maturity, months', y='Yield, %') + 
    theme(legend.position = 'bottom', plot.title = element_text(hjust=0.5)) + 
    guides(color=guide_legend(title="")) +
    scale_color_brewer(palette = "Dark2")
  return(yc.plot)
}

n_row <- 1
yc_example1 <- data.frame(maturity=maturity_vector, data=as.vector(yields.xts[n_row,]),
                          DNS=as.vector(NS_yc[n_row,]), DNS_KF=as.vector(kfs_yc[n_row,]),
                          DNS_nocurv=as.vector(NS_no_curv_yc[n_row,]))
date <- colnames(t(yields.xts[n_row,]))[1]
yc_example1.plot <- get_yc_plot(yc_example1, date)

n_row <- 100
yc_example2 <- data.frame(maturity=maturity_vector, data=as.vector(yields.xts[n_row,]),
                          DNS=as.vector(NS_yc[n_row,]), DNS_KF=as.vector(kfs_yc[n_row,]),
                          DNS_nocurv=as.vector(NS_no_curv_yc[n_row,]))
date <- colnames(t(yields.xts[n_row,]))[1]
yc_example2.plot <- get_yc_plot(yc_example2, date)

n_row <- 163
yc_example3 <- data.frame(maturity=maturity_vector, data=as.vector(yields.xts[n_row,]),
                          DNS=as.vector(NS_yc[n_row,]), DNS_KF=as.vector(kfs_yc[n_row,]),
                          DNS_nocurv=as.vector(NS_no_curv_yc[n_row,]))
date <- colnames(t(yields.xts[n_row,]))[1]
yc_example3.plot <- get_yc_plot(yc_example3, date)

n_row <- 290
yc_example4 <- data.frame(maturity=maturity_vector, data=as.vector(yields.xts[n_row,]),
                          DNS=as.vector(NS_yc[n_row,]), DNS_KF=as.vector(kfs_yc[n_row,]),
                          DNS_nocurv=as.vector(NS_no_curv_yc[n_row,]))
date <- colnames(t(yields.xts[n_row,]))[1]
yc_example4.plot <- get_yc_plot(yc_example4, date)

ggarrange(yc_example1.plot, yc_example2.plot, yc_example3.plot, yc_example4.plot,
          common.legend = T, legend='bottom', align='hv')
#ggsave('different_shapes.png', dpi=300, width=10, height=7)

# Fact 3
acf.matrix.dns <- c()
for (col in colnames(NS_betas)[1:3]){
  current_acf <- acf(NS_betas[,col], lag.max=12, plot=FALSE)$acf[c(1,6,12)]
  acf.matrix.dns <- rbind(acf.matrix.dns, current_acf)
}
row.names(acf.matrix.dns) <- colnames(NS_betas)[1:3]
colnames(acf.matrix.dns) <- paste0('ro_', c(1,6,12))
acf.matrix.dns <- round(acf.matrix.dns,2)

ACF.dns_plot <- corrplot(acf.matrix.dns, method='color', type="full",
                     addCoef.col="white", tl.col="black", tl.srt=30,
                     number.cex=0.7, tl.cex = .6)

acf.matrix.dns_kalman <- c()
for (col in colnames(kf_smoothedxi)){
  current_acf <- acf(kf_smoothedxi[,col], lag.max=12, plot=FALSE)$acf[c(1,6,12)]
  acf.matrix.dns_kalman <- rbind(acf.matrix.dns_kalman, current_acf)
}
row.names(acf.matrix.dns_kalman) <- colnames(kf_smoothedxi)
colnames(acf.matrix.dns_kalman) <- paste0('ro_', c(1,6,12))
acf.matrix.dns_kalman <- round(acf.matrix.dns_kalman,2)

ACF.dns_kalman_plot <- corrplot(acf.matrix.dns_kalman, method='color', type="full",
                                addCoef.col="white", tl.col="black", tl.srt=30,
                                number.cex=0.7, tl.cex = .6)

acf.matrix.dns_nocurv <- c()
for (col in colnames(NS_betas.no_curv)[1:2]){
  current_acf <- acf(NS_betas.no_curv[,col], lag.max=12, plot=FALSE)$acf[c(1,6,12)]
  acf.matrix.dns_nocurv <- rbind(acf.matrix.dns_nocurv, current_acf)
}
row.names(acf.matrix.dns_nocurv) <- colnames(NS_betas.no_curv)[1:2]
colnames(acf.matrix.dns_nocurv) <- paste0('ro_', c(1,6,12))
acf.matrix.dns_nocurv <- round(acf.matrix.dns_nocurv,2)

ACF.dns_nocurv_plot <- corrplot(acf.matrix.dns_nocurv, method='color', type="full",
                         addCoef.col="white", tl.col="black", tl.srt=30,
                         number.cex=0.7, tl.cex = .6)

# Fact 4
acf.matrix.dns_yields <- c()
for (col in colnames(NS_yc)){
  current_acf <- acf(NS_yc[,col], lag.max=30, plot=FALSE)$acf[c(2,13,31)]
  acf.matrix.dns_yields <- rbind(acf.matrix.dns_yields, current_acf)
}
row.names(acf.matrix.dns_yields) <- colnames(NS_yc)
colnames(acf.matrix.dns_yields) <- paste0('ro_', c(1,12,30))
acf.matrix.dns_yields <- round(acf.matrix.dns_yields,2)

#ACF.dns_yields_plot <- corrplot(acf.matrix.dns_yields, method='color', type="full",
#                         addCoef.col="white", tl.col="black", tl.srt=30,
#                         number.cex=0.7, tl.cex = .6)

acf.matrix.dns_kalman_yields <- c()
for (col in colnames(kfs_yc)){
  current_acf <- acf(kfs_yc[,col], lag.max=30, plot=FALSE)$acf[c(2,13,31)]
  acf.matrix.dns_kalman_yields <- rbind(acf.matrix.dns_kalman_yields, current_acf)
}
row.names(acf.matrix.dns_kalman_yields) <- colnames(kfs_yc)
colnames(acf.matrix.dns_kalman_yields) <- paste0('ro_', c(1,12,30))
acf.matrix.dns_kalman_yields <- round(acf.matrix.dns_kalman_yields,2)

ACF.dns_kalman_yields_plot <- corrplot(acf.matrix.dns_kalman_yields, method='color', 
                                       type="full",
                                addCoef.col="white", tl.col="black", tl.srt=30,
                                number.cex=0.7, tl.cex = .6)

acf.matrix.dns_nocurv_yields <- c()
for (col in colnames(NS_no_curv_yc)){
  current_acf <- acf(NS_no_curv_yc[,col], lag.max=30, plot=FALSE)$acf[c(2,13,31)]
  acf.matrix.dns_nocurv_yields <- rbind(acf.matrix.dns_nocurv_yields, current_acf)
}
row.names(acf.matrix.dns_nocurv_yields) <- colnames(NS_no_curv_yc)
colnames(acf.matrix.dns_nocurv_yields) <- paste0('ro_', c(1,12,30))
acf.matrix.dns_nocurv_yields <- round(acf.matrix.dns_nocurv_yields,2)

ACF.dns_nocurv_yields_plot <- corrplot(acf.matrix.dns_nocurv_yields, method='color', type="full",
                                addCoef.col="white", tl.col="black", tl.srt=30,
                                number.cex=0.7, tl.cex = .6)

all_dns.acf <- as.data.frame(cbind(acf.matrix.dns_yields, acf.matrix.dns_kalman_yields,
                     acf.matrix.dns_nocurv_yields))
WriteXLS(all_dns.acf, ExcelFileName = 'all_DNS_ACF.xls', row.names = TRUE)

# Fact 5
st_devs.ns_betas <- round(sqrt(diag(var(NS_betas[,1:3]))),2)
var(NS_betas[,1:3])
st_devs.ns_betas.kf <- round(sqrt(diag(var(kf_smoothedxi))),2)
st_devs.ns_betas.no_curv <- round(sqrt(diag(var(NS_betas.no_curv[,1:2]))),2)

st_devs.ns_yields <- round(sqrt(diag(var(NS_yc))),2)
st_devs.ns_yields.kf <- round(sqrt(diag(var(kfs_yc))),2)
st_devs.ns_yields.no_curv <- round(sqrt(diag(var(NS_no_curv_yc))),2)

std.dns_models <- data.frame(maturity=maturity_vector, DNS=st_devs.ns_yields,
                             DNS_Kalman=st_devs.ns_yields.kf, 
                             DNS_nocurv=st_devs.ns_yields.no_curv)

std.dns_plot <- ggplot(data=std.dns_models) + 
  geom_line(aes(x=maturity, y=DNS, color='DNS'), size=1) + 
  geom_line(aes(x=maturity, y=DNS_Kalman, color='Kalman DNS'), size=1) + 
  geom_line(aes(x=maturity, y=DNS_nocurv, color='DNS without curvature'), size=1) + 
  labs(x='Maturity, months', y='Standard Deviaton, pp')
std.dns_plot

# Fact 6
cor_matrix.ns_betas <- cor(NS_betas[,1:3])
cor_matrix.ns_betas_kalman <- cor(kf_smoothedxi)
cor_matrix.ns_betas_nocurv <- cor(NS_betas.no_curv[,1:2])

cor_matrix.ns_yields <- cor(NS_yc)
cor_matrix.ns_yields_kalman <- cor(kfs_yc)
cor_matrix.ns_yields_nocurv <- cor(NS_no_curv_yc)

corr.ns_yields_plot <- corrplot(cor_matrix.ns_yields, method='color', type="full",
                      addCoef.col="white", tl.col="black", tl.srt=30,
                      number.cex=0.45, tl.cex = .45, cl.cex = 0.45)
corr.ns_yields_plot
corr.ns_yields_kalman_plot <- corrplot(cor_matrix.ns_yields_kalman, method='color',
                                       type="full",
                                addCoef.col="white", tl.col="black", tl.srt=30,
                                number.cex=0.45, tl.cex = .45, cl.cex = 0.45)
corr.ns_yields_kalman_plot
corr.ns_yields_nocurv_plot <- corrplot(cor_matrix.ns_yields_nocurv, method='color',
                                       type="full",
                                       addCoef.col="white", tl.col="black", tl.srt=30,
                                       number.cex=0.45, tl.cex = .45, cl.cex = 0.45)
corr.ns_yields_nocurv_plot


# Factors plots ####
# Level
levels <- data.frame(`empirical`=as.vector(yields.xts$M120), 
                     `Simple DNS`=as.vector(NS_betas$level),
                     `Kalman DNS`=kf_smoothedxi[,1],
                     `Simple DNS w/o curvature`=as.vector(NS_betas.no_curv$level))
levels.xts <- xts(levels, order.by = yields$Date)

levels.plot <- ggplot(data=levels.xts) + 
  geom_line(aes(x=Index, y=empirical, color='empirical')) +
  geom_line(aes(x=Index, y=Simple.DNS, color='simple DNS')) +
  geom_line(aes(x=Index, y=Kalman.DNS, color='Kalman DNS')) + 
  geom_line(aes(x=Index, y=Simple.DNS.w.o.curvature, color='simple DNS w/o curvature')) + 
  labs(title='Level', linetype='', color='',
       x='Date', y='') + 
  coord_cartesian(expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom') +
  scale_color_brewer(palette = "Dark2")

# Slope
slopes <- data.frame(`empirical` = as.vector(yields.xts$M120-yields.xts$M3), 
                     `Simple DNS`= -as.vector(NS_betas$slope),
                     `Kalman DNS`= -kf_smoothedxi[,2],
                     `Simple DNS w/o curvature` = -as.vector(NS_betas.no_curv$slope))
slopes.xts <- xts(slopes, order.by = yields$Date)

slopes.plot <- ggplot(data=slopes.xts) + 
  geom_line(aes(x=Index, y=empirical, color='empirical')) +
  geom_line(aes(x=Index, y=Simple.DNS, color='simple DNS')) +
  geom_line(aes(x=Index, y=Kalman.DNS, color='Kalman DNS')) + 
  geom_line(aes(x=Index, y=Simple.DNS.w.o.curvature, color='simple DNS w/o curvature')) + 
  labs(title='Slope', linetype='', color='', x='Date', y='') + 
  coord_cartesian(expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom') +
  scale_color_brewer(palette = "Dark2")

# Curvature
# DL2006: the curvature as the twice the 2-year yield minus 
# the sum of the 3-month and 10-year yields
curvatures <- data.frame(`empirical` = as.vector(2*yields.xts$M24 - 
                                               (yields.xts$M120+yields.xts$M3)), 
                     `Simple DNS`= 0.3*as.vector(NS_betas$curvature),
                     `Kalman DNS`= 0.3*kf_smoothedxi[,3])
curvatures.xts <- xts(curvatures, order.by = yields$Date)

curvatures.plot <- ggplot(data=curvatures.xts) + 
  geom_line(aes(x=Index, y=empirical, color='empirical')) +
  geom_line(aes(x=Index, y=Simple.DNS, color='simple DNS')) +
  geom_line(aes(x=Index, y=Kalman.DNS, color='Kalman DNS')) + 
  labs(title='Curvature', linetype='', color='', x='Date', y='') + 
  coord_cartesian(expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom') +
  scale_color_brewer(palette = "Dark2")

# All factors
ggpubr::ggarrange(levels.plot, slopes.plot, curvatures.plot, nrow=3,
                  align='hv', label.x='Date', common.legend = T, legend='bottom')
#ggsave('DNS_factors.png', dpi=300, width=10, height=12)

# Yields plots ####
# M3
M3 <- data.frame(M3=as.vector(yields.xts$M3), 
                     `Simple DNS`=as.vector(NS_yc$M3),
                     `Kalman DNS`=as.vector(kfs_yc$M3),
                     `Simple DNS w/o curvature`=as.vector(NS_no_curv_yc$M3))
M3.xts <- xts(M3, order.by = yields$Date)

M3.plot <- ggplot(data=M3.xts) + 
  geom_line(aes(x=Index, y=M3, color='empirical')) +
  geom_line(aes(x=Index, y=Simple.DNS, color='simple DNS')) +
  geom_line(aes(x=Index, y=Kalman.DNS, color='Kalman DNS')) + 
  geom_line(aes(x=Index, y=Simple.DNS.w.o.curvature, color='simple DNS w/o curvature')) + 
  labs(title='3-month yield', linetype='', color='', x='Date', y='') + 
  coord_cartesian(expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom') +
  scale_color_brewer(palette = "Dark2")

# M12
M12 <- data.frame(M12=as.vector(yields.xts$M12), 
                 `Simple DNS`=as.vector(NS_yc$M12),
                 `Kalman DNS`=as.vector(kfs_yc$M12),
                 `Simple DNS w/o curvature`=as.vector(NS_no_curv_yc$M12))
M12.xts <- xts(M12, order.by = yields$Date)

M12.plot <- ggplot(data=M12.xts) + 
  geom_line(aes(x=Index, y=M12, color='empirical')) +
  geom_line(aes(x=Index, y=Simple.DNS, color='simple DNS')) +
  geom_line(aes(x=Index, y=Kalman.DNS, color='Kalman DNS')) + 
  geom_line(aes(x=Index, y=Simple.DNS.w.o.curvature, color='simple DNS w/o curvature')) + 
  labs(title='1-year yield', linetype='', color='', x='Date', y='') + 
  coord_cartesian(expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom') +
  scale_color_brewer(palette = "Dark2")

# M24 
M24 <- data.frame(M24=as.vector(yields.xts$M24), 
                  `Simple DNS`=as.vector(NS_yc$M24),
                  `Kalman DNS`=as.vector(kfs_yc$M24),
                  `Simple DNS w/o curvature`=as.vector(NS_no_curv_yc$M24))
M24.xts <- xts(M24, order.by = yields$Date)

M24.plot <- ggplot(data=M24.xts) + 
  geom_line(aes(x=Index, y=M24, color='empirical')) +
  geom_line(aes(x=Index, y=Simple.DNS, color='simple DNS')) +
  geom_line(aes(x=Index, y=Kalman.DNS, color='Kalman DNS')) + 
  geom_line(aes(x=Index, y=Simple.DNS.w.o.curvature, color='simple DNS w/o curvature')) + 
  labs(title='2-year yield', linetype='', color='', x='Date', y='') + 
  coord_cartesian(expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom') +
  scale_color_brewer(palette = "Dark2")

# M120
M120 <- data.frame(M120=as.vector(yields.xts$M120), 
                  `Simple DNS`=as.vector(NS_yc$M120),
                  `Kalman DNS`=as.vector(kfs_yc$M120),
                  `Simple DNS w/o curvature`=as.vector(NS_no_curv_yc$M120))
M120.xts <- xts(M120, order.by = yields$Date)

M120.plot <- ggplot(data=M120.xts) + 
  geom_line(aes(x=Index, y=M120, color='empirical')) +
  geom_line(aes(x=Index, y=Simple.DNS, color='simple DNS')) +
  geom_line(aes(x=Index, y=Kalman.DNS, color='Kalman DNS')) + 
  geom_line(aes(x=Index, y=Simple.DNS.w.o.curvature, color='simple DNS w/o curvature')) + 
  labs(title='10-year yield', linetype='', color='', x='Date', y='') + 
  coord_cartesian(expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom') +
  scale_color_brewer(palette = "Dark2")

ggpubr::ggarrange(M3.plot, M24.plot, M120.plot, nrow=3,
                  align='hv', label.x='Date', common.legend = T, legend='bottom')
#ggsave('DNS_yields.png', dpi=300, width=10, height=12)

# Estimation accuracy ####

prediction_list <- list(NS_yc, kfs_yc, NS_no_curv_yc)
rmse_list <- list()

for (n in 1:3){
  cur_df <- prediction_list[[n]]
  
  cur_rmse <- c()
  for (maturity in colnames(yields.xts)){
    cur_rmse <- c(cur_rmse, 
                  rmse(actual=yields.xts[,maturity], predicted=cur_df[,maturity]))
  }
  
  rmse_list[[n]] <- cur_rmse
}

rmse_list

rmse.df <- round( data.frame(SDNS=rmse_list[[1]], KDNS=rmse_list[[2]],
                      SDNS_nocurv=rmse_list[[3]]), 3)
row.names(rmse.df) <- colnames(yields.xts)
WriteXLS(rmse.df, 'RMSE.xls', row.names = TRUE)


# Simulation hist plots ####
library(ggplot2)
library(gridExtra)
theme_set(theme_light())

dat_2step = read.csv('2step.csv', header=FALSE)
dat_ss = read.csv('state_space.csv', header=FALSE)

plot_hist <- function(data, column_name, DGP_param, factor_coef_name){
  
  plot <- ggplot(data, aes_string(x=column_name)) + 
    geom_histogram(color='blue', fill='blue') +
    geom_vline(aes(xintercept = mean(data[,column_name]), color = 'simulation mean'),
               linetype = "dashed", size=1) +
    geom_vline(aes(xintercept = DGP_param, color = 'DGP parameter'), 
               linetype = "dashed", size=1) +
    coord_cartesian(expand = FALSE) +
    labs(title = factor_coef_name, y="", x="") +
    guides(color=guide_legend(title="")) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.title.x = element_text(hjust = 0.5, size = 10),
      axis.title.y = element_text(hjust = 0.5, size = 10),
      legend.position = 'bottom') +
    scale_color_brewer(palette = "Dark2")
  
  return(plot)
}

# Normal simulation
p1 <- plot_hist(dat_2step, 'V1', -0.005673833, 'Level drift')
p2 <- plot_hist(dat_2step, 'V2', -0.045405732687023, 'Slope drift')
p3 <- plot_hist(dat_2step, 'V3', -0.238580841324576, 'Curvature drift')
p4 <- plot_hist(dat_2step, 'V4', 0.994, 'Level AR coef.')
p5 <- plot_hist(dat_2step, 'V5', 0.972, 'Slope AR coef.')
p6 <- plot_hist(dat_2step, 'V6', 0.919, 'Curvature AR coef.')

grid_2step_1 <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2,
                  common.legend = TRUE, legend='bottom')
#ggsave(file="histograms_2step_1.png", dpi=300, width=10, height=6) #save plot

p1 <- plot_hist(dat_ss, 'V1', -0.005673833, 'Level drift')
p2 <- plot_hist(dat_ss, 'V2', -0.045405732687023, 'Slope drift')
p3 <- plot_hist(dat_ss, 'V3', -0.238580841324576, 'Curvature drift')
p4 <- plot_hist(dat_ss, 'V4', 0.994, 'Level AR coef.')
p5 <- plot_hist(dat_ss, 'V5', 0.972, 'Slope AR coef.')
p6 <- plot_hist(dat_ss, 'V6', 0.919, 'Curvature AR coef.')

grid_ss_1 <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2,
                          common.legend = TRUE, legend='bottom')
#ggsave(file="histograms_ss_1.png", dpi=300, width=10, height=6)


# Robustness check with increased variance
dat_2step_highnoise = read_excel('2step_highnoise.xlsx',col_names =FALSE,
                                 col_types = 'numeric') %>% as.data.frame()
colnames(dat_2step_highnoise) <- colnames(dat_2step)
dat_ss_highnoise = read_excel('state_space_highnoise.xlsx', col_names=FALSE,
                              col_types = 'numeric') %>% as.data.frame()
colnames(dat_ss_highnoise) <- colnames(dat_2step)

p1 <- plot_hist(dat_2step_highnoise, "V1", -0.005673833, 'Level drift')
p2 <- plot_hist(dat_2step_highnoise, 'V2', -0.045405732687023, 'Slope drift')
p3 <- plot_hist(dat_2step_highnoise, 'V3', -0.238580841324576, 'Curvature drift')
p4 <- plot_hist(dat_2step_highnoise, 'V4', 0.994, 'Level AR coef.')
p5 <- plot_hist(dat_2step_highnoise, 'V5', 0.972, 'Slope AR coef.')
p6 <- plot_hist(dat_2step_highnoise, 'V6', 0.919, 'Curvature AR coef.')

grid_2step_highnoise <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2,
                          common.legend = TRUE, legend='bottom')
#ggsave(file="histograms_2step_largenoise.png", dpi=300, width=10, height=6)

p1 <- plot_hist(dat_ss_highnoise, "V1", -0.005673833, 'Level drift')
p2 <- plot_hist(dat_ss_highnoise, 'V2', -0.045405732687023, 'Slope drift')
p3 <- plot_hist(dat_ss_highnoise, 'V3', -0.238580841324576, 'Curvature drift')
p4 <- plot_hist(dat_ss_highnoise, 'V4', 0.994, 'Level AR coef.')
p5 <- plot_hist(dat_ss_highnoise, 'V5', 0.972, 'Slope AR coef.')
p6 <- plot_hist(dat_ss_highnoise, 'V6', 0.919, 'Curvature AR coef.')

grid_ss_highnoise <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2,
                                  common.legend = TRUE, legend='bottom')
#ggsave(file="histograms_ss_largenoise.png", dpi=300, width=10, height=6)

# Robustness check with decreased variance

dat_2step_smallnoise = read_excel('2step_smallnoise.xlsx',col_names =FALSE,
                                 col_types = 'numeric') %>% as.data.frame()
colnames(dat_2step_smallnoise) <- colnames(dat_2step)
dat_ss_smallnoise = read_excel('state_space_smallnoise.xlsx', col_names=FALSE,
                              col_types = 'numeric') %>% as.data.frame()
colnames(dat_ss_smallnoise) <- colnames(dat_2step)

p1 <- plot_hist(dat_2step_smallnoise, "V1", -0.005673833, 'Level drift')
p2 <- plot_hist(dat_2step_smallnoise, 'V2', -0.045405732687023, 'Slope drift')
p3 <- plot_hist(dat_2step_smallnoise, 'V3', -0.238580841324576, 'Curvature drift')
p4 <- plot_hist(dat_2step_smallnoise, 'V4', 0.994, 'Level AR coef.')
p5 <- plot_hist(dat_2step_smallnoise, 'V5', 0.972, 'Slope AR coef.')
p6 <- plot_hist(dat_2step_smallnoise, 'V6', 0.919, 'Curvature AR coef.')

grid_2step_smallnoise <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2,
                                  common.legend = TRUE, legend='bottom')
#ggsave(file="histograms_2step_smallnoise.png", dpi=300, width=10, height=6)

p1 <- plot_hist(dat_ss_smallnoise, "V1", -0.005673833, 'Level drift')
p2 <- plot_hist(dat_ss_smallnoise, 'V2', -0.045405732687023, 'Slope drift')
p3 <- plot_hist(dat_ss_smallnoise, 'V3', -0.238580841324576, 'Curvature drift')
p4 <- plot_hist(dat_ss_smallnoise, 'V4', 0.994, 'Level AR coef.')
p5 <- plot_hist(dat_ss_smallnoise, 'V5', 0.972, 'Slope AR coef.')
p6 <- plot_hist(dat_ss_smallnoise, 'V6', 0.919, 'Curvature AR coef.')

grid_ss_smallnoise <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2,
                               common.legend = TRUE, legend='bottom')
#ggsave(file="histograms_ss_smallnoise.png", dpi=300, width=10, height=6)