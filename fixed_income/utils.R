plot_3D_YC <- function(yields.xts, maturities,
                       xlabel='Date', ylabel='Maturity', zlabel='Yield, %'){
  "Create our 3d surface yield curve"
  
  yields.xts %>%
    # convert to numeric matrix
    data.matrix() %>% 
    # transpose
    t() %>%
    # draw our Plotly 3d surface
    plot_ly(
      x=as.Date(index(yields.xts)),
      y=maturities,
      z=.,
      type="surface"
    ) %>%
    plotly::layout(
      scene=list(
        xaxis=list(title=xlabel),
        yaxis=list(title=ylabel),
        zaxis=list(title=zlabel)
      )
    )
}

plot_stat_yc <- function(yields, maturities, country_name){
  "Plot mean, median and percentiles of the yield curve"
  mean_yc <- apply(yields, 2, FUN = mean)
  median_yc <- apply(yields, 2, FUN = median) # 2 for columns
  q25_yc <- colQuantiles(yields)[,2]
  q75_yc <- colQuantiles(yields)[,4]
  stat_df <- data.frame("mean"=mean_yc, "median"=median_yc, 'Q25'=q25_yc, 'Q75'=q75_yc,
                        'maturity'=maturities)
  stat_yc <- ggplot(data=stat_df) +
    geom_line(aes(x=maturity, y=mean, linetype='mean'), size=1.5) +
    geom_line(aes(x=maturity, y=median, linetype='median'), size=1.5) + 
    geom_point(aes(x=maturity, y=Q25, color='25th percentile'), size=2) +
    geom_point(aes(x=maturity, y=Q75, color='75th percentile'), size=2) + 
    labs(title=country_name, linetype='', color='',
         x='Maturity, months', y='Yield, %',
         subtitle = paste('Sample:', 
                          paste(as.yearmon(index(yields)[1]),
                                as.yearmon(index(yields[nrow(yields)])), sep=" - "),
                          sep=" ")) + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          legend.position = 'bottom') +
    scale_color_brewer(palette = "Dark2")
  
  return(stat_yc)
}

NS.estimate <- function(rate, maturity, lambda, include_curvature=TRUE){
  
  slope_loading <- ( 1-exp(-lambda*maturity) ) / 
    ( lambda*maturity ) 
  
  if (include_curvature == TRUE){
    curvature_loading <- slope_loading - exp(-lambda*maturity)
    model <- lm(t(rate) ~ 1 + slope_loading + curvature_loading)
    beta_hat <- coef(model)
    names(beta_hat) <- c("level", "slope", "curvature")
    
  } else {
    model <- lm(t(rate) ~ 1 + slope_loading)
    beta_hat <- coef(model)
    names(beta_hat) <- c("level", "slope")
  }
  
  return(beta_hat)
}

NS.fixed_lambda <- function(rate, maturity, lambda, include_curvature=TRUE){
  
  if (include_curvature == TRUE){
    FinalResults <- matrix(NA, nrow(rate), 4)
    colnames( FinalResults ) <- c("level","slope","curvature","lambda")
  } else {
    FinalResults <- matrix(NA, nrow(rate), 3)
    colnames( FinalResults ) <- c("level","slope","lambda")
  }
  
  for (i in 1:nrow(rate)){
    NS_estim <- NS.estimate(rate[i,], maturity, lambda, 
                            include_curvature=include_curvature)
    BetaCoef <- NS_estim # beta_coefs
    FinalResults[i,] <- c(BetaCoef, lambda)
  } 
  
  FinalResults <- xts(FinalResults, order.by=index(rate))
  return(FinalResults)
}

NS_curve <- function(dates, level, slope, curvature, lambda, maturities,
                     prefix='M'){
  
  level_init <- as.matrix(level)
  level <- level_init
  for (i in 1:(length(maturities)-1)){
    level <- cbind(level, level_init)
  }
  
  slope <- as.matrix(slope)
  slope_loading <- (1-exp(-lambda*maturities))/(lambda*maturities)
  
  if (is.null(curvature) == FALSE){
    curvature <- as.matrix(curvature)
    curvature_loading <- (1-exp(-lambda*maturities))/(lambda*maturities) -
      exp(-lambda*maturities)
  }
  
  if (is.null(curvature) == FALSE){
    yc <- level + slope%*%t(slope_loading) + curvature%*%t(curvature_loading)
  } else {
    yc <- level + slope%*%t(slope_loading)
  }
  
  yc_df <- data.frame(yc)
  rownames(yc_df) <- as.Date(dates)
  colnames(yc_df) <- paste0(prefix, as.character(maturities))
  return(as.xts(yc_df))
}