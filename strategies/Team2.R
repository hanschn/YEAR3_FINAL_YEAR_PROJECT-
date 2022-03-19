# -------- Parameters ----------------------

#  params <- list(
#     "MACD_BBand" = list(nFast = 12, nSlow = 26, nSig = 9,
#     nMA = 30, Series = c(4,9,10),
#     lost_ratio = 0.15, 
#     ratio = 0.13),
#     "RSI_High" = list(rsi_period = 15,
#     rsi_threshold = 60,
#     Series = c(2,5),  
#     ema_long = 50, 
#     ema_short = 20,
#     ratio = 0.18),
#     "RSI_EMA_trend" = list(trend_period = 30,
#     trend_duration = 4,
#     long_EMAPeriod = 10,
#     short_EMAPeriod = 5,
#     long_EMAExit = 40,
#     short_EMAExit = 15,
#     RSI_threshold = 50,
#     RSI_period = 14,
#     Port_lookback = 200,
#     Series = c(4,2),
#     ratio = 0.18) )

# ----- Parameters END ----------------------


# Position Sizing:
# Mixed strategy "MACD_BBand" : 13%, "RSI_High(HighorderPortfolio)" : 18%, "EMA_trend": 18%  <- optimised 

# ---- HighOrderPortfolio package from Github ----
# The github package <- HighOrderPortfolio 
# devtools::install_github("dppalomar/highOrderPortfolios")
library(highOrderPortfolios)



#  ----- VARIABLE FOR ALL STRATEGY ----------

Order_info <- list(signals = rep(0, 10), 
                   volumes = rep(0, 10), 
                   enterPrice = rep(0, 10)
)

all_order <- list(MACD_BBand = Order_info,
                RSI_High = Order_info,
                RSI_Trend = Order_info)

# ------ Order from MACD_BBand -----------  
# parameters of MACD: nFast = 12, nSlow = 26, nSig = 9
# parameters of bband: sdLow = 2, sdHigh = 4.2, nMA = 30 <- optimised

# optimised parameters <- 4.2 sdvalue for the upper bound 2 sdValue for the lower bound

MACD_BBand_new_order <- function(price, macdValue, maValue, sdValue){
  
  retValue <- 0
  
  price1 <- tail(price, 1)
  if(macdValue > 0){ 
    if(price1 > (maValue + 4.2 * sdValue)){
      retValue <- (-1)
    }else if(price1 < (maValue - 2 * sdValue)){ 
      retValue  <- (1)
    }
  }else if(macdValue < 0){ 
    if(price1 > (maValue + 4.2 * sdValue)){
      retValue <- (-1)
    }else if(price1 < (maValue - 2 * sdValue)){
      retValue <- (1)
    }
  }
  return(retValue)
}

MACD_BBand_exit_order <- function(cur_signal, price, maValue){
  
  retValue <- 0
    # exit long trade
  if(cur_signal > 0 & price[1] > maValue & price[2] < maValue){ 
    retValue <- 1
    # exit short trade
  }else if(cur_signal < 0 & price[1] < maValue & price[2] > maValue){ 
    retValue <- 1
  }
  
  return(retValue)
}

# parameters of "MACD_BBand", here we set parameters as lost_ratio(0.15) <- optimised 
MACD_BBand_stop_loss <- function(store, params){
  
  retValue <- rep(FALSE, ncol(store))
  
  for(i in 1:length(retValue)){
    
    if(all_order$MACD_BBand$enterPrice[i] != 0){ 
      cur_rate <- (store[nrow(store), i]/all_order$MACD_BBand$enterPrice[i]) ^ (all_order$MACD_BBand$signals[i])
      if(cur_rate < (1 - params$lost_ratio)){
        retValue[i] <- TRUE
      }
    }
  }
  return(retValue)
}

# MACD_BBand order function 
MACD_BBand_order <- function(store, params, info){
  
  allzero  <- rep(0,ncol(store))
  marketOrders <- allzero

  last_signals <- all_order$MACD_BBand$signals 
  loss_signals <- allzero 
  new_signals <- allzero 
  exit_signals <- allzero 
  
  
  numOfDays   <- nrow(store) 
  
  if(numOfDays >= (max(params$nSlow, params$nFast) + 10) ){
    
    loss_signals <- MACD_BBand_stop_loss(store, params)
    
    loss_signals[base::setdiff(c(1:length(loss_signals)), params$Series)] <- 0
    
    for(i in 1:length(allzero)){
        
      order_new <- 0
      order_exit <- 0
      
      if(i %in% params$Series){
        
        # stop loss signal
        if(loss_signals[i]) loss_signals[i] <- (-1) * last_signals[i]
        
        
        # close price <- price 
        price <- tail(store[,i], 2) 
        # macd, nFast = 12, nSlow = 26, nSig = 9
        macd <- tail(MACD(store[,i], params$nFast, params$nSlow, params$nSig)[, "macd"], 1)
        # bband, sdLow = 2, sdHigh = 4.2, nMA = 30
        bb <- BBands(store[, i], n = params$nMA)
        bb_ma <- tail(bb[,"mavg"], 1)
        bb_sd <- (tail(bb[,"up"], 1) - bb_ma) / 2
        
        # new order signal
        order_new <- MACD_BBand_new_order(price, macd, bb_ma, bb_sd)
        if(order_new != 0) new_signals[i] <- order_new - last_signals[i]
        
        # exit order signal
        order_exit <- MACD_BBand_exit_order(last_signals[i], price, bb_ma)
        if(order_exit) exit_signals[i] <- (-1) * last_signals[i]
      }
    }
    
    #  sum :
    market_signals <- loss_signals + new_signals + exit_signals
    
    #  convert signals to market_orders
    for(i in 1:length(market_signals)){
      
      if(abs(market_signals[i]) >=2){
        # abs(market_signal) can not exceed 2
        # if exceed 2, sell last_volume, and add position 
        marketOrders[i] <- (-1) * all_order$MACD_BBand$signals[i] * all_order$MACD_BBand$volumes[i] + 
          sign(market_signals[i]) * floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        
        # update all_order info
        all_order$MACD_BBand$signals[i] <- sign(market_signals[i])
        all_order$MACD_BBand$volumes[i] <- floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        all_order$MACD_BBand$enterprice[i] <- store[nrow(store), i]
      }else if(abs(market_signals[i]) == 1){
        
        marketOrders[i] <- sign(market_signals[i]) * floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        
        # update all_order info
        all_order$MACD_BBand$signals[i] <- sign(market_signals[i])
        all_order$MACD_BBand$volumes[i] <- floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        all_order$MACD_BBand$enterprice[i] <- store[nrow(store), i]
      }
    }
  }
  return(marketOrders)
}

# -----------   end of MACD_BBand  ---------------------------

#   HighOrderPortfolio : 

# -------- References ----------:

# https://github.com/dppalomar/highOrderPortfolios <- the highOrderPortfolio github website 
#[1] H. Markowitz, "Portfolio selection," J. Financ., vol. 7, no. 1, pp. 77-91, 1952.
#[2] R. Zhou and D. P. Palomar, "Solving high-order portfolios via successive convex approximation algorithms," https://arxiv.org/abs/2008.00863. 2020.
#[3] K. Boudt, D. Cornilly, F. V. Holle, and J. Willems, "Algorithmic portfolio tilting to harvest higher moment gains," Heliyon, vol. 6, no. 3, pp. 1-8, 2020.

# -------------------------------- #

#----- Order from RSI_EMA_highOrderPortolio ------------------


# function to exit current trade
RSI_High_exit_order <- function(cur_signal, ema10, ema25){
  
  if(cur_signal > 0 & ema10 < ema25){ # exit long trade
    return(-cur_signal)
  }else if(cur_signal < 0 & ema10 > ema25){ # exit short trade
    return(-cur_signal)
  }else{ 
    return(0)
  }
}

RSI_High_new_order <- function(rsix, threshold, ema10, ema25){
  ret_v <- 0 # return value

  if(rsix[1] >= threshold & rsix[1] > rsix[2] & ema10 < ema25){ 
    ret_v <- -1
  }else if(rsix[1] <= (100-threshold) & rsix[1] < rsix[2] & ema10 > ema25){ 
    ret_v <- 1
  }else{
    ret_v <- 0
  }
  return(ret_v) # 
}

# return: weights of each series
HighOrderPort <- function(store, order_new, params){
  
  ret_weight <- order_new 
  
  # referance from : fast Design of HighOrderPortfolios - RuiZhou and Daniel P.Palomar
  sum_order <- sum(abs(order_new))
  if(sum_order >= 2){
    idx <- max(c(nrow(store) - 300, 1) ):nrow(store) 
    mat <- store[idx, abs(order_new) == 1]

    X_moments <- estimate_moments(mat, adjust_magnitude = TRUE)
    
    hop <- rep(1 / sum_order, sum_order)
    hop_moments <- eval_portfolio_moments(hop, X_moments)
    d <- abs(hop_moments) 
    kappa <- 0.3 * sqrt(hop %*% X_moments$Sgm %*% hop)
    
    # portfolio optimization
    sol <- design_MVSKtilting_portfolio(d, X_moments, w_init = hop, hop = hop, 
                                        hop_moments = hop_moments, kappa = kappa, 
                                        ftol = 1e-10)
    ret_weight[abs(order_new) == 1] <- sol$w
  }
  
  return(ret_weight)
}

RSI_High_order <- function(store, params, info){
  
  
  allzero  <- rep(0, ncol(store)) 
  marketOrders <- allzero
  # get current RSI_Trend order(position)
  cur_signal <- all_order$RSI_High$signals 
  
  order_exit <- allzero 
  order_new <- allzero 
  
  # get days of history data
  numOfDays   <- nrow(store) 
  
    # check weather the data is enough for our strategy
  if(numOfDays >= (max(params$rsi_period, params$ema_long) + 10) ){ 

    for(i in 1:ncol(store)){

      
      if(i %in% params$Series){
        # RSI
        rsi_x <- tail(RSI(store[,i], n = params$rsi_period), 2)

        # EMA
        ema10 <- tail(EMA(store[,i], n = params$ema_short), 1)
        ema25 <- tail(EMA(store[,i], n = params$ema_long), 1)
        
        # 1: exit current position
        order_exit[i] <- RSI_High_exit_order(cur_signal[i], ema10, ema25)
        # 2: enter new position
        order_new[i] <- RSI_High_new_order(rsi_x, params$rsi_threshold, ema10, ema25)
      }
      
    }
    
    # order of new, if new order arrived, exit order must triggered,
    # so this must be executed before 'order of exit'
    mktorder_new <- allzero
    for(i in 1:length(order_new)){
      
      if((order_new[i] == -1 * (all_order$RSI_High$signals[i])) & order_new[i] != 0){
        order_exit[i] <- (-all_order$RSI_High$signals)
        mktorder_new[i] <- order_new[i]
      }
      
      # new order 
      if(order_new[i] != 0){ 
        mktorder_new[i] <- order_new[i]
      }else{ 
        mktorder_new[i] <- all_order$RSI_High$signals[i]
      }
      
    }
    
    if(sum(mktorder_new == all_order$RSI_High$signals) != length(mktorder_new) | (sum(abs(order_exit)) != 0)){
      
      mktvolume_new <- allzero
      
      sumx <- sum(abs(mktorder_new) )
      if(sumx > 0){
        mktorder_new <- HighOrderPort(store, mktorder_new) 
        
        mktvolume_new <- (mktorder_new / sumx ) * info$balance * params$ratio 
        mktvolume_new <- floor(mktvolume_new / store[nrow(store), ])
      }
      
      # order of exit
      mktvolume_exit <- order_exit * all_order$RSI_High$volumes
      
      # market orders
      marketOrders <- mktvolume_new + mktvolume_exit
      
      # update order information 
      all_order$RSI_High$signals <- mktorder_new
      all_order$RSI_High$volumes <- mktvolume_new
      all_order$RSI_High$enterPrice <- info$balance * params$ratio
    } 
  }
  return(marketOrders)
}

#------ end of RSI_EMA_highOrderPortolio -------------

# RSI_EMA_TREND
# -------------- Referance ---------------------

# Shankar, R.S. (2020). The Amazing Combination of ‘EMA & RSI’ While Trading The Forex Market.  
# - Available at: https://www.forex.academy/the-amazing-combination-of-ema-rsi-while-trading-the-forex-market/

# -----------------------------------------------

#----- Order from RSI_EMA_trend ------------------------------


RSI_EMA_trend_exit_order <- function(cur_signal, price, params){
  
  emaShort <- tail(SMA(price, n = params$short_EMAExit), 1)
  emaLong <- tail(SMA(price, n = params$long_EMAExit), 1)
  if(cur_signal > 0 & emaShort < emaLong){ 
    return(1)
  }else if(cur_signal < 0 & emaShort > emaLong){ 
    return(1)
  }else{ 
    return(0)
  }
}

RSI_EMA_trend_new_order <- function(price, params){
  
  rsix <- tail(RSI(price, n = params$RSI_period), 1) # rsi
  emaTrend <- tail(SMA(price, n = params$trend_period), params$trend_duration) # SMA of trend
  Trend_long <- sum(diff(emaTrend) > 0, na.rm = T) == (params$trend_duration - 1) # trend go Long
  Trend_short <- sum(diff(emaTrend) < 0, na.rm = T) == -(params$trend_duration - 1) # trend go SHORT
  emaShort <- tail(SMA(price, n = params$short_EMAPeriod), 2)
  emaLong <- tail(SMA(price, n = params$long_EMAPeriod), 2)
  
  retValue <- 0
  
  # The strategy will go LONG : 
  #     if price[0] > EMA(80), EMA(80)[-3] <= EMA(80)[-2] <= EMA(80)[-1] <= EMA(80)[0]
  #     if RSI[0] >= 50
  #     if EMA(3)[0] > EMA(5)[0] and  EMA(3)[-1] <= EMA(5)[-1]
  if(tail(price, 1) > tail(emaTrend, 1) & 
     Trend_long & 
     rsix >= params$RSI_threshold & 
     emaShort[1] <= emaLong[1] & 
     emaShort[2] > emaLong[2]){ 
    retValue <- 1 
  }
  
  # The strategy will go SHORT：
  #     if price[0] < EMA(80), EMA(80)[-3] >= EMA(80)[-2] >= EMA(80)[-1] >= EMA(80)[0] 
  #     if RSI[-1] <= 50
  #     if EMA(3)[0] < EMA(5)[0] and  EMA(3)[-1] >= EMA[5][-1]
  if(tail(price, 1) < tail(emaTrend, 1) & 
     Trend_short & 
     rsix <= params$RSI_threshold & 
     emaShort[1] >= emaLong[1] & 
     emaShort[2] < emaLong[2]){ 
    retValue <- -1
  }
  return(retValue)
}

RSI_EMA_trend_order <- function(store, params, info){
  
  allzero  <- rep(0, ncol(store)) 
  marketOrders <- allzero
  last_signals <- all_order$RSI_Trend$signals 
  new_signals <- allzero 
  exit_signals <- allzero 
  
  numOfDays   <- nrow(store) 
  
  
  if(numOfDays >= (max(params$trend_period, params$long_EMAPeriod, params$long_EMAExit) + 10) ){

    
    for(i in 1:length(allzero)){
      order_new <- 0
      order_exit <- 0
      
      if(i %in% params$Series){
        # new order signal
        order_new <- RSI_EMA_trend_new_order(store[, i], params)
        if(order_new != 0) new_signals[i] <- order_new - last_signals[i]
        
        # exit order signal
        order_exit <- RSI_EMA_trend_exit_order(last_signals[i], store[, i], params)
        if(order_exit) exit_signals[i] <- (-1) * last_signals[i]
      }
    }
    
    # 2. sum them up:
    market_signals <- new_signals + exit_signals
    
    # 3. convert signals to market_orders
    for(i in 1:length(market_signals)){
      
      if(abs(market_signals[i]) >=2){
        # abs(market_signal) can not exceed 2
        # if exceed 2, sell last_volume, and buy new one share
        marketOrders[i] <- (-1) * all_order$RSI_Trend$signals[i] * all_order$RSI_Trend$volumes[i] + 
          sign(market_signals[i]) * floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        
        # update all_order info
        all_order$RSI_Trend$signals[i] <- sign(market_signals[i])
        all_order$RSI_Trend$volumes[i] <- floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        all_order$RSI_Trend$enterprice[i] <- store[nrow(store), i]
      }else if(abs(market_signals[i]) == 1){
        
        marketOrders[i] <- sign(market_signals[i]) * floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        
        # update all_order info
        all_order$RSI_Trend$signals[i] <- sign(market_signals[i])
        all_order$RSI_Trend$volumes[i] <- floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        all_order$RSI_Trend$enterprice[i] <- store[nrow(store), i]
      }
    }
     
  }
  return(marketOrders)
}



#----- getOrder function  --------

library(TTR)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  marketOrders <- allzero
  
  # check if sum of ratio of each strategy is above 1
  sum_ratio <- sum(sapply(params, function(x)(x$ratio)))
  if(sum_ratio > 1 & is.null(store)) {
    cat("Warning: Sum of ratio is above 1.", sep = "\n")
  }
  
  
  if (is.null(store)) store <- matrix(0, nrow= 0, ncol=length(newRowList))
  # only get Close price 
  data_Close <- sapply(newRowList, function(x) x$Close ) 

  # put Close price into store 
  store <- rbind(store, data_Close) 
  
  # all of the orders from three strategies
  marketOrders1 <- MACD_BBand_order(store, params$MACD_BBand, info)
  marketOrders2 <- RSI_High_order(store, params$RSI_High, info)
  marketOrders3 <- RSI_EMA_trend_order(store, params$RSI_EMA_trend, info)
  
  # Sum of the Orders 
  marketOrders <- marketOrders1 + marketOrders2 + marketOrders3
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}

# from bbands_holding_period.R

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
initStore <- function(newRowList,series) {
  count <- vector(mode="numeric",length=length(series)) 
  return(list(iter=0,cl=initClStore(newRowList,series),count=count))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}