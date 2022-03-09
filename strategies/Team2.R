# Parameters:
# "Team2" = list(
#     "MACD_BBand" = list(nFast = 12, nSlow = 26, nSig = 9,
#     nMA = 30, IndexOfStocks = c(4,9,10),
#     lost_ratio = 0.15, 
#     ratio = 0.28),
#     "RSI_High" = list(rsi_period = 15,
#     rsi_threshold = 60,
#     IndexOfStocks = c(2,5),
#     ema_long = 50, 
#     ema_short = 20,
#     ratio = 0.15),
#     "RSI_EMA_trend" = list(trend_period = 30,
#     trend_duration = 4,
#     long_EMAPeriod = 10,
#     short_EMAPeriod = 5,
#     long_EMAExit = 40,
#     short_EMAExit = 15,
#     RSI_threshold = 50,
#     RSI_period = 14,
#     Port_lookback = 200,
#     IndexOfStocks = c(4,2),
#     ratio = 0.15) )

### ---- Parameters END ----
# Position Sizing: 
# Mixed strategy "MACD_BBand" : 28%, "RSI_High(orderPortfolio)" : 15%, "EMA_trend": 15% 

# highOrderPortfolios: risk parity portfolios, which are widely used by practitioners in the financial industry, with the package highOrderPortfolios
# see vignette to get more information about
# The github package <- HighOrderPortfolio 
# devtools::install_github("dppalomar/highOrderPortfolios")
library(highOrderPortfolios)

# g_table <- data.frame(matrix(NA, ncol = 10 + 1))
# colnames(g_table) <- c("Pos", paste0("Signal_", 1:10))

###----- VARIABLE FOR ALL STRATEGY ----------
# To hold last order of each strategy,
# because it can not be infered from current order
# (do not use currentPos in function 'getOrders') 
# rep(0, 10) is hard code, for there are 10 stocks(assets)
Order_info <- list(signals = rep(0, 10), # sign of trades(-1 mean sell, 0 mean nop, 1 mean buy)
                   volumes = rep(0, 10), # volume of trades
                   enterPrice = rep(0, 10) # price when enter last trading, which can be inferred when stop loss 
)

G_order <- list(MACD_BBand = Order_info,
                RSI_High = Order_info,
                RSI_Trend = Order_info)

###----- Order from MACD_BBand ---------------
# This strategy uses only market orders
# (-1 mean the period before last, 0 mean the last period)
# parameters of MACD: nFast = 12, nSlow = 26, nSig = 9
# parameters of bband: sdLow = 1.3, sdHigh = 2, nMA = 10 
# The strategy will go under macd >0：
#     if macd > 0 and price > ma10 + 2SD, go short
#     if price(-1) > ma10 and price(0) < ma10, exit 
#     if macd > 0 and price < ma10 - 1.3SD, go long
#     if price(-1) < ma10 and price(0) > ma10, exit
# The strategy will go under macd <0：
#     if macd < 0 and price > ma10 + 1.3SD, go short
#     if price(-1) > ma10 and price(0) < ma10, exit
#     if macd < 0 and price < ma10 - 2SD, go long
#     if price(-1) < ma10 and price(0) > ma10, exit
# and will be flat otherwise

### function to exit current trade
# @cur_signal: current signal, 1 mean long, -1 mean short
# @price: price value, [1] is value before last, [2] is the last value
# @maValue: move average price
# return:  1 if exit, otherwise 0
MACD_BBand_exit_order <- function(cur_signal, price, maValue){
  
  retValue <- 0
  if(cur_signal > 0 & price[1] > maValue & price[2] < maValue){ # exit long trade
    retValue <- 1
  }else if(cur_signal < 0 & price[1] < maValue & price[2] > maValue){ # exit short trade
    retValue <- 1
  }
  
  return(retValue)
}

# @price: price 
# @macdValue: value of macd
# @maValue: move average of price
# @sdValue: sd value
# return: 1 mean long, -1 mean short
F_MACD_BBand_new_order <- function(price, macdValue, maValue, sdValue){
  
  retValue <- 0
  
  price1 <- tail(price, 1)
  if(macdValue > 0){ # if macd > 0
    if(price1 > (maValue + 4.2 * sdValue)){# if macd > 0 and price > ma10 + 2SD, go short
      retValue <- (-1)
    }else if(price1 < (maValue - 2 * sdValue)){ # if macd > 0 and price < ma10 - 1.3SD, go long
      retValue  <- (1)
    }
  }else if(macdValue < 0){ # if macd < 0
    if(price1 > (maValue + 4.2 * sdValue)){
      retValue <- (-1)
    }else if(price1 < (maValue - 2 * sdValue)){
      retValue <- (1)
    }
  }
  return(retValue)
}

# function to evalue stop loss
# @store: price of stocks
# @params: parameters of "MACD_BBand", here we use lost_ratio(0.15)
# return: if current value of "MACD_BBand" position above the lost_ratio, return TRUE(means exit current "MACD_BBand"), else FALSE
F_MACD_BBand_stop_loss <- function(store, params){
  
  retValue <- rep(FALSE, ncol(store))
  
  for(i in 1:length(retValue)){
    
    if(G_order$MACD_BBand$enterPrice[i] != 0){ # enterPrice must no be 0
      # must consider buy or sell order
      cur_rate <- (store[nrow(store), i]/G_order$MACD_BBand$enterPrice[i]) ^ (G_order$MACD_BBand$signals[i])
      # stop loss when below lost_ratio
      if(cur_rate < (1 - params$lost_ratio)){ 
        retValue[i] <- TRUE
      }
    }
    
  }
  
  return(retValue)
}


F_MACD_BBand_order <- function(store, params, info){
  
  allzero  <- rep(0,ncol(store)) # used for initializing vectors
  marketOrders <- allzero
  
  
  # 1. get current stop_loss/new/exit signals
  #       if loss_signal == TRUE, loss_signal =  - last_signal[i]
  #       if new_signal !=0, new_signal = new_signal - last_signal[i]
  #       if exit_signal == TRUE, exit_signal = -last_signal[i]
  # 2. sum them up:
  #       market_signal = loss_signal + new_signal + exit_signal
  # 3. convert signals to market_orders
  # abs(market_signal) must not exceed 2
  #       if abs(market_signal) >=2, exit last_volume and new current signal
  #       else if abs(market_signal) ==2, new current signal
  #       else if "Do nothing"
  
  last_signals <- G_order$MACD_BBand$signals # get last signals
  loss_signals <- allzero # get current stop loss signals(position)
  new_signals <- allzero # get current new order signals(position)
  exit_signals <- allzero # get current exit order signals(position)
  
  
  numOfDays   <- nrow(store) # get days of history data
  
  if(numOfDays >= (max(params$nSlow, params$nFast) + 10) ){ # data is enough for our strategy
    
    loss_signals <- F_MACD_BBand_stop_loss(store, params)
    
    loss_signals[base::setdiff(c(1:length(loss_signals)), params$IndexOfStocks)] <- 0
    
    for(i in 1:length(allzero)){
        
      order_new <- 0
      order_exit <- 0
      
      if(i %in% params$IndexOfStocks){
        
        # stop loss signal
        if(loss_signals[i]) loss_signals[i] <- (-1) * last_signals[i]
        
        
        price <- tail(store[,i], 2) # close price
        # macd, nFast = 12, nSlow = 26, nSig = 9
        macd <- tail(MACD(store[,i], params$nFast, params$nSlow, params$nSig)[, "macd"], 1)
        # bband, sdLow = 1.3, sdHigh = 2, nMA = 10
        bb <- BBands(store[, i], n = params$nMA) # BBand of close price
        bb_ma <- tail(bb[,"mavg"], 1)
        bb_sd <- (tail(bb[,"up"], 1) - bb_ma) / 2
        
        
        # new order signal
        order_new <- F_MACD_BBand_new_order(price, macd, bb_ma, bb_sd)
        if(order_new != 0) new_signals[i] <- order_new - last_signals[i]
        
        # exit order signal
        order_exit <- MACD_BBand_exit_order(last_signals[i], price, bb_ma)
        if(order_exit) exit_signals[i] <- (-1) * last_signals[i]
      }
    }
    
    # 2. sum them up:
    market_signals <- loss_signals + new_signals + exit_signals
    
    # 3. convert signals to market_orders
    for(i in 1:length(market_signals)){
      
      if(abs(market_signals[i]) >=2){
        # abs(market_signal) can not exceed 2
        # if exceed 2, sell last_volume, and buy new one share
        marketOrders[i] <- (-1) * G_order$MACD_BBand$signals[i] * G_order$MACD_BBand$volumes[i] + 
          sign(market_signals[i]) * floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        
        # update G_order info
        G_order$MACD_BBand$signals[i] <- sign(market_signals[i])
        G_order$MACD_BBand$volumes[i] <- floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        G_order$MACD_BBand$enterprice[i] <- store[nrow(store), i]
      }else if(abs(market_signals[i]) == 1){
        
        marketOrders[i] <- sign(market_signals[i]) * floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        
        # update G_order info
        G_order$MACD_BBand$signals[i] <- sign(market_signals[i])
        G_order$MACD_BBand$volumes[i] <- floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        G_order$MACD_BBand$enterprice[i] <- store[nrow(store), i]
      }
    }
  }
  return(marketOrders)
}

### end of MACD_BBand ####
# HANS



###----- Order from RSI_EMA_highOrderPortolio ------------------

# This strategy uses only market orders
# (-1 mean the period before last, 0 mean the last period)
# The strategy will go short：
#     if RSI(-1) >= 50, RSI(-1) > RSI(0), EMA(10) < EMA(25)
#     if EMA(10) > EMA(25), exit
# The strategy will go long：
#     if RSI(1) <= 50, RSI(-1) < RSI(0), EMA(10) > EMA(25)
#     if EMA(10) < EMA(25), exit
# and will be flat otherwise

### function to exit current trade
# @cur_pos: current position, >0 mean long, <0 mean short
# @ema10: EMA(10) value
# @ema25: EMA(25) value
# return: clear current trade if conditions match, otherwise 0
F_RSI_High_exit_order <- function(cur_signal, ema10, ema25){
  
  if(cur_signal > 0 & ema10 < ema25){ # exit long trade
    return(-cur_signal)
  }else if(cur_signal < 0 & ema10 > ema25){ # exit short trade
    return(-cur_signal)
  }else{ # noop
    return(0)
  }
}

F_RSI_High_new_order <- function(rsix, threshold, ema10, ema25){
  ret_x <- 0 # return value
  # [1] mean before last, [2] mean last
  if(rsix[1] >= threshold & rsix[1] > rsix[2] & ema10 < ema25){ # if RSI(-1) >= 50, RSI(-1) > RSI(0), EMA(10) < EMA(25), short
    ret_x <- -1
  }else if(rsix[1] <= (100-threshold) & rsix[1] < rsix[2] & ema10 > ema25){ # if RSI(-1) <= 50, RSI(-1) < RSI(0), EMA(10) > EMA(25), long
    ret_x <- 1
  }else{
    ret_x <- 0
  }
  return(ret_x) # 
}

# use "highOrderPortfolios::design_MVSKtilting_portfolio" function to calc portofolio
# @store: close price of each stock(matrix)
# @order_new: order of step 1(vector)
# return: weights of each stock
F_highOrderPort <- function(store, order_new, params){
  
  ret_weight <- order_new # return weights of each stock
  
  sum_ord <- sum(abs(order_new))
  if(sum_ord >= 2){
    idx <- max(c(nrow(store) - 300, 1) ):nrow(store) # only use last 300
    mat <- store[idx, abs(order_new) == 1] # get 'return' matrix
    
    # mat <- store[, abs(order_new) == 1] # get 'return' matrix
    # estimate moments
    X_moments <- estimate_moments(mat, adjust_magnitude = TRUE)
    
    w0 <- rep(1 / sum_ord, sum_ord)
    w0_moments <- eval_portfolio_moments(w0, X_moments)
    d <- abs(w0_moments) 
    kappa <- 0.3 * sqrt(w0 %*% X_moments$Sgm %*% w0)
    
    # portfolio optimization
    sol <- design_MVSKtilting_portfolio(d, X_moments, w_init = w0, w0 = w0, 
                                        w0_moments = w0_moments, kappa = kappa, 
                                        ftol = 1e-10)
    ret_weight[abs(order_new) == 1] <- sol$w
  }
  
  return(ret_weight)
}

F_RSI_High_order <- function(store, params, info){
  
  #
  allzero  <- rep(0, ncol(store)) # used for initializing vectors
  marketOrders <- allzero
  cur_signal <- G_order$RSI_High$signals # get current RSI_Trend order(position)
  
  order_exit <- allzero # 1: exit current position
  order_new <- allzero # 2: enter new position, and they should be rebalanced by current worth and stock price
  
  numOfDays   <- nrow(store) # get days of history data
  
  if(numOfDays >= (max(params$rsi_period, params$ema_long) + 10) ){ # data is enough for our strategy
    
    # loop
    for(i in 1:ncol(store)){

      
      if(i %in% params$IndexOfStocks){
        # RSI
        rsi_x <- tail(RSI(store[,i], n = params$rsi_period), 2)
        # EMA
        ema10 <- tail(EMA(store[,i], n = params$ema_short), 1)
        ema25 <- tail(EMA(store[,i], n = params$ema_long), 1)
        
        # 1: exit current position
        order_exit[i] <- F_RSI_High_exit_order(cur_signal[i], ema10, ema25)
        # 2: enter new position
        order_new[i] <- F_RSI_High_new_order(rsi_x, params$rsi_threshold, ema10, ema25)
      }
      
    }
    
    # order of new, if new order arrived, exit order must triggered,
    # so this must be executed before 'order of exit'
    mktorder_new <- allzero
    for(i in 1:length(order_new)){
      
      if((order_new[i] == -1 * (G_order$RSI_High$signals[i])) & order_new[i] != 0){
        order_exit[i] <- (-G_order$RSI_High$signals)
        mktorder_new[i] <- order_new[i]
      }
      
      if(order_new[i] != 0){ # new order
        mktorder_new[i] <- order_new[i]
      }else{ # no new order, same as old signal
        mktorder_new[i] <- G_order$RSI_High$signals[i]
      }
      
    }
    
    if(sum(mktorder_new == G_order$RSI_High$signals) != length(mktorder_new) | (sum(abs(order_exit)) != 0)){
      
      mktvolume_new <- allzero
      
      sumx <- sum(abs(mktorder_new) )
      if(sumx > 0){
        mktorder_new <- F_highOrderPort(store, mktorder_new) # rebalance with HighOrderPortfolio
        
        mktvolume_new <- (mktorder_new / sumx ) * info$balance * params$ratio 
        # print(order_new); print(price[nrow(price,)])
        mktvolume_new <- floor(mktvolume_new / store[nrow(store), ])
      }
      
      # order of exit
      mktvolume_exit <- order_exit * G_order$RSI_High$volumes
      
      # market orders
      marketOrders <- mktvolume_new + mktvolume_exit
      
      # update global information 
      G_order$RSI_High$signals <- mktorder_new
      G_order$RSI_High$volumes <- mktvolume_new
      G_order$RSI_High$enterPrice <- info$balance * params$ratio
    }
    
  }
  
  # return
  return(marketOrders)
}



### end of RSI_EMA_highOrderPortolio

###----- Order from RSI_EMA_trend ------------------------------
### function to exit current trade
# @cur_pos: current position, >0 mean long, <0 mean short
# @price: price of one stock
# @pars: parameters holded to calculate long/short EMA
# return: 1 if exit current position, otherwise 0
F_RSI_EMA_trend_exit_order <- function(cur_signal, price, pars){
  
  emaShort <- tail(SMA(price, n = pars$short_EMAExit), 1)
  emaLong <- tail(SMA(price, n = pars$long_EMAExit), 1)
  if(cur_signal > 0 & emaShort < emaLong){ # exit long trade
    return(1)
  }else if(cur_signal < 0 & emaShort > emaLong){ # exit short trade
    return(1)
  }else{ # noop
    return(0)
  }
}

### function to new trade
# @price: price of one stock
# @pars: parameters holded to calculate Trading Rules
# return: new trade if conditions match, 1 for Long, -1 for Short, otherwise 0
F_RSI_EMA_trend_new_order <- function(price, pars){
  
  rsix <- tail(RSI(price, n = pars$RSI_period), 1) # rsi
  emaTrend <- tail(SMA(price, n = pars$trend_period), pars$trend_duration) # SMA of trend
  Trend_long <- sum(diff(emaTrend) > 0, na.rm = T) == (pars$trend_duration - 1) # trend go Long
  Tredn_short <- sum(diff(emaTrend) < 0, na.rm = T) == -(pars$trend_duration - 1) # trend go SHORT
  emaShort <- tail(SMA(price, n = pars$short_EMAPeriod), 2)
  emaLong <- tail(SMA(price, n = pars$long_EMAPeriod), 2)
  
  retValue <- 0
  
  # The strategy will go LONG if all following rules matched：
  #     if price[0] > EMA(80), EMA(80)[-3] <= EMA(80)[-2] <= EMA(80)[-1] <= EMA(80)[0](current price is above EMA80, and EMA80 is moving up for 4 period)
  #     if RSI[0] >= 50
  #     if EMA(3)[0] > EMA(5)[0] and  EMA(3)[-1] <= EMA(5)[-1](EMA3 is cross above EMA5)
  if(tail(price, 1) > tail(emaTrend, 1) & # price[0] > EMA(80)
     Trend_long & # trend go Long(EMA80 is moving up for 4 period)
     rsix >= pars$RSI_threshold & # RSI[0] >= 50
     emaShort[1] <= emaLong[1] & # EMA(3)[-1] <= EMA(5)[-1]
     emaShort[2] > emaLong[2]){ # EMA(3)[0] > EMA(5)[0]
    retValue <- 1
  }
  
  # The strategy will go SHORT：
  #     if price[0] < EMA(80), EMA(80)[-3] >= EMA(80)[-2] >= EMA(80)[-1] >= EMA(80)[0](current price is below EMA80, and EMA80 is moving down for 4 period)
  #     if RSI[-1] <= 50
  #     if EMA(3)[0] < EMA(5)[0] and  EMA(3)[-1] >= EMA[5][-1](EMA3 is cross below EMA5)
  if(tail(price, 1) < tail(emaTrend, 1) & # price[0] < EMA(80)
     Tredn_short & # trend go SHORT(EMA80 is moving down for 4 period)
     rsix <= pars$RSI_threshold & # RSI[0] <= 50
     emaShort[1] >= emaLong[1] & # EMA(3)[-1] >= EMA(5)[-1]
     emaShort[2] < emaLong[2]){ # EMA(3)[0] < EMA(5)[0]
    retValue <- -1
  }
  return(retValue)
}

F_RSI_EMA_trend_order <- function(store, params, info){
  
  allzero  <- rep(0, ncol(store)) # used for initializing vectors
  marketOrders <- allzero
  last_signals <- G_order$RSI_Trend$signals # get last signals
  # loss_signals <- allzero # get current stop loss signals(position)
  new_signals <- allzero # get current new order signals(position)
  exit_signals <- allzero # get current exit order signals(position)
  
  numOfDays   <- nrow(store) # get days of history data
  
  
  if(numOfDays >= (max(params$trend_period, params$long_EMAPeriod, params$long_EMAExit) + 10) ){ # data is enough for our strategy

    
    for(i in 1:length(allzero)){
      order_new <- 0
      order_exit <- 0
      
      if(i %in% params$IndexOfStocks){
        # new order signal
        order_new <- F_RSI_EMA_trend_new_order(store[, i], params)
        if(order_new != 0) new_signals[i] <- order_new - last_signals[i]
        
        # exit order signal
        order_exit <- F_RSI_EMA_trend_exit_order(last_signals[i], store[, i], params)
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
        marketOrders[i] <- (-1) * G_order$RSI_Trend$signals[i] * G_order$RSI_Trend$volumes[i] + 
          sign(market_signals[i]) * floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        
        # update G_order info
        G_order$RSI_Trend$signals[i] <- sign(market_signals[i])
        G_order$RSI_Trend$volumes[i] <- floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        G_order$RSI_Trend$enterprice[i] <- store[nrow(store), i]
      }else if(abs(market_signals[i]) == 1){
        
        marketOrders[i] <- sign(market_signals[i]) * floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        
        # update G_order info
        G_order$RSI_Trend$signals[i] <- sign(market_signals[i])
        G_order$RSI_Trend$volumes[i] <- floor(info$balance * params$ratio / 10 / store[nrow(store), i])
        G_order$RSI_Trend$enterprice[i] <- store[nrow(store), i]
      }
    }
    
    
  }
  
  return(marketOrders)
}


###----- getOrder function start --------


library(TTR) # load library

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  #
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  marketOrders <- allzero
  
  # check if sum of ratio of each strategy is above 1
  sum_ratio <- sum(sapply(params, function(x)(x$ratio)))
  if(sum_ratio > 1 & is.null(store)) {
    cat("Warning: Sum of ratio is above 1.", sep = "\n")
  }
  
  
  if (is.null(store)) store <- matrix(0, nrow= 0, ncol=length(newRowList))
  dat <- sapply(newRowList, function(x) x$Close ) # only get 'close' price 
  #
  store <- rbind(store, dat) # store data
  
  # print(paste0("rows of store:", nrow(store)) )
  # print(tail(store, 10))
  
  # orders from three strategies
  marketOrders1 <- F_MACD_BBand_order(store, params$MACD_BBand, info)
  marketOrders2 <- F_RSI_High_order(store, params$RSI_High, info)
  marketOrders3 <- F_RSI_EMA_trend_order(store, params$RSI_EMA_trend, info)
  
  marketOrders <- marketOrders1 + marketOrders2 + marketOrders3
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}

# copy from bbands_holding_period.R
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