#
#
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
F_exit_order <- function(cur_pos, ema10, ema25){
  
  if(cur_pos > 0 & ema10 < ema25){ # exit long trade
    return(-cur_pos)
  }else if(cur_pos < 0 & ema10 > ema25){ # exit short trade
    return(-cur_pos)
  }else{ # noop
    return(0)
  }
}

F_new_order <- function(rsix, threshold, ema10, ema25){
  
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
  # return(-ret_x) # reverse trade
}

# add 'highOrderPortfolios' to enhance our trading algorithm
# highOrderPortfolios: risk parity portfolios, which are widely used by practitioners in the financial industry, with the package highOrderPortfolios
# see vignette to get more information about
# devtools::install_github("dppalomar/highOrderPortfolios")
# install devtools-> library(devtools)-> install_github("dppalomar/highOrderPortfolios")
library(highOrderPortfolios)

# use "highOrderPortfolios::design_MVSKtilting_portfolio" function to calc portofolio
# @store: close price of each stock(matrix)
# @order_new: order of step 1(vector)
# return: weights of each stock
F_highOrderPort <- function(store, order_new){
  
  ret_weight <- order_new # return weights of each stock
  
  sum_ord <- sum(abs(order_new))
  if(sum_ord >= 2){
    idx <- max(c(nrow(store) - 300, 1) ):nrow(store) # only use last 200
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


### --- start ---

library(TTR) # load library

getOrders <- function(store, newRowList, currentPos, info, params) {

  #
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  marketOrders <- allzero
  
  
  if (is.null(store)) store <- matrix(0, nrow= 0, ncol=length(newRowList))
  dat <- sapply(newRowList, function(x) x$Close ) # only get 'close' price 
  #
  store <- rbind(store, dat) # store data
  
  # print(paste0("rows of store:", nrow(store)) )
  # print(tail(store, 10))
  
  # last market order have to consider:
  # 1: exit current position
  # # (2: enter new position, they should be rebalanced by current worth and each stock price,  and minus current volume)
  # 2: enter new position, if the signals is greater than 2, they will rebalanced by "highOrderPortfolios::design_MVSKtilting_portfolio"
  # 3: the "new"(1+2) positions
  order_exit <- allzero # 1: exit current position
  order_new <- allzero # 2: enter new position, and they should be rebalanced by current worth and stock price
  
  
  numOfDays   <- nrow(store) # get days of history data
  
  if(numOfDays >= (max(params$rsi_period, params$ema_long) + 1) ){ # data is enough for our strategy
    
    # loop
    for(i in 1:ncol(store)){
      # RSI
      rsi_x <- tail(RSI(store[,i], n = params$rsi_period), 2)

      # EMA
      ema10 <- tail(EMA(store[,i], n = params$ema_short), 1)
      ema25 <- tail(EMA(store[,i], n = params$ema_long), 1)
      
      # 1: exit current position
      order_exit[i] <- F_exit_order(currentPos[i], ema10, ema25)
      
      # 2: enter new position
      order_new[i] <- F_new_order(rsi_x, params$rsi_threshold, ema10, ema25)
      
    }
    
    # # (2: (enter new position), they should be rebalanced by current worth and each stock price,  and minus current volume)
    # sumx <- sum(abs(order_new) )
    # if(sumx > 0){
    #   order_new <- (order_new / sumx ) * info$balance * params$ratio 
    #   # print(order_new); print(store[nrow(store,)])
    #   order_new <- floor(order_new / store[nrow(store), ])
    #   order_new <- order_new - currentPos
    # }
    
    # 2: enter new position, if the signals is greater than 2, they will rebalanced by "highOrderPortfolios::design_MVSKtilting_portfolio"
    order_new <- F_highOrderPort(store, order_new)
    sumx <- sum(abs(order_new) )
    if(sumx > 0){
        order_new <- order_new * info$balance * params$ratio
        # print(order_new); print(store[nrow(store,)])
        order_new <- floor(order_new / store[nrow(store), ])
        order_new <- order_new - currentPos
    }
    
    
    # 3: the "new"(1+2) positions
    marketOrders <- order_exit + order_new
    
  }
  # for debug
  # print(currentPos)
  # print(order_exit)
  # print(sum(abs(order_new[order_new !=0]) ))
  # print(order_new)
  # print(marketOrders)
  
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
  count <- vector(mode="numeric",length=length(series)) # stores # of days in trade
  return(list(iter=0,cl=initClStore(newRowList,series),count=count))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}