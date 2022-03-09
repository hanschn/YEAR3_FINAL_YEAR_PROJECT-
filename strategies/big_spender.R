# This strategy is similar to fixed.R, but it uses market orders to buy or sell
# a fixed number of units *every day*. 

# Note that if the parameter is set too large (as it is by default), then it
# will eventually run out of money.

# The strategy tries to estimate how much money it will need to spend, and if it
# detects that it will spend more than it has, it will cancel its orders

getOrders <- function(store, newRowList, currentPos, info, params) {

    allzero      <- rep(0,length(newRowList)) 
    marketOrders <- allzero

    marketOrders <- params$sizes

    # Remeber yesterday's close prices in the store
    store <- sapply(newRowList, function(x) x$Close) 


    # These functions estimate the amount of money that our market orders will
    # spend (or receive). 
    # It assumes that we trade at today's close price. Our actual order will
    # execute at tomorrow's open price.
    # It calculates slippage using the difference between yesterday's close
    # price and today's open. Our actual order will incur slippage based on the
    # difference between today's close and tomorrow's open
    priceEst <- function(x) marketOrders[[x]] * newRowList[[x]]$Close
    slippageEst <- function(x) 0.2 * abs(marketOrders[[x]]) * abs(store[[x]] - newRowList[[x]]$Open)

    totalPrice <- sum(sapply(1:length(newRowList), priceEst))
    totalSlippage <- sum(sapply(1:length(newRowList), slippageEst))

    # If our estimate of the amount of money we have available to spend is
    # greater than what we have left, cancel our orders for today
    if(totalPrice + totalSlippage > info$balance)
    {
        marketOrders <- allzero
    }
    

	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=allzero, 
	                        limitPrices1=allzero,
	                        limitOrders2=allzero,
	                        limitPrices2=allzero))
}
