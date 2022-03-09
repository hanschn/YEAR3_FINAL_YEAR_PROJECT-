# FOR A GENERAL EXPLANATION OF REQUIREMENTS ON getOrders see rsi_contrarian.R 

# Marketmaking strategy
# Places buy and sell limit orders around close price
# Spread is determined by daily range
# Unit position sizes for limit orders
# Uses market order to clear inventory when it becomes too large

# Note: limit orders are automatically cancelled at the end of the day

getOrders <- function(store, newRowList, currentPos, info, params) {

    #cat("currentPos", formatC(currentPos,3),"\n")

    # check if current inventory is above a limit and if so exit completely
    # with a market order

    marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)

    # use the range (High-Low) as a indicator for a reasonable "spread" for
    # this pseudo market making strategy
    spread <- sapply(1:length(newRowList),function(i)
                     params$spreadPercentage * (newRowList[[i]]$High -
                                                   newRowList[[i]]$Low))

    limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS
    limitPrices1  <- sapply(1:length(newRowList),function(i) 
                                        newRowList[[i]]$Close - spread[i]/2)

    limitOrders2  <- rep(-1,length(newRowList)) # SELL LIMIT ORDERS
    limitPrices2  <- sapply(1:length(newRowList),function(i) 
                                        newRowList[[i]]$Close + spread[i]/2)

	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=limitOrders1,
	                        limitPrices1=limitPrices1,
	                        limitOrders2=limitOrders2,
	                        limitPrices2=limitPrices2))
}
