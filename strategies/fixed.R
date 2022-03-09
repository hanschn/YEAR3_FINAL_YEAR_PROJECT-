# This strategy uses only market orders (and only in the first period)

# Holds a fixed number of contracts (long or short) in each market for the duration

# This works by placing a market order on the first iteration
# Then no further orders are placed
# The backtester automatically exits all positions as market orders at the end

getOrders <- function(store, newRowList, currentPos, info, params) {

    allzero      <- rep(0,length(newRowList)) 
    marketOrders <- allzero

    if (is.null(store)) { 
        # take position during first iteration and hold
        marketOrders <- params$sizes
        store <- 1 # not null
    }
	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=allzero, 
	                        limitPrices1=allzero,
	                        limitOrders2=allzero,
	                        limitPrices2=allzero))
}
