# This strategy deliberately attempts to bankrupt itself by accumulating a (by
# default) £40M short position in series 1 through 5, and then investing ~£41M
# in series 6 through 20, giving a gearing ratio of 40:1

# This is a very risky strategy that is very likely to go bankrupt!

getOrders <- function(store, newRowList, currentPos, info, params) {

    allzero      <- rep(0,length(newRowList)) 
    marketOrders <- allzero

    if (is.null(store)) { 
        # Take our short position on the first day

        openPrices <- lapply(newRowList, function(x) x$Open)
        
        # Work out how many units = £1 for the first five series
        onePound <- lapply(openPrices[1:5], function(x) 1/x)

        # Work out how many units to short to get an exposure of leverage/5
        # on each of the first five series
        shorts <- lapply(onePound, function(x) -params$leverage*x/5)

        # Now setup marketOrders to execute the short
        marketOrders <- c(shorts, rep(0, 5))

        store <- 1 # signals end of first day
    }
    else if(store == 1)
    {
        # Take our long positions on the second day

        # Let's leave a cash buffer of £1M to make sure our trades go through,
        # as the backtester may cancel our orders if we attempt to spend all of
        # our money
        toSpend <- info$balance - 1000000

        openPrices <- lapply(newRowList, function(x) x$Open)
        
        # Work out how many units = £1 for the last five series
        onePound <- lapply(openPrices[6:10], function(x) 1/x)

        # Work out how many units to buy to get an exposure of toSpend/5
        # on each of the last five series
        longs <- lapply(onePound, function(x) toSpend*x/5)

        # Now setup marketOrders to execute the buy
        marketOrders <- c(rep(0, 5), longs)

        store <- 2 # signals end of second day
    }
	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=allzero, 
	                        limitPrices1=allzero,
	                        limitOrders2=allzero,
	                        limitPrices2=allzero))
}
