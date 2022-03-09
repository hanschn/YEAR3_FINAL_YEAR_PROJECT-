library(gridExtra) # grid.arrange used with ggplot2 to arrange multiple plots in a grid
library(ggplot2)
library(grid)
####################################################################################
# Profit Drawdown ratio 
# (variant of the Calmar ratio using profit/loss)

maxdrawdown <- function (cumPnL) {
        as.numeric(max(cummax(cumPnL)-cumPnL))
}
maxdrawdown.end <- function (cumPnL) {
        index(cumPnL)[which.max(cummax(cumPnL)-cumPnL)]
}
maxdrawdown.start <- function (cumPnL) {
        index(cumPnL)[which.max(cumPnL[1:which.max(cummax(cumPnL)-cumPnL)])]
}
pdratio <- function(cumPnL) {
    mdd <- maxdrawdown(cumPnL) # >= 0
    if (last(cumPnL) > 0)
        ret <- last(cumPnL)/mdd
    else 
        ret <- last(cumPnL)
    return(as.numeric(round(ret,2)))
}
pdratioString <- function(cumPnL) {
    mdd <- maxdrawdown(cumPnL) # >= 0
    pdr <- pdratio(cumPnL) 
    if (last(cumPnL) > 0)
        return(paste("PD ratio = ", round(last(cumPnL),2), "/", round(mdd,2), "=", pdr)) 
    else 
        return(paste("PD ratio = ", round(last(cumPnL),2)))
}

####################################################################################
# Create aggregate equity curve

aggregatePnL <- function(pnlList) {
    pfolioPnL <- xts(rep(0, nrow(pnlList[[1]])), index(pnlList[[1]])) # assumes indexes are all the same

    for (i in 1:length(pnlList)) 
        pfolioPnL <- pfolioPnL + pnlList[[i]]$DailyPnL # aggregate trading results
    
    pfolioPnL <- merge(pfolioPnL,cumsum(pfolioPnL)) # cumulate portfolio pnl
    colnames(pfolioPnL) <- c("DailyPnL","CumPnL")

    return(pfolioPnL)
}

####################################################################################
# Plots the trading results for the portfolio and each individual series

# Three options for graphics:

# 1. ggplot2
# 2. chartSeries from the quantmod package

plotResults <- function(dataList, results, plotType='ggplot2', titleString=NULL) {
    STARTING_CAPITAL <- 1000000

    nseries <- length(dataList)

    pfolioPnL <- aggregatePnL(results$pnlList)
    pfolioNetWorth <- results$netWorthList

    # just CumPnL
    cumpnlList <- lapply(results$pnlList,function(x) x[,"CumPnL"])
    allseries <- do.call(cbind,cumpnlList)
    colnames(allseries) <- 1:nseries

    pdrList <- lapply(results$pnlList,function(x) pdratio(x$CumPnL))

    #toplot <- cbind(pfolioPnL[,"CumPnL"],allseries)
    toplot <- cbind(pfolioNetWorth, allseries)


    # GGPLOT2 code. Old code has been scrapped
    colnames(toplot) <- c(paste("Active on", results$k, "% of days;"), # colname for aggregate series hacked 
                        paste(sprintf("%02d",1:nseries),":"))

    # The first plot is not a return series, so we must subtract the
    # starting capital to get a correct pdratio
    pdRatioLists <- toplot
    pdRatioLists[,1] <- pdRatioLists[,1] - STARTING_CAPITAL
    colnames(toplot) <- paste(colnames(toplot),apply(pdRatioLists, MARGIN=2, pdratioString))

    st <- maxdrawdown.start(pfolioPnL$CumPnL) # start of maximum drawdown
    en <- maxdrawdown.end(pfolioPnL$CumPnL)   # end of maximum drawdown

    # This is the top plot for the aggregate performance
    p1 <- autoplot.zoo(toplot[,1]) # just the first column, i.e., the aggregate results
    p1 <- p1 + facet_wrap(~ Series,scales='free',ncol=2) # this gives facet label for title
    p1 <- p1 + geom_hline(yintercept = STARTING_CAPITAL, colour="red", linetype = "longdash") # start line
    #p1 <- p1 + geom_hline(yintercept = 0, colour="blue") # zero line 
    p1 <- p1 + annotate("rect",xmin=st, xmax=en, ymin=-Inf, ymax=Inf, fill="blue", alpha=0.2)
    p1 <- p1 + theme(axis.title.x=element_blank(), # remove axis labels
                     axis.title.y=element_blank())

    # This is a two-column set of panels showing performane of individual series
    p2 <- autoplot.zoo(toplot[,2:(nseries+1)]) 
    p2 <- p2 + facet_wrap(~ Series,scales='free',ncol=2)
    p2 <- p2 + geom_hline(yintercept = 0, colour="red", linetype = "longdash")
    p2 <- p2 + theme(axis.title.x=element_blank())


    grid.arrange(p1,p2,heights=c(0.75,2),top=textGrob(titleString,gp=gpar(fontsize=20,font=1)))

    return(list(pfoliosPnL=pfolioPnL,
                fitInd=max(unlist(pdrList)),
                fitAgg=pdratio(pfolioPnL$CumPnL)))
}
