source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/Team2.R')

numOfDays <- 1000
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

MACD_BBand$lost_ratio <- seq(from=0.05,to=0.4,by=0.5)
paramsList  <- list(lost_ratio)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=2)
colnames(resultsMatrix) <- c("loss_ratio","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lost in MACD_BBand$loss_ratio) {
        params <- list("MACD_BBand" = list(nFast = 12, nSlow = 26, nSig = 9,
                                                       sdLow = 2, sdHigh = 4.2, nMA = 30,
                                                       Series = c(4,9,10),
                                                       lost_ratio = lost,
                                                       ratio = 0.12),
                                   "RSI_High" = list(rsi_period = 15,
                                                     rsi_threshold = 60,
                                                     Series = c(2,5),
                                                     ema_long = 50,
                                                     ema_short = 20,
                                                     ratio = 0.17),
                                   "RSI_EMA_trend" = list(trend_period = 30,
                                                          trend_duration = 4,#4
                                                          long_EMAPeriod = 10,
                                                          short_EMAPeriod = 5, 
                                                          long_EMAExit = 40,
                                                          short_EMAExit = 15,#15
                                                          RSI_threshold = 50,
                                                          RSI_period = 14,
                                                          Port_lookback = 200,
                                                          Series = c(2,4),
                                                          ratio = 0.17) ) 
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results)
        resultsMatrix[count,] <- c(lost,pfolioPnL$fitAgg)
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1
    }

print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
write.csv(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),],file="All.csv")

