source('framework/data.R');
source('framework/backtester.R')
source('framework/processResults.R');
source('strategies/bbands_contrarian.R')

numOfDays <- 200
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackSeq <- seq(from=20,to=40,by=10) <- 2 
sdParamSeq <- seq(from=1.5,to=2,by=0.5) <- 3 

paramsList <- list(lookbackSeq,sdParamSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("lookback","sdParam","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lb in lookbackSeq) {
    for (sdp in sdParamSeq) {
        params <- list(lookback=lb,sdParam=sdp,series=1:4,posSizes=rep(1,10))
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results)
        resultsMatrix[count,] <- c(lb,sdp,pfolioPnL$fitAgg)
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
print(resultsMatrix[count,])
count <- count + 1
}
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])