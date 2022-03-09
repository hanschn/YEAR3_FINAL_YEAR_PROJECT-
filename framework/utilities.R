# This function runs a backtest 
# It then uses plotResults from processResults to make a plot
# These plots are saved to pdf and png format

# This function assumes that all the arguments to backest:
# dataList, getOrders, params, and sMult
# are defined in the envionrment

backtestAndPlot <- function(path="./",filename="out",
                            main="",plotType="ggplot2") {
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results,
                             titleString=main,plotType=plotType)
    filepath <- file.path(path,paste(filename,".pdf",sep=''))
    print(filepath)
    dev.copy(pdf,width=10,height=8,file=filepath)
    dev.off()
    filepath <- file.path(path,paste(filename,".png",sep=""))
    print(filepath)
    dev.copy(png,file=filepath)
    dev.off()
}
