source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('example_strategies.R');

# load data
dataList <- getData(directory="PART1")

sMult <- 0.20 # slippage multiplier

for (strategy in example_strategies) { 
    strategyFile <- file.path('strategies', paste0(strategy,'.R'))
    source(strategyFile) # load in getOrders
    params = example_params[[strategy]]
    backtestAndPlot(path='images', 
                    filename=strategy,
                    main=strategy)
}
