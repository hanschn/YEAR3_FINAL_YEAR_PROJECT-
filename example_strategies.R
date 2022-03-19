example_strategies <- c("fixed", 
                        "big_spender",
                        "bankrupt", 
                        "copycat", 
                        "random", 
                        "rsi_contrarian", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit",
                        "extreme_limit",
                        "RSI_EMA",
                        "MACD_BBand",
                        "RSI_EMA_highOrderPortfolio",
                        "MACD_BBand_highOrderPortfolio",
                        "RSI_EMA_trend",
                        "Team2"
                        )

example_params <- list(
                    "fixed"=list(sizes=rep(1,10)),
                    "big_spender"=list(sizes=rep(1,10)),
                    "bankrupt"=list(leverage=40000000),
                    "copycat"=NULL,
                    "random"=list(maxLots=100),
                    "rsi_contrarian"=list(lookback=10,threshold=25,series=1:5),
                    "bbands_contrarian"=list(lookback=20,sdParam=1.5,series=1:4,posSizes=rep(1,10)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,10),holdPeriod=6),
                    "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "extreme_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),

                    "Team2" = list("MACD_BBand" = list(nFast = 12, nSlow = 26, nSig = 9,
                                                       sdLow = 2, sdHigh = 4.2, nMA = 30,
                                                       Series = c(4,9,10),
                                                       lost_ratio = 0.15,
                                                       ratio = 0.13),
                                   "RSI_High" = list(rsi_period = 15,
                                                     rsi_threshold = 60,
                                                     Series = c(2,5),
                                                     ema_long = 50,
                                                     ema_short = 20,
                                                     ratio = 0.18),
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
                                                          ratio = 0.18) )

                    )

load_strategy <- function(strategy) {

    strategyFile <- file.path('strategies', paste0(strategy,'.R'))

    # load strategy
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders

    # set params
    params <<- example_params[[strategy]]
    print("Parameters:")
    print(params)
}
