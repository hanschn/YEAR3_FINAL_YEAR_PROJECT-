说明

1.程序运行
A：运行backtest_5.6_demo.Rproj，启动Rstudio； 
B：在Rstudio中运行main_MACD_BBand.R；
C：需要TTR包，如果没有，先安装(install.packages)

2.策略介绍
# This strategy uses only market orders
# (-1 mean the period before last, 0 mean the last period)
# parameters of MACD: nFast = 12, nSlow = 26, nSig = 9
# parameters of bband: sdLow = 1.3, sdHigh = 2, nMA = 10 
# The strategy will go under macd >0：
#     if macd > 0 and price > ma10 + 2SD, go short
#     if price(-1) > ma10 and price(0) < ma10, exit 
#     if macd > 0 and price < ma10 - 1.3SD, go long
#     if price(-1) < ma10 and price(0) > ma10, exit
# The strategy will go under macd <0：
#     if macd < 0 and price > ma10 + 1.3SD, go short
#     if price(-1) > ma10 and price(0) < ma10, exit
#     if macd < 0 and price < ma10 - 2SD, go long
#     if price(-1) < ma10 and price(0) > ma10, exit



4.新的信号/平仓信号出现，仓位计算说明
# last market order have to consider:
# 1: exit current position
# 2: enter new position, they should be rebalanced by current worth and each stock price,  and minus current volume
# 3: the "new"(1+2) positions
新信号有2种形式：
1.出场仓位：这种情况下，只要order_new = -currentPos
2.进场仓位：因为有多只股票，新的信号出现时，需要重新分配资金，每个股票的交易量order_new = 新信号 / (信号加和) * 目前资金 * 投资比例 / 股票价格，然后还要减去现有的持仓
3.仓位汇总： 出场仓位 + 进场仓位
