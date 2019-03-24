## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, eval = FALSE, comment = "#>")

## ----`pullit equity BBG`, message = FALSE, warning = FALSE---------------
#  library(pullit); library(lubridate)
#  
#  tickers_equity <- c("ADM US Equity", "CIVI US Equity", "GBX US Equity", "LIND US Equity",
#                      "SERV US Equity", "AE US Equity", "CLGX US Equity", "GDI US Equity",
#                      "LZB US Equity", "SGA US Equity", "AGCO US Equity", "CLR US Equity",
#                      "GHC US Equity", "MAN US Equity", "SITE US Equity", "AJRD US Equity",
#                      "COMM US Equity", "GME US Equity", "MEI US Equity", "SMP US Equity")
#  end <- Sys.Date(); start <- end - years(2L)
#  
#  equity_data_market <- pull_equity_market(source = "Bloomberg", tickers_equity, start, end, verbose = FALSE)
#  
#  get_data(equity_data_market)

## ----`pullit equity storethat`, echo = FALSE, message = FALSE, warning = FALSE, eval = F----
#  library(pullit); library(lubridate); storethat <- "../data-raw/storethat.sqlite"
#  
#  tickers_equity <- c("ADM US Equity", "CIVI US Equity", "GBX US Equity", "LIND US Equity",
#                      "SERV US Equity", "AE US Equity", "CLGX US Equity", "GDI US Equity",
#                      "LZB US Equity", "SGA US Equity", "AGCO US Equity", "CLR US Equity",
#                      "GHC US Equity", "MAN US Equity", "SITE US Equity", "AJRD US Equity",
#                      "COMM US Equity", "GME US Equity", "MEI US Equity", "SMP US Equity")
#  end <- Sys.Date(); start <- end - years(2L)
#  
#  equity_data_market <- pull_equity_market(source = "storethat", tickers = tickers_equity,
#                                           start = start, end = end, file = storethat, verbose = FALSE)
#  
#  get_data(equity_data_market)

## ----`factorem factor`, message = FALSE, warning = FALSE, eval = F-------
#  library(factorem)
#  
#  ranking_period = 1L
#  factor <- factorem(name = "factorem", data = pullit::get_data(equity_data_market),
#                     ranking_period = ranking_period)

## ----`equity market`, message = FALSE, warning = FALSE-------------------
#  equity_market <- market_factor(data = equity_data_market)

## ----`equity momentum`, message = FALSE, warning = FALSE-----------------
#  ranking_period = 1L
#  equity_momentum <- momentum_factor(data = equity_data_market, ranking_period = ranking_period)

## ----`pullit futures`, message = FALSE, warning = FALSE------------------
#  tickers_futures <- c("C A Comdty", "CCA Comdty", "CLA Comdty", "CTA Comdty", "FCA Comdty",
#                       "GCA Comdty", "HGA Comdty", "HOA Comdty", "KCA Comdty", "KWA Comdty",
#                       "LBA Comdty", "LCA Comdty", "LHA Comdty", "NGA Comdty", "O A Comdty",
#                       "PAA Comdty", "S A Comdty", "SIA Comdty", "W A Comdty", "XBA Comdty")
#  
#  futures_data_TS <- pull_futures_market(source = "storethat", type = "term structure", tickers_futures,
#                                         start, end, file = storethat, verbose = FALSE)
#  
#  get_data(futures_data_TS)

## ----`futures market`, message = FALSE, warning = FALSE------------------
#  futures_market <- market_factor(data = futures_data_TS)

## ----`futures momentum`, message = FALSE, warning = FALSE----------------
#  ranking_period = 1L
#  futures_momentum <- momentum_factor(data = futures_data_TS, ranking_period = ranking_period)

## ----`futures CHP`, message = FALSE, warning = FALSE---------------------
#  futures_data_CFTC <- pull_futures_CFTC(source = "Bloomberg", tickers_futures, start, end, verbose = FALSE)
#  
#  ranking_period = 1L
#  futures_CHP <- CHP_factor(price_data = futures_data_TS, CHP_data = futures_data_CFTC,
#                            ranking_period = ranking_period)

## ----`futures OI nearby`, message = FALSE, warning = FALSE---------------
#  ranking_period = 1L
#  futures_OI_nearby <- OI_nearby_factor(data = futures_data_TS, ranking_period = ranking_period)

## ----`futures OI aggregate`, message = FALSE, warning = FALSE------------
#  futures_data_agg <- pull_futures_market(source = "Bloomberg", type = "aggregate", tickers_futures,
#                                          start, end, verbose = FALSE)
#  
#  ranking_period = 1L
#  futures_OI_aggregate <- OI_aggregate_factor(price_data = futures_data_market,
#                                              aggregate_data = futures_data_agg,
#                                              ranking_period = ranking_period)

## ----`futures TS`, message = FALSE, warning = FALSE----------------------
#  ranking_period = 1L
#  futures_TS <- TS_factor(data = futures_data_market, ranking_period = ranking_period)

## ----`factor name`, eval = F---------------------------------------------
#  get_name(factor)

## ----`factor positions`, eval = F----------------------------------------
#  get_positions(factor)

## ----`factor returns`, eval = F------------------------------------------
#  get_returns(factor)

## ----`factor data`, eval = F---------------------------------------------
#  get_data(factor)

## ----`factor params`, eval = F-------------------------------------------
#  get_parameters(factor)

## ----`factor call`, eval = F---------------------------------------------
#  get_call(factor)

## ----`factor summary`, eval = F------------------------------------------
#  summary(factor)

## ----`plot performance`, fig.width = 7.5, fig.height = 5.5, fig.fullwidth = TRUE, eval = F, message = FALSE, warning = FALSE----
#  library(plotit)
#  
#  plot(factor, type = "performance")

## ----`plot positions`, fig.width = 7.5, fig.height = 6.5, fig.fullwidth = TRUE, eval = F----
#  plot(factor, type = "positions")

