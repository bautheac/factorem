## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, eval = T, comment = "#>")
folder <- "literature_files"; dir.create(folder)
download.file("https://www.dropbox.com/s/htnd7o9nnkk8ng8/references.bib?dl=1", paste(folder, "references.bib", sep = "/"))
path <- here::here("development", "storethat.sqlite")

## ----`pullit equity bbg`, message = FALSE, warning = FALSE, eval = F, echo = T----
#  library(pullit); library(lubridate)
#  
#  tickers_equity <- c("LZB US Equity", "SGA US Equity", "AGCO US Equity", "CLR US Equity",
#                      "GHC US Equity", "MAN US Equity", "SITE US Equity", "AJRD US Equity",
#                      "COMM US Equity", "GME US Equity", "MEI US Equity", "SMP US Equity")
#  start <- "2016-01-01"; end <- "2017-12-31"
#  
#  equity_data_market <- pull_equity_market(source = "Bloomberg", tickers_equity, start, end, verbose = FALSE)
#  
#  get_data(equity_data_market)

## ----`pullit equity storethat`, message = FALSE, warning = FALSE, echo = F, eval = T----
library(pullit); library(lubridate)

tickers_equity <- c("LZB US Equity", "SGA US Equity", "AGCO US Equity", "CLR US Equity", 
                    "GHC US Equity", "MAN US Equity", "SITE US Equity", "AJRD US Equity", 
                    "COMM US Equity", "GME US Equity", "MEI US Equity", "SMP US Equity")
# end <- Sys.Date(); start <- end - years(2L)
start <- "2016-01-01"; end <- "2017-12-31"

equity_data_market <- pull_equity_market(
  source = "storethat", tickers = tickers_equity, start = start, end = end,
  verbose = FALSE, file = path
  )

get_data(equity_data_market)

## ----`factorem factor`, message = FALSE, warning = FALSE----------------------
library(factorem)

ranking_period = 1L
factor <- factorem(
  name = "momentum", data = pullit::get_data(equity_data_market),
  ranking_period = ranking_period, sort_levels = FALSE, weighted = FALSE
  )
factor

## ----`equity market`, message = FALSE, warning = FALSE------------------------
equity_market <- market_factor(data = equity_data_market)
equity_market

## ----`equity momentum`, message = FALSE, warning = FALSE----------------------
ranking_period = 1L
equity_momentum <- momentum_factor(data = equity_data_market, ranking_period = ranking_period)
equity_momentum

## ----`pullit futures bbg`, message = FALSE, warning = FALSE, echo = T, eval = F----
#  tickers_futures <- c(
#    'LNA Comdty', 'LPA Comdty', 'LTA Comdty', 'LXA Comdty', 'NGA Comdty', 'O A Comdty',
#    'PAA Comdty', 'PLA Comdty', 'S A Comdty', 'SBA Comdty', 'SIA Comdty', 'SMA Comdty',
#    'W A Comdty', 'XBWA Comdty'
#    )
#  
#  futures_data_TS <- pull_futures_market(source = "storethat", type = "term structure", tickers_futures,
#                                         start, end, file = storethat, verbose = FALSE)
#  
#  pullit::get_data(futures_data_TS)

## ----`pullit futures storethat`, message = FALSE, warning = FALSE, echo = F, eval = T----
tickers_futures <- c(
  'LNA Comdty', 'LPA Comdty', 'LTA Comdty', 'LXA Comdty', 'NGA Comdty', 'O A Comdty', 
  'PAA Comdty', 'PLA Comdty', 'S A Comdty', 'SBA Comdty', 'SIA Comdty', 'SMA Comdty', 
  'W A Comdty', 'XBWA Comdty'
  )

futures_data_TS <- pull_futures_market(
  source = "storethat", type = "term structure", tickers_futures, start, end, 
  verbose = FALSE, file = path
  )

pullit::get_data(futures_data_TS)

## ----`futures market`, message = FALSE, warning = FALSE-----------------------
futures_market <- market_factor(data = futures_data_TS)
futures_market

## ----`futures momentum`, message = FALSE, warning = FALSE---------------------
ranking_period = 1L
futures_momentum <- momentum_factor(data = futures_data_TS, ranking_period = ranking_period)
futures_momentum

## ----`futures CHP bbg`, message = FALSE, warning = FALSE, echo = T, eval = F----
#  futures_data_CFTC <- pull_futures_CFTC(source = "Bloomberg", tickers_futures, start, end, verbose = FALSE)
#  
#  ranking_period = 1L
#  futures_CHP <- CHP_factor(
#    price_data = futures_data_TS, CHP_data = futures_data_CFTC, ranking_period = ranking_period
#    )
#  futures_CHP

## ----`futures CHP storethat`, message = FALSE, warning = FALSE, echo = F, eval = T----
futures_data_CFTC <- pull_futures_CFTC(
  source = "storethat", tickers_futures, start, end, verbose = FALSE, file = path
  )

ranking_period = 1L
futures_CHP <- CHP_factor(
  price_data = futures_data_TS, CHP_data = futures_data_CFTC, ranking_period = ranking_period
  )
futures_CHP

## ----`futures OI nearby`, message = FALSE, warning = FALSE--------------------
ranking_period = 1L
futures_OI_nearby <- OI_nearby_factor(data = futures_data_TS, ranking_period = ranking_period)
futures_OI_nearby

## ----`futures OI aggregate bbg`, message = FALSE, warning = FALSE, echo = T, eval = F----
#  futures_data_agg <- pull_futures_market(source = "Bloomberg", type = "aggregate", tickers_futures,
#                                          start, end, verbose = FALSE)
#  
#  ranking_period = 1L
#  futures_OI_aggregate <- OI_aggregate_factor(price_data = futures_data_market,
#                                              aggregate_data = futures_data_agg,
#                                              ranking_period = ranking_period)

## ----`futures OI aggregate storethat`, message = FALSE, warning = FALSE, echo = F, eval = T----
futures_data_agg <- pull_futures_market(
  source = "storethat", type = "aggregate", tickers_futures, start, end, 
  verbose = FALSE, file = path
  )

ranking_period = 1L
futures_OI_aggregate <- OI_aggregate_factor(
  price_data = futures_data_TS, aggregate_data = futures_data_agg, ranking_period = ranking_period
  )
futures_OI_aggregate

## ----`futures TS`, message = FALSE, warning = FALSE---------------------------
ranking_period = 1L
futures_TS <- TS_factor(data = futures_data_TS, ranking_period = ranking_period)
futures_TS

## ----`factor name`------------------------------------------------------------
get_name(factor)

## ----`factor positions`-------------------------------------------------------
get_positions(factor)

## ----`factor returns`---------------------------------------------------------
get_returns(factor)

## ----`factor data`------------------------------------------------------------
factorem::get_data(factor)

## ----`factor params`----------------------------------------------------------
get_parameters(factor)

## ----`factor call`------------------------------------------------------------
factorem::get_call(factor)

## ----`factor summary`---------------------------------------------------------
summary(factor)

## ----`plot performance`, fig.width = 7.5, fig.height = 5.5, fig.fullwidth = TRUE, message = FALSE, warning = FALSE----
library(plotit)

plot(factor, type = "performance")

## ----`plot positions`, fig.width = 7.5, fig.height = 6.5, fig.fullwidth = TRUE----
plot(factor, type = "positions")

