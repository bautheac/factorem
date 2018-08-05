library(tidyverse); library(magrittr); library(lubridate); library(RcppRoll); library(PerformanceAnalytics); library(ggplot2); library(ggthemes)
library(scales)
load(file = "data-raw/TS_data.rda")
`TS data` <- TS_data.df
remove(TS_data.df)

`test price data` <- `TS data` %>%
  tidyr::gather(field, value, -c(active_contract_ticker, ticker, TS_position, date)) %>%
  tibble::as.tibble() %>%
  dplyr::filter(TS_position == 1L) %>%
  dplyr::select(name = active_contract_ticker, date, field, value)



momentum <- factorem(name = "momentum",
                     data = `test price data`,
                     update_frequency = "week",
                     return_frequency = "week",
                     price_variable = "PX_LAST",
                     sort_variable = "PX_LAST",
                     sort_levels = FALSE,
                     geometric = TRUE,
                     ranking_period = 2L,
                     long_threshold = 0.25,
                     short_threshold = 0.25)

momentum@returns

devtools::document()
?factorem::plot_performance
plot_performance(momentum)
plot_positions(momentum)
summary(momentum, leg = "short")





`test price data` <- `TS data` %>%
  tidyr::gather(field, value, -c(active_contract_ticker, ticker, TS_position, date)) %>%
  tibble::as.tibble()
load(file = "data-raw/COT_data.rda")
`CHP data` <- COT_data.df
remove(COT_data.df)
`test COT data` <- `CHP data` %>%
  tibble::as.tibble() %>%
  filter(active_contract_ticker != "BOA Comdty")

CHP <- CHP_factor(price_data = `test price data`,
                  CHP_data = `test COT data`,
                  update_frequency = "week",
                  return_frequency = "week",
                  sort_levels = FALSE,
                  geometric = TRUE,
                  ranking_period = 13L,
                  long_threshold = 0.25,
                  short_threshold = 0.25)

plot_positions(CHP)

?CHP_factor
?factorem

devtools::use_build_ignore(c("development", "data-raw"))
