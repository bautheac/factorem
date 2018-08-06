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



distinct(`test price data`, field)

`test price data` <- `TS data` %>%
  tidyr::gather(field, value, -c(active_contract_ticker, ticker, TS_position, date)) %>%
  tibble::as.tibble()
load(file = "data-raw/COT_data.rda")
`CHP data` <- COT_data.df
remove(COT_data.df)
`test COT data` <- `CHP data` %>%
  tibble::as.tibble()

CHP <- CHP_factor(price_data = `test price data`,
                  CHP_data = `test COT data`,
                  update_frequency = "week",
                  return_frequency = "week",
                  geometric = TRUE,
                  ranking_period = 13L,
                  long_threshold = 0.25,
                  short_threshold = 0.25)

plot_performance(CHP)
plot_positions(CHP)

?CHP_factor
?factorem



load(file = "data-raw/aggregate_data.rda")
`test aggregate data` <- aggregate_data.df
remove(aggregate_data.df)

`test aggregate data` %<>%
  tibble::as.tibble() %>%
  gather(field, value, -c(date, active_contract_ticker)) %>%
  select(active_contract_ticker, date, field, value)


`test data` <- bind_rows(`test price data`, `test aggregate data`)

temp <- `test data` %>%
  filter(field %in% c("PX_LAST", "FUT_AGGTE_OPEN_INT"))

OI_factor <- OI_factor(data = `test price data`,
                       update_frequency = "week",
                       return_frequency = "week",
                       ranking_period = 0L,
                       long_threshold = 0.5,
                       short_threshold = 0.5,
                       aggregate = FALSE,
                       geometric = TRUE)

plot_performance(OI_factor)
plot_positions(OI_factor)
?OI_factor


momentum_factor <- momentum_factor(data = `test price data`,
                                   update_frequency = "week",
                                   return_frequency = "week",
                                   sort_levels = FALSE,
                                   ranking_period = 0L,
                                   long_threshold = 0.5,
                                   short_threshold = 0.5,
                                   geometric = TRUE)






devtools::use_build_ignore(c("development", "data-raw"))
