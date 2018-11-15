# show ####
#' Show method for \href{https://bautheac.github.io/factorem/}{\pkg{factorem}} S4 objects.
#'
#' @rdname show-methods
#'
#' @aliases show,AssetPricingFactor
#'
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", signature(object = "AssetPricingFactor"), function(object) {
  cat(methods::is(object)[[1]], "\n",
      "  name: ", paste(object@name, "factor", " "), "\n",
      "  parameters\n",
      paste0(rep("    ", ncol(object@params)),
             gsub(pattern = "_", replacement = " ", names(object@params)),
             sapply(names(object@params), function(x) if (nchar(x) <= 10L) ":\t\t " else ":\t "),
             object@params[1L, ],
             rep("\n", ncol(object@params))),
      sep = ""
  )
})


# accessors ####

## name ####
#' @rdname get_name-methods
#'
#' @aliases get_name,AssetPricingFactor
#'
#'
#'
#' @export
setMethod("get_name", "AssetPricingFactor", function(object) object@name)

## data ####
#' @rdname get_data-methods
#'
#' @aliases get_data,AssetPricingFactor
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_data", "AssetPricingFactor", function(object) object@data)

## returns ####
#' @rdname get_returns-methods
#'
#' @aliases get_returns,AssetPricingFactor
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_returns", "AssetPricingFactor", function(object) object@returns)

## positions ####
#' @rdname get_positions-methods
#'
#' @aliases get_positions,AssetPricingFactor
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_positions", "AssetPricingFactor", function(object) object@positions)

## parameters ####
#' @rdname get_parameters-methods
#'
#' @aliases get_parameters,AssetPricingFactor
#'
#'
#' @importFrom tibble tibble
#'
#'
#' @export
setMethod("get_parameters", "AssetPricingFactor", function(object) object@parameters)

## call ####
#' @rdname get_call-methods
#'
#' @aliases get_call,AssetPricingFactor
#'
#'
#' @export
setMethod("get_call", "AssetPricingFactor", function(object) object@call)


# factors ####

## CHP ####

#' @rdname CHP_factor-methods
#' @aliases CHP_factor,AssetPricingFactor
#'
#' @export
setMethod("CHP_factor",
          signature(price_data = "FuturesTS", CHP_data = "FuturesCFTC"),
          function(price_data, CHP_data, update_frequency = "month", return_frequency = "day",
                   ranking_period = 6L, long_threshold = 0.5, short_threshold = 0.5,
                   geometric = TRUE){

            utils::data(list = c("tickers_CFTC"), package = "BBGsymbols", envir = environment())

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold, geometric = geometric)

            price_data <- dplyr::left_join(price_data@data,
                                           dplyr::select(price_data@tickers, `active contract ticker`,
                                                         ticker, `TS position`),
                                           by = "ticker") %>%
              dplyr::filter(`TS position` == 1L, field == "PX_LAST") %>%
              dplyr::select(name = `active contract ticker`, date, field, value) %>%
              data.table::as.data.table()

            CHP_data <- dplyr::left_join(CHP_data@data,
                                         CHP_data@fields,
                                         by = "ticker") %>%
              dplyr::filter(format == "legacy", underlying == "futures only", unit == "contracts",
                            participant == "commercial", position %in% c("long", "short")) %>%
              dplyr::select(name = `active contract ticker`, position, date, value) %>%
              dplyr::mutate(value = abs(value)) %>%
              tidyr::spread(position, value) %>% dplyr::mutate(`inverse CHP` = (long + short) / long) %>%
              dplyr::select(name, date, `inverse CHP`) %>% tidyr::gather(field, value, -c(name, date)) %>%
              data.table::as.data.table()

            if(! all(unique(CHP_data$name) %in% unique(price_data$name))){
              names <- unique(CHP_data$name)[!which((unique(CHP_data$name) %in% unique(price_data$name)))]
              stop(paste0("No price data for ", paste(names, collapse = ", "), "."))
            }

            data <- factorem(name = "CHP", data = rbind(price_data, CHP_data), update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "inverse CHP", sort_levels = TRUE, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold, geometric = geometric)

            methods::new("CHPFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, params = data@params, call = match.call())
          }
)




## futures nearby open interest growth (OI) factor ####

#' @rdname OI_nearby_factor-methods
#' @aliases OI_nearby_factor,AssetPricingFactor
#'
#' @export
setMethod("OI_nearby_factor",
          signature(data = "FuturesTS"),
          function(data, update_frequency = "month", return_frequency = "day", ranking_period = 0L,
                   long_threshold = 0.5, short_threshold = 0.5, geometric = TRUE){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold, geometric = geometric)

            data <- dplyr::left_join(data@data,
                                     dplyr::select(data@tickers, `active contract ticker`, ticker,
                                                   position = `TS position`),
                                     by = "ticker")

            OI <- dplyr::filter(data, position == 1L, field == "OPEN_INT") %>%
              dplyr::select(name = `active contract ticker`, date, field, value)

            price <- dplyr::filter(data, position == 1L, field == "PX_LAST") %>%
              dplyr::select(name = `active contract ticker`, date, field, value)

            data <- factorem(name = "nearby OI", data = rbind(price, OI), update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "OPEN_INT", sort_levels = FALSE, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold,
                             geometric = geometric)

            methods::new("OIFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, params = data@params, call = match.call())

          }
)



## futures aggregate open interest growth (OI) factor ####

#' @rdname OI_aggregate_factor-methods
#' @aliases OI_aggregate_factor,AssetPricingFactor
#'
#' @export
setMethod("OI_aggregate_factor",
          signature(price_data = "FuturesTS", aggregate_data = "FuturesAggregate"),
          function(price_data, aggregate_data, update_frequency = "month", return_frequency = "day",
                   ranking_period = 0L, long_threshold = 0.5, short_threshold = 0.5,
                   geometric = TRUE){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold, geometric = geometric)

            price <- dplyr::left_join(price_data@data,
                                      dplyr::select(price_data@tickers, `active contract ticker`,
                                                    ticker, position = `TS position`),
                                      by = "ticker") %>%
              dplyr::filter(position == 1L, field == "PX_LAST") %>%
              dplyr::select(name = `active contract ticker`, date, field, value)

            OI <- dplyr::filter(aggregate_data@data, field == "FUT_AGGTE_OPEN_INT") %>%
              dplyr::select(name = ticker, date, field, value)

            data <- factorem(name = "aggregate OI", data = rbind(price, OI), update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "FUT_AGGTE_OPEN_INT", sort_levels = FALSE, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold, geometric = geometric)

            methods::new("OIFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, params = data@params, call = match.call())
          }
)




## futures momentum factor ####

#' @rdname momentum_factor-methods
#' @aliases momentum_factor,AssetPricingFactor
#'
#' @export
setMethod("momentum_factor",
          signature(data = "FuturesTS"),
          function(data, update_frequency = "month", return_frequency = "day", ranking_period = 0L,
                   long_threshold = 0.5, short_threshold = 0.5, geometric = 0.5){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold, geometric = geometric)

            price <- dplyr::left_join(data@data, dplyr::select(data@tickers, `active contract ticker`,
                                                               ticker, position = `TS position`),
                                      by = "ticker") %>%
              dplyr::filter(position == 1L, field == "PX_LAST") %>%
              dplyr::select(name = `active contract ticker`, date, field, value)

            data <- factorem(name = "futures momentum", data = price, update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "PX_LAST", sort_levels = FALSE, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold,
                             geometric = geometric)

            methods::new("MomentumFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, params = data@params, call = match.call())
          }
)





## equity momentum factor ####

#' @rdname momentum_factor-methods
#' @aliases momentum_factor,AssetPricingFactor
#'
#' @export
setMethod("momentum_factor",
          signature(data = "EquityMarket"),
          function(data, update_frequency = "month", return_frequency = "day", ranking_period = 0L,
                   long_threshold = 0.5, short_threshold = 0.5, geometric = 0.5){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold, geometric = geometric)

            data <- dplyr::filter(data@data, field == "PX_LAST") %>%
              dplyr::select(name = ticker, date, field, value)

            data <- factorem(name = "equity momentum", data = data, update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "PX_LAST", sort_levels = FALSE,
                             ranking_period = ranking_period, long_threshold = long_threshold,
                             short_threshold = short_threshold, geometric = geometric)

            methods::new("MomentumFactor", name = data@name, positions = data@positions, returns = data@returns,
                         params = data@params, call = deparse(match.call()))
          }
)







## futures term structure factor ####

#' @rdname TS_factor-methods
#' @aliases TS_factor,AssetPricingFactor
#'
#' @export
setMethod("TS_factor",
          signature(data = "FuturesTS"),
          function(data, update_frequency = "month", return_frequency = "day", front = 1L, back = 2L,
                   ranking_period = 0L, long_threshold = 0.5, short_threshold = 0.5, geometric = TRUE){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold, geometric = geometric, front = front,
                         back = back, positions = as.integer(unique(data@tickers$`TS position`)))

            data <- dplyr::left_join(data@data,
                                     dplyr::select(data@tickers, `active contract ticker`, ticker,
                                                   position = `TS position`),
                                     by = "ticker") %>%
              dplyr::filter(field == "PX_LAST", position %in% c(front, back)) %>%
              dplyr::select(name = `active contract ticker`, date, field, position, value) %>%
              tidyr::spread(field, value) %>% tidyr::spread(position, PX_LAST) %>%
              dplyr::rename(front = paste0(!! front), back = paste0(!! back)) %>%
              dplyr::filter(stats::complete.cases(front, back)) %>%
              dplyr::mutate(`roll yield` = log(front/back)) %>%
              dplyr::select(name, date, PX_LAST = front, `roll yield`) %>%
              tidyr::gather(field, value, -c(name, date))

            data <- factorem(name = "futures term structure", data = data, update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "roll yield", sort_levels = FALSE, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold,
                             geometric = geometric)

            methods::new("TSFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, params = data@params, call = match.call())
          }
)





## futures market factor ####

#' @rdname market_factor-methods
#' @aliases market_factor,AssetPricingFactor
#'
#' @export
setMethod("market_factor",
          signature(data = "FuturesTS"),
          function(data, return_frequency = "month", long = TRUE, geometric = TRUE){

            check_params(return_frequency = return_frequency, geometric = geometric,
                         long = long)

            data <- dplyr::left_join(data@data,
                                     dplyr::select(data@tickers, `active contract ticker`,
                                                   ticker, position = `TS position`),
                                     by = "ticker") %>%
              dplyr::filter(field == "PX_LAST", position == 1L) %>%
              dplyr::select(`active contract ticker`, date, return = value) %>%
              dplyr::group_by(`active contract ticker`) %>%
              dplyr::mutate(return = (return / dplyr::lag(return, 1L) - 1L)) %>%
              dplyr::ungroup()

            positions <- dplyr::group_by(data, date) %>% tidyr::nest() %>%
              dplyr::mutate(positions = purrr::map(data,
                                                   function(x) dplyr::select(x, name = `active contract ticker`) %>%
                                                     dplyr::filter(stats::complete.cases(.)))) %>%
              tidyr::unnest(positions) %>% dplyr::mutate(position = dplyr::if_else(long, "long", "short"))

            returns <- dplyr::group_by(data, date) %>% dplyr::summarise(factor = mean(return, na.rm = TRUE)) %>%
              dplyr::filter(!is.na(factor)) %>%
              dplyr::mutate(period = if(return_frequency == "year") paste0(lubridate::year(date))
                            else if(return_frequency == "day") paste(lubridate::year(date), lubridate::yday(date), sep = ".")
                            else paste(lubridate::year(date), get(paste0(return_frequency))(date), sep = ".")) %>%
              dplyr::group_by(period) %>% tidyr::nest() %>%
              dplyr::mutate(returns = purrr::map(data, function(x) {
                tibble::tibble(date = x$date[nrow(x)],
                               leg = "factor",
                               return = apply(dplyr::select(x, factor),
                                              function(y) PerformanceAnalytics::Return.cumulative(y, geometric = geometric),
                                              MARGIN = 2L)) %>%
                  tidyr::spread(leg, return)
              })) %>%
              tidyr::unnest(returns, .drop = TRUE) %>% dplyr::select(date, factor) %>%
              dplyr::mutate(factor = if (long) factor else factor * -1L)

            args <- as.list(match.call())[-1L]

            methods::new("MarketFactor", name = "futures nearby market", positions = data.table::as.data.table(positions),
                         returns = data.table::as.data.table(returns), data = data.table::as.data.table(data),
                         params = tibble::as.tibble(args[names(args) != "data"]), call = match.call())
          }
)




## equity market factor ####

#' @rdname market_factor-methods
#' @aliases market_factor,AssetPricingFactor
#'
#' @export
setMethod("market_factor",
          signature(data = "EquityMarket"),
          function(data, return_frequency = "month", long = TRUE, geometric = TRUE){

            check_params(return_frequency = return_frequency, geometric = geometric,
                         long = long)

            positions <- dplyr::filter(data, field == "PX_LAST") %>% dplyr::group_by(date) %>%
              tidyr::nest() %>% dplyr::mutate(positions = purrr::map(data, function(x) dplyr::select(x, ticker, value) %>%
                                                     dplyr::filter(stats::complete.cases(.)) %>%
                                                     dplyr::select(name = ticker))) %>%
              tidyr::unnest(positions) %>% dplyr::mutate(position = dplyr::if_else(long, "long", "short"))

            returns <- dplyr::filter(data, field == "PX_LAST") %>% dplyr::group_by(date) %>%
              dplyr::summarise(factor = mean(value, na.rm = TRUE)) %>% dplyr::filter(!is.na(factor)) %>%
              dplyr::mutate(factor = factor / dplyr::lag(factor, 1L) - 1L,
                            period = if(return_frequency == "year") paste0(lubridate::year(date))
                            else if(return_frequency == "day") paste(lubridate::year(date), lubridate::yday(date), sep = ".")
                            else paste(lubridate::year(date), get(paste0(return_frequency))(date), sep = ".")) %>%
              dplyr::group_by(period) %>% tidyr::nest() %>%
              dplyr::mutate(returns = purrr::map(data, function(x) {
                tibble::tibble(date = x$date[nrow(x)],
                               leg = "factor",
                               return = apply(dplyr::select(x, factor),
                                              function(y) PerformanceAnalytics::Return.cumulative(y, geometric = geometric),
                                              MARGIN = 2L)) %>%
                  tidyr::spread(leg, return)
              })) %>%
              tidyr::unnest(returns, .drop = TRUE) %>% dplyr::select(date, factor) %>%
              dplyr::mutate(factor = if (long) factor else factor * -1L)

            args <- as.list(match.call())[-1L]

            methods::new("MarketFactor", name = "equity market", positions = positions, returns = returns,
                         params = args[names(args) != "data"] %>% purrr::flatten_dfc(), call = deparse(match.call()))
          }
)







# summary ####

#' @rdname summary-methods
#'
#'
#' @aliases summary,AssetPricingFactor
#'
#'
#' @export
setMethod("summary", "AssetPricingFactor", function(object, leg = "factor") {

  PerformanceAnalytics::table.CalendarReturns(xts::xts(dplyr::select(object@returns, returns = !! leg), order.by = object@returns$date),
                                              digits = 1L, as.perc = TRUE, geometric = object@params$geometric) %>%
    dplyr::rename(total = returns) %>% magrittr::set_names(tolower(names(.)))

})



