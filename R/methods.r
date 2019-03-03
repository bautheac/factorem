# show ####
#' Show method for \href{https://bautheac.github.io/factorem/}{\pkg{factorem}} S4 objects.
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
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
      paste0(rep("    ", ncol(object@parameters)),
             gsub(pattern = "_", replacement = " ", names(object@parameters)),
             sapply(names(object@parameters), function(x) if (nchar(x) <= 10L) ":\t\t " else ":\t "),
             object@parameters[1L, ],
             rep("\n", ncol(object@parameters))),
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
                   ranking_period = 6L, long_threshold = 0.5, short_threshold = 0.5){

            utils::data(list = c("tickers_cftc"), package = "BBGsymbols", envir = environment())

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold)

            tickers <- dplyr::select(price_data@term_structure_tickers, `active contract ticker`, ticker, position = `TS position`)

            price_data <- dplyr::filter(price_data@data, field == "PX_LAST") %>% dplyr::left_join(tickers, by = "ticker") %>%
              dplyr::filter(position == 1L) %>% dplyr::select(ticker = `active contract ticker`, field, date, value)

            CHP_data <- dplyr::left_join(CHP_data@data, dplyr::select(tickers_cftc, format, underlying, unit, participant, position, ticker),
                                         by = "ticker") %>%
              dplyr::filter(format == "legacy", underlying == "futures only", unit == "contracts",
                            participant == "commercial", position %in% c("long", "short")) %>%
              dplyr::select(ticker = `active contract ticker`, position, date, value) %>%
              dplyr::mutate(value = abs(value)) %>%
              tidyr::spread(position, value) %>% dplyr::mutate(`inverse CHP` = (long + short) / long) %>%
              dplyr::select(ticker, date, `inverse CHP`) %>% tidyr::gather(field, value, -c(ticker, date))

            if(! all(unique(CHP_data$ticker) %in% unique(price_data$ticker))){
              names <- unique(CHP_data$ticker)[!which((unique(CHP_data$ticker) %in% unique(price_data$ticker)))]
              stop(paste0("No price data for ", paste(names, collapse = ", "), "."))
            }

            data <- data.table::rbindlist(list(price_data, CHP_data), use.names = T)

            data <- factorem(name = "CHP", data = data, update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "inverse CHP", sort_levels = T, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold)

            methods::new("CHPFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, parameters = data@parameters, call = match.call())
          }
)




## futures nearby open interest growth (OI) factor ####

#' @rdname OI_nearby_factor-methods
#' @aliases OI_nearby_factor,AssetPricingFactor
#'
#' @export
setMethod("OI_nearby_factor",
          signature(data = "FuturesTS"),
          function(data, update_frequency = "month", return_frequency = "day", ranking_period = 1L,
                   long_threshold = 0.5, short_threshold = 0.5){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold)

            tickers <- dplyr::select(data@term_structure_tickers, `active contract ticker`, ticker, position = `TS position`)

            data <- dplyr::left_join(data@data, tickers, by = "ticker") %>% dplyr::filter(position == 1L) %>%
              dplyr::select(ticker = `active contract ticker`, field, date, value)

            OI <- dplyr::filter(data, field == "OPEN_INT") %>% dplyr::select(ticker, field, date, value)
            price <- dplyr::filter(data, field == "PX_LAST") %>% dplyr::select(ticker, field, date, value)

            data <- data.table::rbindlist(list(OI, price), use.names = T)

            data <- factorem(name = "nearby OI", data = data, update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "OPEN_INT", sort_levels = F, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold)

            methods::new("OIFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, parameters = data@parameters, call = match.call())
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
                   ranking_period = 1L, long_threshold = 0.5, short_threshold = 0.5){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold)

            tickers <- dplyr::select(price_data@term_structure_tickers, `active contract ticker`, ticker, position = `TS position`)

            price <- dplyr::left_join(price_data@data, tickers, by = "ticker") %>% dplyr::filter(position == 1L, field == "PX_LAST") %>%
              dplyr::select(ticker = `active contract ticker`, field, date, value)
            OI <- dplyr::filter(aggregate_data@data, field == "FUT_AGGTE_OPEN_INT") %>% dplyr::select(ticker, field, date, value)

            data <- data.table::rbindlist(list(OI, price), use.names = T)

            data <- factorem(name = "aggregate OI", data = data, update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "FUT_AGGTE_OPEN_INT", sort_levels = F, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold)

            methods::new("OIFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, parameters = data@parameters, call = match.call())
          }
)




## futures momentum factor ####

#' @rdname momentum_factor-methods
#' @aliases momentum_factor,AssetPricingFactor
#'
#' @export
setMethod("momentum_factor",
          signature(data = "FuturesTS"),
          function(data, update_frequency = "week", return_frequency = "day", ranking_period = 4L,
                   long_threshold = 0.5, short_threshold = 0.5){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold)

            tickers <- dplyr::select(data@term_structure_tickers, `active contract ticker`, ticker, position = `TS position`)

            price <- dplyr::left_join(data@data, tickers, by = "ticker") %>% dplyr::filter(position == 1L) %>%
              dplyr::select(ticker = `active contract ticker`, field, date, value)

            data <- factorem(name = "futures momentum", data = price, update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "PX_LAST", sort_levels = F, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold)

            methods::new("MomentumFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, parameters = data@parameters, call = match.call())
          }
)





## equity momentum factor ####

#' @rdname momentum_factor-methods
#' @aliases momentum_factor,AssetPricingFactor
#'
#' @export
setMethod("momentum_factor",
          signature(data = "EquityMarket"),
          function(data, update_frequency = "month", return_frequency = "day", ranking_period = 1L,
                   long_threshold = 0.5, short_threshold = 0.5){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold)

            price <- dplyr::filter(data@data, field == "PX_LAST") %>% dplyr::select(ticker, field, date, value)

            data <- factorem(name = "equity momentum", data = price, update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "PX_LAST", sort_levels = F, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold)

            methods::new("MomentumFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, parameters = data@parameters, call = match.call())
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
                   ranking_period = 1L, long_threshold = 0.5, short_threshold = 0.5){

            check_params(update_frequency = update_frequency, return_frequency = return_frequency,
                         ranking_period = ranking_period, long_threshold = long_threshold,
                         short_threshold = short_threshold, front = front, back = back,
                         positions = as.integer(unique(data@term_structure_tickers$`TS position`)))

            tickers <- dplyr::select(data@term_structure_tickers, `active contract ticker`, ticker, position = `TS position`)

            price <- dplyr::left_join(data@data, tickers, by = "ticker") %>% dplyr::filter(field == "PX_LAST", position == 1L) %>%
              dplyr::select(ticker = `active contract ticker`, field, date, value)
            carry <- dplyr::left_join(data@data, tickers, by = "ticker") %>% dplyr::filter(field == "PX_LAST", position %in% c(front, back)) %>%
              dplyr::select(ticker = `active contract ticker`, position, date, value) %>%
              dplyr::mutate(position = dplyr::case_when(position == front ~ "front", position == back ~ "back")) %>%
              dplyr::group_by(ticker) %>% tidyr::spread(position, value) %>% dplyr::mutate(carry = (back / front) - 1L) %>%
              dplyr::select(ticker, date, carry) %>% tidyr::gather(field, value, -c(ticker, date)) %>% dplyr::ungroup()

            data <- data.table::rbindlist(list(price, carry), use.names = T)

            data <- factorem(name = "futures term structure", data = data, update_frequency = update_frequency,
                             return_frequency = return_frequency, price_variable = "PX_LAST",
                             sort_variable = "carry", sort_levels = T, ranking_period = ranking_period,
                             long_threshold = long_threshold, short_threshold = short_threshold)

            methods::new("TSFactor", name = data@name, positions = data@positions, returns = data@returns,
                         data = data@data, parameters = data@parameters, call = match.call())
          }
)





## futures market factor ####

#' @rdname market_factor-methods
#' @aliases market_factor,AssetPricingFactor
#'
#' @export
setMethod("market_factor",
          signature(data = "FuturesTS"),
          function(data, return_frequency = "month", long = TRUE){

            check_params(return_frequency = return_frequency, long = long)

            tickers <- dplyr::select(data@term_structure_tickers, `active contract ticker`, ticker, position = `TS position`)

            temp <- dplyr::filter(data@data, field == "PX_LAST") %>% dplyr::left_join(tickers, by = "ticker") %>%
              dplyr::filter(position == 1L) %>% dplyr::select(date, ticker = `active contract ticker`, value)

            positions <- dplyr::group_by(temp, date) %>% dplyr::mutate(name = ticker, position = ifelse(long, "long", "short")) %>%
              dplyr::select(date, name, position) %>% dplyr::ungroup() %>% dplyr::arrange(date, position)

            temp <- dplyr::group_by(temp, ticker) %>% dplyr::mutate(value = (value / dplyr::lag(value, 1L) - 1L)) %>% dplyr::slice(2L:n()) %>%
              dplyr::ungroup() %>% dplyr::group_by(date) %>% dplyr::summarise(factor = mean(value, na.rm = T)) %>%
              dplyr::ungroup() %>% dplyr::mutate(factor = ifelse(long, factor, factor * -1L), year = lubridate::year(date))
            returns <- if (return_frequency == "day") { dplyr::mutate(temp, unit = lubridate::yday(date)) }
            else { dplyr::mutate(temp, unit = do.call(what = !! return_frequency, args = list(date))) }
            returns <- dplyr::group_by(returns, year, unit) %>% dplyr::summarise(factor = return_cumulative(factor)) %>%
              dplyr::ungroup()

            temp <- if (return_frequency == "day") { dplyr::mutate(temp, unit = lubridate::yday(date)) }
            else { dplyr::mutate(temp, unit = do.call(what = !! return_frequency, args = list(date))) }
            temp <- dplyr::select(temp, -factor) %>% dplyr::group_by(year, unit) %>% dplyr::filter(dplyr::row_number() == n())
            returns <- dplyr::left_join(returns, temp, by = c("year", "unit")) %>% dplyr::select(date, factor)

            args <- as.list(match.call())[-1L]

            methods::new("MarketFactor", name = "futures nearby market", returns = data.table::as.data.table(returns),
                         positions = data.table::as.data.table(positions), data = data.table::as.data.table(data@data),
                         parameters = tibble::as_tibble(args[names(args) != "data"]), call = match.call())
          }
)




## equity market factor ####

#' @rdname market_factor-methods
#' @aliases market_factor,AssetPricingFactor
#'
#' @export
setMethod("market_factor",
          signature(data = "EquityMarket"),
          function(data, return_frequency = "month", long = T){

            check_params(return_frequency = return_frequency, long = long)

            positions <- dplyr::filter(data, field == "PX_LAST") %>% dplyr::group_by(date) %>%
              dplyr::mutate(name = ticker, position = ifelse(long, "long", "short")) %>%
              dplyr::select(date, name, position) %>% dplyr::ungroup() %>% dplyr::arrange(date, position)

            temp <- dplyr::filter(data, field == "PX_LAST") %>% dplyr::select(-field) %>% dplyr::group_by(ticker) %>%
              dplyr::mutate(value = (value / dplyr::lag(value, 1L) - 1L)) %>% dplyr::slice(2L:n()) %>%
              dplyr::ungroup() %>% dplyr::group_by(date) %>% dplyr::summarise(factor = mean(value, na.rm = T)) %>%
              dplyr::ungroup() %>% dplyr::mutate(factor = ifelse(long, factor, factor * -1L), year = lubridate::year(date))
            returns <- if (return_frequency == "day") { dplyr::mutate(temp, unit = lubridate::yday(date)) }
            else { dplyr::mutate(temp, unit = do.call(what = !! return_frequency, args = list(date))) }
            returns <- dplyr::group_by(returns, year, unit) %>% dplyr::summarise(factor = return_cumulative(factor)) %>%
              dplyr::ungroup()

            temp <- if (return_frequency == "day") { dplyr::mutate(temp, unit = lubridate::yday(date)) }
            else { dplyr::mutate(temp, unit = do.call(what = !! return_frequency, args = list(date))) }
            data <- dplyr::select(data, -factor) %>% dplyr::group_by(year, unit) %>% dplyr::filter(dplyr::row_number() == n())
            returns <- dplyr::left_join(returns, temp, by = c("year", "unit")) %>% dplyr::select(date, factor)

            args <- as.list(match.call())[-1L]

            methods::new("MarketFactor", name = "equity market", returns = data.table::as.data.table(returns),
                         positions = data.table::as.data.table(positions), data = data.table::as.data.table(data@data),
                         parameters = tibble::as_tibble(args[names(args) != "data"]), call = deparse(match.call()))
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

  months <- dplyr::select(object@returns, date, return = leg) %>% dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date, label = T)) %>%
    dplyr::group_by(year, month) %>% dplyr::summarise(return = round(return_cumulative(return), 5L)) %>% dplyr::ungroup() %>% dplyr::select(year, month, return) %>%
    tidyr::spread(month, return)
  years <- dplyr::select(object@returns, date, return = leg) %>% dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::group_by(year) %>% dplyr::summarise(total = round(return_cumulative(return), 5L)) %>% dplyr::ungroup()

  dplyr::left_join(months, years, by = "year") %>% as.data.frame()
})



