#' Show method for S4 object of class `AssetPricingFactor`.
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector specifying the name to use for the factor.}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long} , \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset used for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom methods show
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


#' Accessor method for factor name
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A unit character vector containing the name of the factor.
#'
#' @export
setMethod("get_name", "AssetPricingFactor", function(factor) factor@name)


#' Accessor method for factor returns
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A tibble of factor returns.
#'
#' @export
setMethod("get_returns", "AssetPricingFactor", function(factor) factor@returns)


#' Accessor method for factor positions
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A tibble of factor positions.
#'
#' @export
setMethod("get_positions", "AssetPricingFactor", function(factor) factor@positions)

#' Accessor method for factor data construction
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A tibble containing the original dataset used for factor construction.
#'
#' @export
setMethod("get_data_original", "AssetPricingFactor", function(factor) factor@data)


#' Accessor method for factor parameters
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A tibble containing the original parameters supplied for factor construction.
#'
#' @export
setMethod("get_parameters", "AssetPricingFactor", function(factor) factor@params)

#' Accessor method for call to factor constructor function
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A scalar character vector showing the original call to the factor constructor function.
#'
#' @export
setMethod("get_call", "AssetPricingFactor", function(factor) factor@call)


#' Summary plot of factor performance by leg
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A line plot showing the wealth index for the factor itself as well as that of both long and short legs separately.
#'
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export
setMethod("plot_performance", "AssetPricingFactor", function(factor) {
  data <- factor@returns
  data[, tidyselect::matches("long|short|factor", vars = names(data))] <- data[, tidyselect::matches("long|short|factor", vars = names(data))] + 1L
  data %<>%
    dplyr::mutate_at(dplyr::vars(tidyselect::matches("long|short|factor")), dplyr::funs(c(1L, sapply(2L:NROW(.), function(x) .[x] * .[x - 1L]))))%>%
    tidyr::gather(leg, `wealth index`, -date) %>%
    dplyr::mutate(leg = dplyr::if_else(leg %in% c("long", "short"), paste(leg, "leg", sep = " "), paste(factor@name, leg, sep = " ")),
           leg = factor(leg, levels = c(paste(factor@name, "factor", sep = " "), "long leg", "short leg")))

  dygraphs::dygraph(data %>% dplyr::filter(leg == paste(factor@name, "factor", sep = " ")) %>% dplyr::select(date, `wealth index`),
                   main = paste(factor@name, "factor", sep = " "),
                   group = "factor")
  dygraphs::dygraph(data %>% dplyr::filter(leg == "long leg") %>% dplyr::select(date, `wealth index`),
                   main = "long leg",
                   group = "factor")
  dygraphs::dygraph(data %>% dplyr::filter(leg == "short leg") %>% dplyr::select(date, `wealth index`),
                   main = "short leg",
                   group = "factor")

  # ggplot2::ggplot(data, ggplot2::aes(x = date, y = `wealth index`, colour = leg)) +
  #   ggplot2::geom_line(size = 1L) +
  #   ggplot2::geom_line(ggplot2::aes(y = 1L), colour = "black", show.legend = FALSE) +
  #   ggthemes::theme_tufte() +
  #   ggplot2::theme(legend.position = "none")+
  #   ggplot2::labs(x = NULL, y = NULL) +
  #   ggplot2::facet_wrap(~leg, ncol = 1L)
})


#' Summary plot of factor positions by leg
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A bar plot showing the proportion of time individual names appear in the factor.
#'
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export
setMethod("plot_positions", "AssetPricingFactor", function(factor) {
  data <- factor@positions
  data %<>%
    dplyr::group_by(position) %>%
    tidyr::nest() %>%
    dplyr::mutate(proportion = purrr::map(data, function(x) x %>%
                              dplyr::group_by(name) %>%
                              dplyr::tally() %>%
                              dplyr::mutate(n = n / nrow(x))
    )) %>%
    tidyr::unnest(proportion) %>%
    rbind(data %>%
            dplyr::group_by(name) %>%
            dplyr::tally() %>%
            dplyr::mutate(position = "factor", n = n / nrow(factor@positions))) %>%
    dplyr::rename(proportion = n) %>%
    dplyr::mutate(leg = dplyr::if_else(position %in% c("long", "short"), paste(position, "leg", sep = " "), paste(factor@name, position, sep = " ")),
           leg = factor(leg, levels = c(paste(factor@name, "factor", sep = " "), "long leg", "short leg")))

  ggplot2::ggplot(data, ggplot2::aes(name, proportion, fill = name)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_discrete(breaks = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent()) +
    ggplot2::facet_wrap(~leg, ncol = 1L) +
    ggthemes::theme_tufte() +
    ggplot2::theme(legend.title = ggplot2::element_blank())

})


#' Summary of factor returns by month and year
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#' @param leg A character vector. 'long', 'short' and 'factor' display the returns for the long, short and the factor itself respectively.
#'
#' @return A dataframe with months as colums and years as rows. Column total shows the cumulated return for the corresponding year.
#'
#' @importFrom magrittr "%>%" "%<>%" set_colnames
#'
#' @export
setMethod("summary", "AssetPricingFactor", function(object, leg = "factor") {
  returns <- xts::xts(x = object@returns %>%
                        dplyr::select(returns = !! leg),
                      order.by = object@returns$date)

  PerformanceAnalytics::table.CalendarReturns(returns, digits = 1, as.perc = TRUE, geometric = object@params$geometric) %>%
    dplyr::rename(total = returns) %>%
    magrittr::set_colnames(value = tolower(names(.)))

})






#' Construct CHP factor
#'
#' @param price_data an S4 object of class \code{\linkS4class{FuturesTS}}. \code{\linkS4class{FuturesTS}} objects are returned by the
#'   \code{\link[pullit]{bbg_futures_TS}} function in the \code{pullit} package.
#' @param CHP_data an S4 object of class \code{\linkS4class{FuturesCFTC}}. \code{\linkS4class{FuturesCFTC}} objects are returned by the
#'   \code{\link[pullit]{bbg_futures_CFTC}} function in the \code{pullit} package.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', 'semester', 'quarter', 'month' or 'week'. Defaults to 'month'.
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for CHP average calculation.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @importClassesFrom pullit FuturesTS FuturesCFTC
#'
#' @export
setMethod("CHP_factor",
          signature(price_data = "FuturesTS", CHP_data = "FuturesCFTC"),
          function(price_data,
                   CHP_data,
                   update_frequency = "month",
                   return_frequency = "day",
                   ranking_period = 0L,
                   long_threshold = 0.5,
                   short_threshold = 0.5,
                   geometric = TRUE){

            data(list = c("tickers_cftc"), package = "bbgsymbols", envir = environment())

            if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L))
              stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1")

            price_data@data %<>%
              dplyr::mutate(position = stringr::str_extract(ticker, pattern = "(?<=^.{0,10})\\d(?=\\s[A-Z]:)")) %>%
              dplyr::filter(position == 1L, field == "PX_LAST") %>%
              dplyr::select(name = `active contract ticker`, date, field, value)

            CHP_data@data %<>%
              dplyr::left_join(tickers_cftc, by = "ticker") %>%
              dplyr::filter(format == "legacy", underlying == "futures only", unit == "# positions", participant == "commercial",
                     position %in% c("long", "short")) %>%
              dplyr::select(name = `active contract ticker`, position, date, value) %>%
              dplyr::mutate(value = abs(value)) %>%
              tidyr::spread(position, value) %>%
              dplyr::mutate(`inverse CHP` = (long + short) / long) %>%
              dplyr::select(name, date, `inverse CHP`) %>%
              tidyr::gather(field, value, -c(name, date))

            if(! all(purrr::flatten_chr(dplyr::distinct(CHP_data, name)) %in% purrr::flatten_chr(dplyr::distinct(price_data, name)))){
              names <- purrr::flatten_chr(dplyr::distinct(CHP_data, name))[!which((purrr::flatten_chr(dplyr::distinct(CHP_data, name)) %in% purrr::flatten_chr(dplyr::distinct(price_data, name))))]
              stop(paste0("No price data for ", paste(names, collapse = ", "), "."))
            }

            data <- factorem(name = "CHP",
                             data = rbind(price_data, CHP_data),
                             update_frequency = update_frequency,
                             return_frequency = return_frequency,
                             price_variable = "PX_LAST",
                             sort_variable = "inverse CHP",
                             sort_levels = TRUE,
                             ranking_period = ranking_period,
                             long_threshold = long_threshold,
                             short_threshold = short_threshold,
                             geometric = geometric)

            methods::new("CHPFactor", name = data@name, positions = data@positions, returns = data@returns, params = data@params, call = match.call())
          }
)




## futures nearby open interest growth (OI) factor ####

#' Construct futures open interest growth (OI) nearby factor
#'
#' @param data an S4 object of class \code{\linkS4class{FuturesTS}}. \code{\linkS4class{FuturesTS}} objects are returned by the
#'   \code{\link[pullit]{bbg_futures_TS}} function from the \code{pullit} package.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @importFrom magrittr "%>%"
#'
#' @importClassesFrom pullit FuturesTS
#'
#' @export
setMethod("OI_nearby_factor",
          signature(data = "FuturesTS"),
          function(data,
                   update_frequency = "month",
                   return_frequency = "day",
                   ranking_period = 0L,
                   long_threshold = 0.5,
                   short_threshold = 0.5,
                   geometric = TRUE){

            if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L))
              stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1")

            data <- dplyr::mutate(data@data, position = stringr::str_extract(ticker, pattern = "(?<=^.{0,10})\\d(?=\\s[A-Z]:)"))

            OI <- data %>%
              dplyr::filter(position == 1L, field == "OPEN_INT") %>%
              dplyr::select(name = `active contract ticker`, date, field, value)

            price <- data %>%
              dplyr::filter(position == 1L, field == "PX_LAST") %>%
              dplyr::select(name = `active contract ticker`, date, field, value)

            data <- factorem(name = "nearby OI",
                             data = rbind(price, OI),
                             update_frequency = update_frequency,
                             return_frequency = return_frequency,
                             price_variable = "PX_LAST",
                             sort_variable = "OPEN_INT",
                             sort_levels = FALSE,
                             ranking_period = ranking_period,
                             long_threshold = long_threshold,
                             short_threshold = short_threshold,
                             geometric = geometric)

            methods::new("OIFactor", name = data@name, positions = data@positions, returns = data@returns, params = data@params, call = deparse(match.call()))

          }
)



## futures aggregate open interest growth (OI) factor ####

#' Construct futures open interest growth (OI) nearby factor
#'
#' @param price_data an S4 object of class \code{\linkS4class{FuturesTS}}. \code{\linkS4class{FuturesTS}} objects are returned by the
#'   \code{\link[pullit]{bbg_futures_TS}} function from the \code{pullit} package.
#' @param aggregate_data an S4 object of class code{\linkS4class{FuturesAggregate}}. \code{\linkS4class{FuturesAggregate}} objects are
#'   returned by the \code{\link[pullit]{bbg_futures_aggregate}} function from the \code{pullit} package.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @importFrom magrittr "%>%"
#' @importClassesFrom pullit FuturesTS FuturesAggregate
#'
#' @export
setMethod("OI_aggregate_factor",
          signature(price_data = "FuturesTS", aggregate_data = "FuturesAggregate"),
          function(price_data,
                   aggregate_data,
                   update_frequency = "month",
                   return_frequency = "day",
                   ranking_period = 0L,
                   long_threshold = 0.5,
                   short_threshold = 0.5,
                   geometric = TRUE){

            if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L))
              stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1")

            OI <- dplyr::filter(aggregate_data@data, field == "FUT_AGGTE_OPEN_INT") %>%
              dplyr::select(name = `active contract ticker`, date, field, value)

            price <- dplyr::filter(price_data@data, field == "PX_LAST") %>%
              dplyr::select(name = `active contract ticker`, date, field, value)


            data <- factorem(name = "aggregate OI",
                             data = rbind(price, OI),
                             update_frequency = update_frequency,
                             return_frequency = return_frequency,
                             price_variable = "PX_LAST",
                             sort_variable = "FUT_AGGTE_OPEN_INT",
                             sort_levels = FALSE,
                             ranking_period = ranking_period,
                             long_threshold = long_threshold,
                             short_threshold = short_threshold,
                             geometric = geometric)

            methods::new("OIFactor", name = data@name, positions = data@positions, returns = data@returns, params = data@params, call = deparse(match.call()))
          }
)







## futures momentum factor ####

#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @importFrom magrittr "%>%"
#'
#' @importClassesFrom pullit FuturesTS
#'
#' @describeIn momentum_factor futures momentum factor.
#'
#' @export
setMethod("momentum_factor",
          signature(data = "FuturesTS"),
          function(data,
                   update_frequency = "month",
                   return_frequency = "day",
                   ranking_period = 0L,
                   long_threshold = 0.5,
                   short_threshold = 0.5,
                   geometric = TRUE){

            if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L))
              stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1")

            data <- dplyr::mutate(data@data, position = stringr::str_extract(ticker, pattern = "(?<=^.{0,10})\\d(?=\\s[A-Z]:)")) %>%
              dplyr::filter(field == "PX_LAST", position == 1L) %>%
              dplyr::select(name = `active contract ticker`, date, field, value)

            data <- factorem(name = "futures momentum",
                             data = data,
                             update_frequency = update_frequency,
                             return_frequency = return_frequency,
                             price_variable = "PX_LAST",
                             sort_variable = "PX_LAST",
                             sort_levels = FALSE,
                             ranking_period = ranking_period,
                             long_threshold = long_threshold,
                             short_threshold = short_threshold,
                             geometric = geometric)

            methods::new("MomentumFactor", name = data@name, positions = data@positions, returns = data@returns, params = data@params, call = deparse(match.call()))
          }
)





## equity momentum factor ####

#' @importFrom magrittr "%>%"
#'
#' @importClassesFrom pullit EquityMarket
#'
#' @describeIn momentum_factor equity momentum factor.
#'
#' @export
setMethod("momentum_factor",
          signature(data = "EquityMarket"),
          function(data,
                   update_frequency = "month",
                   return_frequency = "day",
                   ranking_period = 0L,
                   long_threshold = 0.5,
                   short_threshold = 0.5,
                   geometric = TRUE){

            if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L))
              stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1")

            data <- dplyr::filter(data@data, field == "PX_LAST") %>%
              dplyr::select(name = ticker, date, field, value)

            data <- factorem(name = "equity momentum",
                             data = data,
                             update_frequency = update_frequency,
                             return_frequency = return_frequency,
                             price_variable = "PX_LAST",
                             sort_variable = "PX_LAST",
                             sort_levels = FALSE,
                             ranking_period = ranking_period,
                             long_threshold = long_threshold,
                             short_threshold = short_threshold,
                             geometric = geometric)

            methods::new("MomentumFactor", name = data@name, positions = data@positions, returns = data@returns, params = data@params, call = deparse(match.call()))
          }
)







## futures term structure factor ####

#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param front a scalar integer vector. Specifies the term structure position to use as front contract in roll yield (sort variable) calculation. Defaults to 1.
#' @param back a scalar integer vector. Specifies the term structure position to use as back contract in roll yield (sort variable) calculation. Defaults to 2.
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @importFrom magrittr "%>%"
#'
#' @importClassesFrom pullit FuturesTS
#'
#' @describeIn TS_factor futures term structure factor.
#'
#' @export
setMethod("TS_factor",
          signature(data = "FuturesTS"),
          function(data,
                   update_frequency = "month",
                   return_frequency = "day",
                   ranking_period = 0L,
                   long_threshold = 0.5,
                   short_threshold = 0.5,
                   geometric = TRUE){

            if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_integer(front)) stop("Parameter 'front' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_integer(back)) stop("Parameter 'back' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! all(c(front, back) %in% (dplyr::distinct(data, `TS position`) %>% purrr::flatten_int())))
              stop("The dataframe supplied in 'data' doesn't contain data for the term structure positions supplied in the `front` and `back` parameters")
            if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (front > back) stop("The front contract must come before the back contract on the term strucutre (front < back)")
            if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1")

            data <- dplyr::mutate(data@data, position = stringr::str_extract(ticker, pattern = "(?<=^.{0,10})\\d(?=\\s[A-Z]:)")) %>%
              dplyr::filter(field == "PX_LAST", position %in% c(front, back)) %>%
              dplyr::select(name = `active contract ticker`, date, field, position, value) %>%
              tidyr::spread(field, value) %>%
              tidyr::spread(position, PX_LAST) %>%
              dplyr::rename(front = paste0(!! front), back = paste0(!! back)) %>%
              dplyr::filter(stats::complete.cases(front, back)) %>%
              dplyr::mutate(`roll yield` = log(front/back)) %>%
              dplyr::select(name, date, PX_LAST = front, `roll yield`) %>%
              tidyr::gather(field, value, -c(name, date))

            data <- factorem(name = "futures term structure",
                             data = data,
                             update_frequency = update_frequency,
                             return_frequency = return_frequency,
                             price_variable = "PX_LAST",
                             sort_variable = "roll yield",
                             sort_levels = FALSE,
                             ranking_period = ranking_period,
                             long_threshold = long_threshold,
                             short_threshold = short_threshold,
                             geometric = geometric)

            methods::new("TSFactor", name = data@name, positions = data@positions, returns = data@returns, params = data@params, call = deparse(match.call()))
          }
)





## futures market factor ####

#' @importFrom magrittr "%>%"
#'
#' @importClassesFrom pullit FuturesTS
#'
#' @describeIn market_factor futures nearby market factor.
#'
#' @export
setMethod("market_factor",
          signature(data = "FuturesTS"),
          function(data,
                   return_frequency = "day",
                   long = TRUE,
                   geometric = TRUE){

            if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_integer(front)) stop("Parameter 'front' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_integer(back)) stop("Parameter 'back' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! all(c(front, back) %in% (dplyr::distinct(data, `TS position`) %>% purrr::flatten_int())))
              stop("The dataframe supplied in 'data' doesn't contain data for the term structure positions supplied in the `front` and `back` parameters")
            if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (front > back) stop("The front contract must come before the back contract on the term strucutre (front < back)")
            if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1")

            if (! is.data.frame(data)) stop("Parameter 'data' must be supplied as a dataframe")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_logical(long)) stop("Parameter 'long' must be supplied as a scalar logical vector (TRUE or FALSE)")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! all(c("active contract ticker", "date", "field", "value") %in% names(data)))
              stop("The columns of the dataframe supplied in 'data' must include 'active contract ticker', 'date', 'TS position', 'field' and 'value'")
            if (! "PX_LAST" %in% (dplyr::distinct(data, field) %>% purrr::flatten_chr()))
              stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")

            data <- dplyr::mutate(data@data, `TS position` = stringr::str_extract(ticker, pattern = "(?<=^.{0,10})\\d(?=\\s[A-Z]:)"))

            positions <- dplyr::filter(data, field == "PX_LAST", `TS position` == 1L) %>%
              dplyr::group_by(date) %>%
              tidyr::nest() %>%
              dplyr::mutate(positions = purrr::map(data, function(x) dplyr::select(x, `active contract ticker`, value) %>%
                                                     dplyr::filter(stats::complete.cases(.)) %>%
                                                     dplyr::select(name = `active contract ticker`))) %>%
              tidyr::unnest(positions) %>%
              dplyr::mutate(position = dplyr::if_else(long, "long", "short"))

            returns <- dplyr::filter(data, field == "PX_LAST", `TS position` == 1L) %>%
              dplyr::group_by(date) %>%
              dplyr::summarise(factor = mean(value, na.rm = TRUE)) %>%
              dplyr::filter(!is.na(factor)) %>%
              dplyr::mutate(factor = factor / dplyr::lag(factor, 1L) - 1L,
                            period = if(return_frequency == "year") paste0(lubridate::year(date))
                            else if(return_frequency == "day") paste(lubridate::year(date), lubridate::yday(date), sep = ".")
                            else paste(lubridate::year(date), get(paste0(return_frequency))(date), sep = ".")) %>%
              dplyr::group_by(period) %>%
              tidyr::nest() %>%
              dplyr::mutate(returns = purrr::map(data, function(x) {
                tibble::tibble(date = x$date[nrow(x)], leg = "factor", return = apply(dplyr::select(x, factor), function(y) PerformanceAnalytics::Return.cumulative(y, geometric = geometric), MARGIN = 2L)) %>%
                  tidyr::spread(leg, return)
              })) %>%
              tidyr::unnest(returns, .drop = TRUE) %>%
              dplyr::select(date, factor) %>%
              dplyr::mutate(factor = if (long) factor else factor * -1L)

            args <- as.list(match.call())[-1L]

            methods::new("MarketFactor", name = "futures nearby market", positions = positions, returns = returns, params = args[names(args) != "data"] %>%
                           purrr::flatten_dfc(), call = deparse(match.call()))
          }
)




## equity market factor ####

#' @importFrom magrittr "%>%"
#'
#' @importClassesFrom pullit EquityMarket
#'
#' @describeIn market_factor equity market factor.
#'
#' @export
setMethod("market_factor",
          signature(data = "EquityMarket"),
          function(data,
                   return_frequency = "day",
                   long = TRUE,
                   geometric = TRUE){

            if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_integer(front)) stop("Parameter 'front' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_integer(back)) stop("Parameter 'back' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector")
            if (! rlang::is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! all(c(front, back) %in% (dplyr::distinct(data, `TS position`) %>% purrr::flatten_int())))
              stop("The dataframe supplied in 'data' doesn't contain data for the term structure positions supplied in the `front` and `back` parameters")
            if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")
            if (front > back) stop("The front contract must come before the back contract on the term strucutre (front < back)")
            if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1")

            if (! is.data.frame(data)) stop("Parameter 'data' must be supplied as a dataframe")
            if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector")
            if (! rlang::is_scalar_logical(long)) stop("Parameter 'long' must be supplied as a scalar logical vector (TRUE or FALSE)")
            if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE)")

            if (! all(c("active contract ticker", "date", "field", "value") %in% names(data)))
              stop("The columns of the dataframe supplied in 'data' must include 'active contract ticker', 'date', 'TS position', 'field' and 'value'")
            if (! "PX_LAST" %in% (dplyr::distinct(data, field) %>% purrr::flatten_chr()))
              stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column.")
            if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
              stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'")

            positions <- dplyr::filter(data, field == "PX_LAST") %>%
              dplyr::group_by(date) %>%
              tidyr::nest() %>%
              dplyr::mutate(positions = purrr::map(data, function(x) dplyr::select(x, ticker, value) %>%
                                                     dplyr::filter(stats::complete.cases(.)) %>%
                                                     dplyr::select(name = ticker))) %>%
              tidyr::unnest(positions) %>%
              dplyr::mutate(position = dplyr::if_else(long, "long", "short"))

            returns <- dplyr::filter(data, field == "PX_LAST") %>%
              dplyr::group_by(date) %>%
              dplyr::summarise(factor = mean(value, na.rm = TRUE)) %>%
              dplyr::filter(!is.na(factor)) %>%
              dplyr::mutate(factor = factor / dplyr::lag(factor, 1L) - 1L,
                            period = if(return_frequency == "year") paste0(lubridate::year(date))
                            else if(return_frequency == "day") paste(lubridate::year(date), lubridate::yday(date), sep = ".")
                            else paste(lubridate::year(date), get(paste0(return_frequency))(date), sep = ".")) %>%
              dplyr::group_by(period) %>%
              tidyr::nest() %>%
              dplyr::mutate(returns = purrr::map(data, function(x) {
                tibble::tibble(date = x$date[nrow(x)], leg = "factor", return = apply(dplyr::select(x, factor), function(y) PerformanceAnalytics::Return.cumulative(y, geometric = geometric), MARGIN = 2L)) %>%
                  tidyr::spread(leg, return)
              })) %>%
              tidyr::unnest(returns, .drop = TRUE) %>%
              dplyr::select(date, factor) %>%
              dplyr::mutate(factor = if (long) factor else factor * -1L)

            args <- as.list(match.call())[-1L]

            methods::new("MarketFactor", name = "equity market", positions = positions, returns = returns, params = args[names(args) != "data"] %>%
                           purrr::flatten_dfc(), call = deparse(match.call()))
          }
)
