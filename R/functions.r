#' Get time series of long and short factor positions
#'
#'
#' @param data a dataframe/tibble. Columns must include \code{name}, \code{date},
#'   \code{field} and \code{value}.
#'
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency.
#'   Must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'month'.
#'
#' @param sort_variable a scalar character vector. Specifies the name of the variable to use
#'   for sorting. Must be found in the \code{field} columns of \code{data}.
#'
#' @param sort_levels a scalar logical vector. If \code{TRUE}, sort is done on
#'   \code{sort_variable}'s levels, else on relative changes. Default: \code{FALSE}.
#'
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of
#'   \code{update_frequency} looking backward for averaging \code{sort_variable}.
#'
#' @param short_threshold a scalar numeric vector. Specifies the threshold for short
#'   positions. Default: 0.5.
#'
#' @param long_threshold a scalar numeric vector. Specifies the threshold for long
#'   positions. Default: 0.5.
#'
#'
#' @return A \code{\link[data.table]{data.table}} with factor positions by rebalancing
#'   date. Columns include \code{date}, \code{name} and \code{position}.
#'
#'
#' @importFrom lubridate month quarter semester week
#' @importFrom magrittr "%<>%"
factor_positions <- function(data, update_frequency, sort_variable, sort_levels, ranking_period,
                             long_threshold, short_threshold){

  data %<>%
    dplyr::filter(field == !! sort_variable) %>%
    dplyr::mutate(period = if(update_frequency == "year") paste0(lubridate::year(date))
           else if(update_frequency == "day") paste(lubridate::year(date), lubridate::yday(date), sep = ".")
           else paste(lubridate::year(date), do.call(what = update_frequency, args = list(date)), sep = "."))

  positions <- dplyr::select(data, name = ticker, period, field, value) %>%
    dplyr::group_by(name, period) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(value = if(sort_levels) value else (value/dplyr::lag(value, 1L)) - 1L) %>%
    tidyr::nest() %>%
    dplyr::mutate(average = purrr::pmap(list(data, ranking_period), function(x, y) x %>%
                            dplyr::mutate(average = RcppRoll::roll_meanr(x %>% dplyr::select(value) %>% purrr::flatten_dbl(), n = y + 1L, na.rm = TRUE)) %>%
                            dplyr::select(period, average))) %>%
    tidyr::unnest(average)

  dplyr::group_by(positions, period) %>%
    tidyr::nest() %>%
    dplyr::mutate(positions = purrr::pmap(list(data, long_threshold, short_threshold), function(x, y, z) {
      x %<>% dplyr::filter(stats::complete.cases(.))
      long <- tibble::tibble(name = dplyr::arrange(x, dplyr::desc(average)) %>%
                       dplyr::slice(1L:(floor((nrow(x) * y)))) %>%
                       dplyr::select(name) %>%
                       purrr::flatten_chr()) %>%
        dplyr::mutate(position = "long")
      short <- tibble::tibble(name = dplyr::arrange(x, average) %>%
                        dplyr::slice(1L:(floor((nrow(x) * z)))) %>%
                        dplyr::select(name) %>%
                        purrr::flatten_chr()) %>%
        dplyr::mutate(position = "short")
      rbind(long, short)
    })) %>%
    tidyr::unnest(positions, .drop = TRUE) %>%
    data.table::as.data.table()
}

#' Get return time series for factor as well as for long and short legs independently.
#'
#'
#' @param data a dataframe/tibble. Columns must include \code{name},
#'   \code{date}, \code{field} and \code{value}.
#'
#' @param positions  a dataframe/tibble. Returned by function
#'   \code{factor_positions}.
#'
#' @param update_frequency a scalar character vector. Specifies the
#'   rebalancing frequency. Must be one of 'year', 'semester',
#'   'quarter', 'month', 'week' or 'day'. Defaults to 'month'.
#'
#' @param return_frequency a scalar character vector. Specifies the
#'   frequency of the returns output. Must be one of 'year', 'semester',
#'   'quarter', 'month', 'week' or 'day'. Defaults to 'day'.
#'
#' @param price_variable a scalar character vector. Specifies the name
#'   of the variable hosting asset prices. Must be found in the
#'   \code{field} columns of \code{data}.
#'
#' @param geometric a scalar logical vector. If \code{TRUE} geometric
#'   returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#'
#' @return A \code{\link[data.table]{data.table}} with factor returns by
#'   date and factor leg. Columns include \code{date}, \code{name} and
#'   \code{position}.
#'
#'
#' @importFrom lubridate month quarter semester week
#' @importFrom magrittr "%<>%"
factor_returns <- function(data, positions, update_frequency, return_frequency, price_variable, geometric){

  data %<>%
    dplyr::filter(field == !! price_variable) %>%
    dplyr::mutate(period = if(update_frequency == "year") paste0(lubridate::year(date))
           else if(update_frequency == "day") paste(lubridate::year(date), lubridate::yday(date), sep = ".")
           else paste(lubridate::year(date), do.call(what = update_frequency, args = list(date)), sep = ".")) %>%
    dplyr::select(name = ticker, date, period, value) %>%
    dplyr::group_by(name) %>% dplyr::mutate(return = value/dplyr::lag(value, 1L) - 1L ) %>%
    dplyr::ungroup() %>% dplyr::select(name, date, period, return)

  positions %<>%
    dplyr::left_join(dplyr::distinct(positions, period) %>% dplyr::mutate(new = dplyr::lead(period, 1L)),
                     by = "period") %>%
    dplyr::select(period = new, name, position) %>% dplyr::filter(stats::complete.cases(period))

  long <- dplyr::distinct(data, date, period) %>%
    dplyr::left_join(positions %>% dplyr::filter(position == "long") %>% dplyr::select(name, period),
                     by = "period") %>%
    dplyr::select(-period) %>% dplyr::filter(stats::complete.cases(name)) %>%
    dplyr::left_join (data %>% dplyr::select(name, date, return), by = c("name", "date")) %>%
    dplyr::group_by(date) %>% dplyr::summarise(long = mean(return, na.rm = TRUE))

  short <- dplyr::distinct(data, date, period) %>%
    dplyr::left_join(positions %>% dplyr::filter(position == "short") %>% dplyr::select(name, period),
                     by = "period") %>%
    dplyr::select(-period) %>% dplyr::filter(stats::complete.cases(name)) %>%
    dplyr::left_join (data %>% dplyr::select(name, date, return), by = c("name", "date")) %>%
    dplyr::group_by(date) %>% dplyr::summarise(short = -1L * mean(return, na.rm = TRUE))

  dplyr::full_join(long, short, by = "date") %>%
    dplyr::mutate(factor = apply(dplyr::select(., long, short),
                                 function(x) mean(x, na.rm = TRUE), MARGIN = 1L)) %>%
    dplyr::mutate(period = if(return_frequency == "year") paste0(lubridate::year(date))
           else if(return_frequency == "day") paste(lubridate::year(date),
                                                    lubridate::yday(date), sep = ".")
           else paste(lubridate::year(date), get(paste0(return_frequency))(date), sep = ".")) %>%
    dplyr::group_by(period) %>% tidyr::nest() %>%
    dplyr::mutate(returns = purrr::map(data, function(x) {
      tibble::tibble(date = x$date[nrow(x)],
                     leg = c("long", "short", "factor"),
                     return = apply(dplyr::select(x, long, short, factor),
                                    function(y) PerformanceAnalytics::Return.cumulative(y, geometric = geometric),
                                    MARGIN = 2L)) %>% tidyr::spread(leg, return)
      })) %>%
    tidyr::unnest(returns, .drop = TRUE) %>% dplyr::select(date, long, short, factor) %>%
    data.table::as.data.table()
}


#' Construct an asset pricing factor
#'
#'
#' @description Given a reference dataset and a set of parameters, construct the desired asset
#'   pricing factor and returns its return and position time series by leg.
#'
#'
#' @param name a scalar character vector specifying the name to use for the factor.
#'
#' @param data a dataframe/tibble. Columns must include \code{name}, \code{date},
#'   \code{field} and \code{value}.
#'
#' @param update_frequency a scalar character vector. Specifies the rebalancing
#'   frequency. Must be one of 'year', 'semester', 'quarter', 'month', 'week' or
#'   'day'. Defaults to 'month'.
#'
#' @param return_frequency a scalar character vector. Specifies the frequency of
#'   the returns output. Must be one of 'year', 'semester', 'quarter', 'month',
#'   'week' or 'day'. Defaults to 'day'.
#'
#' @param price_variable a scalar character vector. Specifies the name of the variable
#'   hosting asset prices. Must be found in the \code{field} columns of \code{data}.
#'
#' @param sort_variable a scalar character vector. Specifies the name of the variable
#'   to use for sorting. Must be found in the \code{field} columns of \code{data}.
#'
#' @param sort_levels a scalar logical vector. If \code{TRUE}, sort is done on
#'   \code{sort_variable}'s levels, else on relative changes. Default: \code{FALSE}.
#'
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are
#'   returned, else arithmetic. Default: \code{TRUE}.
#'
#' @param ranking_period a scalar integer vector. Specifies number of periods in term
#'   of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#'
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short
#'   positions. Default: 0.5.
#'
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long
#'   positions. Default: 0.5.
#'
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}}.
#'
#'
#' @export
factorem <- function(name = "", data, update_frequency = "month", return_frequency = "day",
                     price_variable = "PX_LAST", sort_variable = "PX_LAST", sort_levels = FALSE,
                     geometric = TRUE, ranking_period = 0L, long_threshold = 0.5, short_threshold = 0.5){

  check_params(name = name, data = data, update_frequency = update_frequency,
               return_frequency = return_frequency, price_variable = price_variable,
               price_variables = unique(data$field), sort_variable = sort_variable,
               sort_variables = unique(data$field), sort_levels = sort_levels,
               geometric = geometric, ranking_period = ranking_period,
               long_threshold = long_threshold, short_threshold = short_threshold)

  positions <- factor_positions(data = data, update_frequency = update_frequency,
                                sort_variable = sort_variable, sort_levels = sort_levels,
                                ranking_period = ranking_period,
                                long_threshold = long_threshold, short_threshold = short_threshold)

  returns <- factor_returns(data = data, positions = positions, price_variable = price_variable,
                            update_frequency = update_frequency, return_frequency = return_frequency,
                            geometric = geometric)

  params <- list(`update frequency` = update_frequency, `return frequency` = return_frequency,
                 `price variable` = price_variable, `sort variable` = sort_variable,
                 `sort levels` = sort_levels, geometric = geometric, `ranking period` = ranking_period,
                 `long threshold` = long_threshold, `short threshold` = short_threshold) %>%
    purrr::flatten_dfc() %>%
    tibble::as.tibble()

  methods::new("AssetPricingFactor", name = name, positions = positions, returns = returns,
               data = data.table::as.data.table(data), parameters = params, call = match.call())
}
