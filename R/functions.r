if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "inverse CHP", "active contract ticker", "average", "date", "field", "leg", "long", "leg", "name",
                                                        "participant", "period", "PX_LAST", "returns", "roll yield", "short", "TS position", "underlying", "unit", "value"))


#' Get time series of long and short factor positions
#'
#' @param data a dataframe/tibble. Columns must include \code{name}, \code{date}, \code{field} and \code{value}.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "month".
#' @param sort_variable a scalar character vector. Specifies the name of the variable to use for sorting. Must be found in the \code{field} columns of \code{data}.
#' @param sort_levels a scalar logical vector. If \code{TRUE}, sort is done on 'sort_variable's levels, else on relative changes. Default: \code{FALSE}.
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#'
#' @return A tibble with columns \code{date}, \code{name} and \code{position}.
#'
#' @importFrom lubridate month quarter semester week
#' @importFrom magrittr "%>%" "%<>%"
factor_positions <- function(data,
                             update_frequency = "month",
                             sort_variable = "PX_LAST",
                             sort_levels = FALSE,
                             ranking_period = 0L,
                             long_threshold = 0.5,
                             short_threshold = 0.5){

  if (! is.data.frame(data)) stop("Parameter 'data' must be supplied as a dataframe.")
  if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector.")
  if (! rlang::is_scalar_character(sort_variable)) stop("Parameter 'sort_variable' must be supplied as a scalar character vector.")
  if (! rlang::is_scalar_logical(sort_levels)) stop("Parameter 'sort_levels' must be supplied as a scalar logical vector (TRUE or FALSE).")
  if (! rlang::is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector.")
  if (! rlang::is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector.")
  if (! rlang::is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector.")

  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(c("name", "date", "field", "value") %in% names(data))) stop("The columns of the dataframe supplied in 'data' must include 'name', 'date', 'field' and 'value'.")
  if (! sort_variable %in% (dplyr::distinct(data, field) %>% purrr::flatten_chr())) stop("The dataframe supplied in 'data' doesn't contain the sorting variable specified in 'sort_variable'.")

  data %<>%
    dplyr::filter(field == !! sort_variable) %>%
    dplyr::mutate(period = if(update_frequency == "year") paste0(lubridate::year(date))
           else if(update_frequency == "day") paste(lubridate::year(date), lubridate::yday(date), sep = ".")
           else paste(lubridate::year(date), get(paste0(update_frequency))(date), sep = "."))

  positions <- data %>%
    dplyr::select(name, period, field, value) %>%
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

  positions %>%
    dplyr::group_by(period) %>%
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
    tidyr::unnest(positions, .drop = TRUE)
}

#' Get return time series for factor as well as for long and short legs independently.
#'
#' @param data a dataframe/tibble. Columns must include \code{name}, \code{date}, \code{field} and \code{value}.
#' @param positions  a dataframe/tibble. Returned by function \code{factor_positions}.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'month'.
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'day'.
#' @param price_variable a scalar character vector. Specifies the name of the variable hosting asset prices. Must be found in the \code{field} columns of \code{data}.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @return A tibble with columns \code{date}, \code{long} , \code{short} and \code{factor}.
#'
#' @importFrom lubridate month quarter semester week
#' @importFrom magrittr "%>%" "%<>%"
factor_returns <- function(data,
                           positions,
                           update_frequency = "month",
                           return_frequency = "day",
                           price_variable = "PX_LAST",
                           geometric = TRUE){

  if (! is.data.frame(data)) stop("Parameter 'data' must be supplied as a dataframe.")
  if (! is.data.frame(positions)) stop("Parameter 'positions' must be supplied as a dataframe.")
  if (! rlang::is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector.")
  if (! rlang::is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector.")
  if (! rlang::is_scalar_character(price_variable)) stop("Parameter 'price_variable' must be supplied as a scalar character vector.")
  if (! rlang::is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE).")

  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(c("name", "date", "field", "value") %in% names(data))) stop("The columns of the dataframe supplied in 'data' must include 'name', 'date', 'field' and 'value'.")
  if (! price_variable %in% (dplyr::distinct(data, field) %>% purrr::flatten_chr())) stop("The dataframe supplied in 'data' doesn't contain the price variable specified in 'price_variable'.")

  data %<>%
    dplyr::filter(field == !! price_variable) %>%
    dplyr::mutate(period = if(update_frequency == "year") paste0(lubridate::year(date))
           else if(update_frequency == "day") paste(lubridate::year(date), lubridate::yday(date), sep = ".")
           else paste(lubridate::year(date), get(paste0(update_frequency))(date), sep = ".")) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(return = value/dplyr::lag(value, 1L) - 1L ) %>%
    dplyr::ungroup() %>%
    dplyr::select(name, date, period, return)

  positions %<>%
    dplyr::left_join (dplyr::distinct(positions, period) %>% dplyr::mutate(new = dplyr::lead(period, 1L)), by = "period") %>%
    dplyr::select(period = new, name, position) %>%
    dplyr::filter(stats::complete.cases(period))

  long <- dplyr::distinct(data, date, period) %>%
    dplyr::left_join (positions %>% dplyr::filter(position == "long") %>% dplyr::select(name, period), by = "period") %>%
    dplyr::select(-period) %>%
    dplyr::filter(stats::complete.cases(name)) %>%
    dplyr::left_join (data %>% dplyr::select(name, date, return), by = c("name", "date")) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(long = mean(return, na.rm = TRUE))
  short <- dplyr::distinct(data, date, period) %>%
    dplyr::left_join (positions %>% dplyr::filter(position == "short") %>% dplyr::select(name, period), by = "period") %>%
    dplyr::select(-period) %>%
    dplyr::filter(stats::complete.cases(name)) %>%
    dplyr::left_join (data %>% dplyr::select(name, date, return), by = c("name", "date")) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(short = -1L * mean(return, na.rm = TRUE))

  dplyr::full_join(long, short, by = "date") %>%
    dplyr::mutate(factor = apply(dplyr::select(., long, short), function(x) mean(x, na.rm = TRUE), MARGIN = 1L)) %>%
    dplyr::mutate(period = if(return_frequency == "year") paste0(lubridate::year(date))
           else if(return_frequency == "day") paste(lubridate::year(date), lubridate::yday(date), sep = ".")
           else paste(lubridate::year(date), get(paste0(return_frequency))(date), sep = ".")) %>%
    dplyr::group_by(period) %>%
    tidyr::nest() %>%
    dplyr::mutate(returns = purrr::map(data, function(x) {
      tibble::tibble(date = x$date[nrow(x)], leg = c("long", "short", "factor"), return = apply(dplyr::select(x, long, short, factor), function(y) PerformanceAnalytics::Return.cumulative(y, geometric = geometric), MARGIN = 2L)) %>%
        tidyr::spread(leg, return)
      })) %>%
    tidyr::unnest(returns, .drop = TRUE) %>%
    dplyr::select(date, long, short, factor)
}


#' Construct asset pricing factor
#'
#' @description Given a reference dataset and a set of parameters.
#'
#' @param name a scalar character vector specifying the name to use for the factor.
#' @param data a dataframe/tibble. Columns must include \code{name}, \code{date}, \code{field} and \code{value}.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency.
#'   Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "month".
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output.
#'   Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param price_variable a scalar character vector. Specifies the name of the variable hosting asset prices.
#'   Must be found in the \code{field} columns of \code{data}.
#' @param sort_variable a scalar character vector. Specifies the name of the variable to use for sorting.
#'   Must be found in the \code{field} columns of \code{data}.
#' @param sort_levels a scalar logical vector. If \code{TRUE}, sort is done on \code{sort_variable}'s levels, else on relative changes.
#'   Default: \code{FALSE}.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency}
#'   looking backward for averaging \code{sort_variable}.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector specifying the name to use for the factor.}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long} , \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset used for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @export

factorem <- function(name = "",
                     data,
                     update_frequency = "month",
                     return_frequency = "day",
                     price_variable = "PX_LAST",
                     sort_variable = "PX_LAST",
                     sort_levels = FALSE,
                     geometric = TRUE,
                     ranking_period = 0L,
                     long_threshold = 0.5,
                     short_threshold = 0.5){

  if (! rlang::is_scalar_character(name)) stop("Parameter 'name' must be supplied as a scalar character vector.")

  positions <- factor_positions(data = data, update_frequency = update_frequency, sort_variable = sort_variable, ranking_period = ranking_period,
                                long_threshold = long_threshold, short_threshold = short_threshold)
  returns <- factor_returns(data = data, positions = positions, price_variable = price_variable, update_frequency = update_frequency,
                            return_frequency =return_frequency)

  params <- list(`update frequency` = update_frequency, `return frequency` = return_frequency, `price variable` = price_variable,
                 `sort variable` = sort_variable, `sort levels` = sort_levels, geometric = geometric, `ranking period` = ranking_period,
                 `long threshold` = long_threshold, `short threshold` = short_threshold) %>%
    purrr::flatten_dfc()

  methods::new("AssetPricingFactor", name = name, positions = positions, returns = returns, data = data, params = params)
}


