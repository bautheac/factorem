#' Get time series of long and short factor positions
#'
#' @param data A dataframe/tibble. Columns must include `name`, `date`, `field` and `value`.
#' @param update_frequency A scalar character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "month".
#' @param sort_variable A scalar character vector. Specifies the name of the variable to use for sorting. Must be found in the `field` columns of `data`.
#' @param sort_levels A scalar logical vector. If `TRUE`, sort is done on `sort_variable`'s levels, else on relative changes. Default: `FALSE`.
#' @param ranking_period A scalar integer vector. Specifies number of periods in term of `update_frequency` looking backward for averaging `sort_variable`
#' @param short_threshold A scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param long_threshold A scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#'
#' @return A tibble with columns `date`, `name` and `position`.
#'
#' @importFrom dplyr arrange desc distinct filter group_by mutate select slice row_number ungroup
#' @importFrom lubridate yday month quarter semester week year
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr flatten_dbl is_scalar_character is_scalar_double is_scalar_integer is_scalar_logical pmap
#' @importFrom RcppRoll roll_meanr
#' @importFrom tibble tibble
#' @importFrom tidyr nest spread unnest

factor_positions <- function(data,
                             update_frequency = "month",
                             sort_variable = "PX_LAST",
                             sort_levels = FALSE,
                             ranking_period = 0L,
                             long_threshold = 0.5,
                             short_threshold = 0.5){

  if (! is.data.frame(data)) stop("Parameter 'price_data' must be supplied as a dataframe.")
  if (! is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_character(sort_variable)) stop("Parameter 'sort_variable' must be supplied as a scalar character vector.")
  if (! is_scalar_logical(sort_levels)) stop("Parameter 'sort_levels' must be supplied as a scalar logical vector (TRUE or FALSE).")
  if (! is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector.")
  if (! is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector.")
  if (! is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector.")

  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(c("name", "date", "field", "value") %in% names(data))) stop("The columns of the dataframe supplied in 'data' must include 'name', 'date', 'field' and 'value'.")
  if (! sort_variable %in% (distinct(data, field) %>% flatten_chr())) stop("The dataframe supplied in 'data' doesn't contain the sorting variable specified in 'sort_variable'.")

  data %<>%
    filter(field == !! sort_variable) %>%
    mutate(period = if(update_frequency == "year") paste0(year(date))
           else if(update_frequency == "day") paste(year(date), yday(date), sep = ".")
           else paste(year(date), get(paste0(update_frequency))(date), sep = "."))

  positions <- data %>%
    select(name, period, field, value) %>%
    group_by(name, period) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    group_by(name) %>%
    mutate(value = if(sort_levels) value else (value/dplyr::lag(value, 1L)) - 1L) %>%
    nest() %>%
    mutate(average = pmap(list(data, ranking_period), function(x, y) x %>%
                            mutate(average = roll_meanr(x %>% select(value) %>% flatten_dbl(), n = y + 1L, na.rm = TRUE)) %>%
                            select(period, average))) %>%
    unnest(average)

  positions %>%
    group_by(period) %>%
    nest() %>%
    mutate(positions = pmap(list(data, long_threshold, short_threshold), function(x, y, z) {
      x %<>% filter(complete.cases(.))
      long <- tibble(name = arrange(x, desc(average)) %>%
                       slice(1L:(floor((nrow(x) * y)))) %>%
                       select(name) %>%
                       flatten_chr()) %>%
        mutate(position = "long")
      short <- tibble(name = arrange(x, average) %>%
                        slice(1L:(floor((nrow(x) * z)))) %>%
                        select(name) %>%
                        flatten_chr()) %>%
        mutate(position = "short")
      rbind(long, short)
    })) %>%
    unnest(positions, .drop = TRUE)
}

#' Get return time series for factor as well as for long and short legs independently.
#'
#' @param data A dataframe/tibble. Columns must include `name`, `date`, `field` and `value`.
#' @param update_frequency A scalar character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "month".
#' @param return_frequency A scalar character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param price_variable A scalar character vector. Specifies the name of the variable hosting asset prices. Must be found in the `field` columns of `data`.
#' @param geometric A scalar logical vector. If `TRUE` geometric returns are returned, else arithmetic. Default: `TRUE`.
#'
#' @return A tibble with columns `date`, `long` , `short` and `factor`.
#'
#' @importFrom dplyr distinct filter full_join group_by lag lead left_join mutate select summarise ungroup
#' @importFrom lubridate yday month quarter semester week year
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom PerformanceAnalytics Return.cumulative
#' @importFrom purrr flatten_chr is_scalar_character is_scalar_logical map
#' @importFrom tidyr nest unnest

factor_returns <- function(data,
                           positions,
                           update_frequency = "month",
                           return_frequency = "day",
                           price_variable = "PX_LAST",
                           geometric = TRUE){

  if (! is.data.frame(data)) stop("Parameter 'price_data' must be supplied as a dataframe.")
  if (! is.data.frame(positions)) stop("Parameter 'positions' must be supplied as a dataframe.")
  if (! is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_character(price_variable)) stop("Parameter 'price_variable' must be supplied as a scalar character vector.")
  if (! is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE).")

  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(c("name", "date", "field", "value") %in% names(data))) stop("The columns of the dataframe supplied in 'data' must include 'name', 'date', 'field' and 'value'.")
  if (! price_variable %in% (distinct(data, field) %>% flatten_chr())) stop("The dataframe supplied in 'data' doesn't contain the price variable specified in 'price_variable'.")

  data %<>%
    filter(field == !! price_variable) %>%
    mutate(period = if(update_frequency == "year") paste0(year(date))
           else if(update_frequency == "day") paste(year(date), yday(date), sep = ".")
           else paste(year(date), get(paste0(update_frequency))(date), sep = ".")) %>%
    group_by(name) %>%
    mutate(return = value/lag(value, 1L) - 1L ) %>%
    ungroup() %>%
    select(name, date, period, return)

  positions %<>%
    left_join(distinct(positions, period) %>% mutate(new = lead(period, 1L)), by = "period") %>%
    select(period = new, name, position) %>%
    filter(complete.cases(period))

  long <- distinct(data, date, period) %>%
    left_join(positions %>% filter(position == "long") %>% select(name, period), by = "period") %>%
    select(-period) %>%
    filter(complete.cases(name)) %>%
    left_join(data %>% select(name, date, return), by = c("name", "date")) %>%
    group_by(date) %>%
    summarise(long = mean(return, na.rm = TRUE))
  short <- distinct(data, date, period) %>%
    left_join(positions %>% filter(position == "short") %>% select(name, period), by = "period") %>%
    select(-period) %>%
    filter(complete.cases(name)) %>%
    left_join(data %>% select(name, date, return), by = c("name", "date")) %>%
    group_by(date) %>%
    summarise(short = -1L * mean(return, na.rm = TRUE))

  full_join(long, short, by = "date") %>%
    mutate(factor = apply(select(., long, short), function(x) mean(x, na.rm = TRUE), MARGIN = 1L)) %>%
    mutate(period = if(return_frequency == "year") paste0(year(date))
           else if(return_frequency == "day") paste(year(date), yday(date), sep = ".")
           else paste(year(date), get(paste0(return_frequency))(date), sep = ".")) %>%
    group_by(period) %>%
    nest() %>%
    mutate(returns = map(data, function(x) {
      tibble(date = x$date[nrow(x)], leg = c("long", "short", "factor"), return = apply(select(x, long, short, factor), function(y) Return.cumulative(y, geometric = geometric), MARGIN = 2L)) %>%
        spread(leg, return)
      })) %>%
    unnest(returns, .drop = TRUE) %>%
    select(date, long, short, factor)
}


#' Construct asset pricing factor
#'
#' @description Given a reference dataset and a set of parameters.
#'
#' @param name A scalar character vector specifying the name to use for the factor.
#' @param ... Factor construction parameters:
#'   \itemize{
#'     \item{data: a dataframe/tibble. Columns must include `name`, `date`, `field` and `value`.}
#'     \item{update_frequency: a scalar character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "month".}
#'     \item{return_frequency: a scalar character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "day".}
#'     \item{sort_variable: a scalar character vector. Specifies the name of the variable to use for sorting. Must be found in the `field` columns of `data`.}
#'     \item{price_variable: a scalar character vector. Specifies the name of the variable hosting asset prices. Must be found in the `field` columns of `data`.}
#'     \item{sort_levels: a scalar logical vector. If `TRUE`, sort is done on `sort_variable`'s levels, else on relative changes. Default: `FALSE`.}
#'     \item{ranking_period: a scalar integer vector. Specifies number of periods in term of `update_frequency` looking backward for averaging `sort_variable`.}
#'     \item{long_threshold: a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.}
#'     \item{short_threshold: a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.}
#'   }
#'
#' @return An S4 object of class \linkS4class{AssetPricingFactor} with slots:
#'   \itemize{
#'     \item{name: a scalar character vector specifying the name to use for the factor.}
#'     \item{returns: a tibble with columns `date`, `long` , `short` and `factor`.}
#'     \item{positions: a tibble with columns `date`, `name` and `position`.}
#'     \item{data: a tibble containing the original dataset used for factor construction.}
#'     \item{params: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr flatten_dfc
#' @importFrom magrittr "%>%"
#' @importFrom purrr is_scalar_character
#'
#' @export

factorem <- function(name = NA, ...){

  if (! is_scalar_character(name)) stop("Parameter 'name' must be supplied as a scalar character vector.")

  positions <- do.call("factor_positions", list(...)[c("data", "update_frequency", "sort_variable", "sort_levels", "ranking_period", "long_threshold", "short_threshold")])
  returns <- do.call("factor_returns", modifyList(x = list(...)[c("data", "price_variable", "update_frequency", "return_frequency")],
                                                  val = list(positions = positions)))

  .AssetPricingFactor(name = name,
                      positions = positions,
                      returns = returns,
                      data = list(...)[["data"]],
                      params = list(...)[names(list(...)) != "data"] %>% flatten_dfc()
                      )
}


#' Construct CHP factor
#'
#' @description Given a reference dataset and a set of parameters.
#'
#' @param price_data A dataframe/tibble. Columns must include `active_contract_ticker`, `date`, `TS_position`, `field` and `value`. `field` must contain `PX_LAST` variable.
#' @param CHP_data A dataframe/tibble. Columns must include:
#'   \itemize{
#'     \item{format: must contain `legacy`.}
#'     \item{underlying: must contain `futures only`.}
#'     \item{unit: must contain `# positions`.}
#'     \item{participant: must contain `commercial`.}
#'     \item{position: must contain `long` & `short`.}
#'     \item{PX_LAST: contains the corresponding values.}
#'   }
#' @param update_frequency A scalar character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency A scalar character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period A scalar integer vector. Specifies number of periods in term of `update_frequency` looking backward for CHP average calculation.
#' @param long_threshold A scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold A scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric A scalar logical vector. If `TRUE` geometric returns are returned, else arithmetic. Default: `TRUE`.
#'
#' @return An S4 object of class \linkS4class{AssetPricingFactor} with slots:
#'   \itemize{
#'     \item{name: a scalar character vector: "CHP factor".}
#'     \item{returns: a tibble with columns `date`, `long` , `short` and `factor`.}
#'     \item{positions: a tibble with columns `date`, `name` and `position`.}
#'     \item{data: a tibble containing the original dataset used for factor construction.}
#'     \item{params: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr filter flatten_chr flatten_dfc mutate select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom tidyr gather spread
#'
#' @export
CHP_factor <- function(price_data,
                       CHP_data,
                       update_frequency = "month",
                       return_frequency = "day",
                       ranking_period = 0L,
                       long_threshold = 0.5,
                       short_threshold = 0.5,
                       geometric = TRUE){

  if (! is.data.frame(price_data)) stop("Parameter 'price_data' must be supplied as a dataframe.")
  if (! is.data.frame(CHP_data)) stop("Parameter 'CHP_data' must be supplied as a dataframe.")
  if (! is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector.")
  if (! is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector.")
  if (! is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector.")
  if (! is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE).")

  if (! all(c("active_contract_ticker", "date", "TS_position", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active_contract_ticker', 'date', `TS_position`, 'field' and 'value'.")
  if (! "PX_LAST" %in% (distinct(data, field) %>% flatten_chr()))
    stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column.")
  if (! c("format", "underlying", "unit", "participant", "position", "PX_LAST") %in% all(names(CHP_data)))
    stop("The columns of the dataframe supplied in 'CHP_data' must include 'format', 'underlying', 'unit', ''participant', 'position' and 'PX_LAST'.")
  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1.")

  price_data %<>%
    filter(TS_position == 1L, field == "PX_LAST") %>%
    select(name = active_contract_ticker, date, field, value)

  CHP_data %<>%
    filter(format == "legacy", underlying == "futures only", unit == "# positions", participant == "commercial", position %in% c("long", "short")) %>%
    select(name = active_contract_ticker, position, date, PX_LAST) %>%
    mutate(PX_LAST = abs(PX_LAST)) %>%
    spread(position, PX_LAST) %>%
    mutate(`1 / CHP` = (long + short) / long) %>%
    select(name, date, `1 / CHP`) %>%
    gather(field, value, -c(name, date))

  if(! all(flatten_chr(distinct(CHP_data, name)) %in% flatten_chr(distinct(price_data, name)))){
    names <- flatten_chr(distinct(CHP_data, name))[!which((flatten_chr(distinct(CHP_data, name)) %in% flatten_chr(distinct(price_data, name))))]
    stop(paste0("No price data for ", paste(names, collapse = ", "), "."))
  }

  data <- rbind(price_data, CHP_data)

  factorem(name = "CHP",
           data = data,
           update_frequency = update_frequency,
           return_frequency = return_frequency,
           price_variable = "PX_LAST",
           sort_variable = "1 / CHP",
           sort_levels = TRUE,
           ranking_period = ranking_period,
           long_threshold = long_threshold,
           short_threshold = short_threshold,
           geometric = geometric)
}




#' Construct OI factor
#'
#' @description Given a reference dataset and a set of parameters.
#'
#' @param data A dataframe/tibble. Columns must include `active_contract_ticker`, `date`, `TS_position`, `field` and `value`. `field` must contain `PX_LAST` and `OPEN_INT` variables.
#' @param update_frequency A scalar character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency A scalar character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period A scalar integer vector. Specifies number of periods in term of `update_frequency` looking backward for averaging `sort_variable`.
#' @param long_threshold A scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold A scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param aggregate A scalar logical vector. If `TRUE` open interest is aggregated over the whole term structure supplied in `data`. Defaults to `FALSE`.
#' @param geometric A scalar logical vector. If `TRUE` geometric returns are returned, else arithmetic. Default: `TRUE`.
#'
#' @return An S4 object of class \linkS4class{AssetPricingFactor} with slots:
#'   \itemize{
#'     \item{name: a scalar character vector: "OI factor".}
#'     \item{returns: a tibble with columns `date`, `long` , `short` and `factor`.}
#'     \item{positions: a tibble with columns `date`, `name` and `position`.}
#'     \item{data: a tibble containing the original dataset used for factor construction.}
#'     \item{params: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr filter group_by select summarise
#' @importFrom magrittr "%>%"
#' @importFrom purrr flatten_chr is_scalar_character is_scalar_double is_scalar_double is_scalar_logical
#'
#' @export
OI_factor <- function(data,
                      update_frequency = "month",
                      return_frequency = "day",
                      ranking_period = 0L,
                      long_threshold = 0.5,
                      short_threshold = 0.5,
                      aggregate = FALSE,
                      geometric = TRUE){

  if (! is.data.frame(data)) stop("Parameter 'data' must be supplied as a dataframe.")
  if (! is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector.")
  if (! is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector.")
  if (! is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector.")
  if (! is_scalar_logical(aggregate)) stop("Parameter 'aggregate' must be supplied as a scalar logical vector (TRUE or FALSE).")
  if (! is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE).")

  if (! all(c("active_contract_ticker", "date", "TS_position", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active_contract_ticker', 'date', `TS_position`, 'field' and 'value'.")
  if (! all(c("PX_LAST", "OPEN_INT") %in% (distinct(data, field) %>% flatten_chr())))
    stop("The dataframe supplied in 'data' doesn't contain the required variables 'PX_LAST' amd 'OPEN_INT' in the `field` column.")
  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1.")

  OI <- data %>%
    filter(field == "OPEN_INT") %>%
    {
      if (aggregate) group_by(., active_contract_ticker, date, field) %>%
        summarise(value = sum(value)) %>%
        select(name = active_contract_ticker, everything())
      else
        filter(., TS_position == 1L) %>%
        select(name = active_contract_ticker, date, field, value)
    }

  price <- data %>%
    filter(TS_position == 1L, field == "PX_LAST") %>%
    select(name = active_contract_ticker, date, field, value)

  data <- rbind(price, OI)

  factorem(name = "OI",
           data = data,
           update_frequency = update_frequency,
           return_frequency = return_frequency,
           price_variable = "PX_LAST",
           sort_variable = "OPEN_INT",
           sort_levels = FALSE,
           ranking_period = ranking_period,
           long_threshold = long_threshold,
           short_threshold = short_threshold,
           geometric = geometric)
}





#' Construct momentum factor
#'
#' @description Given a reference dataset and a set of parameters.
#'
#' @param data A dataframe/tibble. Columns must include `active_contract_ticker`, `date`, `TS_position`, `field` and `value`.
#' @param update_frequency A scalar character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency A scalar character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period A scalar integer vector. Specifies number of periods in term of `update_frequency` looking backward for averaging `sort_variable`.
#' @param long_threshold A scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold A scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric A scalar logical vector. If `TRUE` geometric returns are returned, else arithmetic. Default: `TRUE`.
#'
#' @return An S4 object of class \linkS4class{AssetPricingFactor} with slots:
#'   \itemize{
#'     \item{name: a scalar character vector: "momentum factor".}
#'     \item{returns: a tibble with columns `date`, `long` , `short` and `factor`.}
#'     \item{positions: a tibble with columns `date`, `name` and `position`.}
#'     \item{data: a tibble containing the original dataset used for factor construction.}
#'     \item{params: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr is_scalar_character is_scalar_double is_scalar_integer is_scalar_logical
#'
#' @export
momentum_factor <- function(data,
                            update_frequency = "month",
                            return_frequency = "day",
                            ranking_period = 0L,
                            long_threshold = 0.5,
                            short_threshold = 0.5,
                            geometric = TRUE){

  if (! is.data.frame(data)) stop("Parameter 'data' must be supplied as a dataframe.")
  if (! is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector.")
  if (! is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector.")
  if (! is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector.")
  if (! is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE).")

  if (! all(c("active_contract_ticker", "date", "TS_position", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active_contract_ticker', 'date', `TS_position`, 'field' and 'value'.")
  if (! "PX_LAST" %in% (distinct(data, field) %>% flatten_chr()))
    stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column.")
  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1.")

  data %<>%
    filter(field == "PX_LAST", TS_position == 1L) %>%
    select(name = active_contract_ticker, date, field, value)

  factorem(name = "momentum",
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
}




#' Construct term structure factor
#'
#' @description Given a reference dataset and a set of parameters.
#'
#' @param data A dataframe/tibble. Columns must include `active_contract_ticker`, `date`, `TS_position`, `field` and `value`.
#' @param update_frequency A scalar character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency A scalar character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param front A scalar integer vector. Specifies the term structure position to use as front contract in roll yield (sort variable) calculation. Defaults to 1.
#' @param back A scalar integer vector. Specifies the term structure position to use as back contract in roll yield (sort variable) calculation. Defaults to 2.
#' @param ranking_period A scalar integer vector. Specifies number of periods in term of `update_frequency` looking backward for averaging `sort_variable`.
#' @param long_threshold A scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold A scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric A scalar logical vector. If `TRUE` geometric returns are returned, else arithmetic. Default: `TRUE`.
#'
#' @return An S4 object of class \linkS4class{AssetPricingFactor} with slots:
#'   \itemize{
#'     \item{name: a scalar character vector: "term structure factor".}
#'     \item{returns: a tibble with columns `date`, `long` , `short` and `factor`.}
#'     \item{positions: a tibble with columns `date`, `name` and `position`.}
#'     \item{data: a tibble containing the original dataset used for factor construction.}
#'     \item{params: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr filter mutate rename select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr flatten_int is_scalar_character is_scalar_double is_scalar_integer is_scalar_logical
#' @importFrom tidyr gather spread
#'
#' @export
TS_factor <- function(data,
                      update_frequency = "month",
                      return_frequency = "day",
                      front = 1L,
                      back = 2L,
                      ranking_period = 0L,
                      long_threshold = 0.5,
                      short_threshold = 0.5,
                      geometric = TRUE){

  if (! is.data.frame(data)) stop("Parameter 'data' must be supplied as a dataframe.")
  if (! is_scalar_character(update_frequency)) stop("Parameter 'update_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_integer(front)) stop("Parameter 'front' must be supplied as a scalar integeter vector.")
  if (! is_scalar_integer(back)) stop("Parameter 'back' must be supplied as a scalar integeter vector.")
  if (! is_scalar_integer(ranking_period)) stop("Parameter 'ranking_period' must be supplied as a scalar integeter vector.")
  if (! is_scalar_double(long_threshold)) stop("Parameter 'long_threshold' must be supplied as a scalar numeric vector.")
  if (! is_scalar_double(short_threshold)) stop("Parameter 'short_threshold' must be supplied as a scalar numeric vector.")
  if (! is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE).")

  if (! all(c("active_contract_ticker", "date", "TS_position", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active_contract_ticker', 'date', 'TS_position', 'field' and 'value'.")
  if (! all(c(front, back) %in% (distinct(data, TS_position) %>% flatten_int())))
    stop("The dataframe supplied in 'data' doesn't contain data for the term structure positions supplied in the `front` and `back` parameters.")
  if (! "PX_LAST" %in% (distinct(data, field) %>% flatten_chr()))
    stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column.")
  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (front > back) stop("The front contract must come before the back contract on the term strucutre (front < back).")
  if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1.")


  data %<>%
    filter(field == "PX_LAST", TS_position %in% c(front, back)) %>%
    select(name = active_contract_ticker, date, field, TS_position, value) %>%
    spread(field, value) %>%
    spread(TS_position, PX_LAST) %>%
    rename(front = paste0(!! front), back = paste0(!! back)) %>%
    filter(complete.cases(front, back)) %>%
    mutate(`roll yield` = log(front/back)) %>%
    select(name, date, PX_LAST = front, `roll yield`) %>%
    gather(field, value, -c(name, date))

  factorem(name = "term structure",
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
}






#' Construct market factor
#'
#' @description Given a reference dataset and a set of parameters.
#'
#' @param data A dataframe/tibble. Columns must include `active_contract_ticker`, `date`, `TS_position`, `field` and `value`.
#' @param return_frequency A scalar character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param long A scalar logical vector. If `TRUE` long only, else short only. Default: `TRUE`.
#' @param geometric A scalar logical vector. If `TRUE` geometric returns are returned, else arithmetic. Default: `TRUE`.
#'
#' @return An S4 object of class \linkS4class{AssetPricingFactor} with slots:
#'   \itemize{
#'     \item{name: a scalar character vector: "market factor".}
#'     \item{returns: a tibble with columns `date` and `factor`.}
#'     \item{positions: a tibble with columns `date`, `name` and `position`.}
#'     \item{data: a tibble containing the original dataset used for factor construction.}
#'     \item{params: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr flatten_dfc filter group_by lag mutate rename select summarise
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr is_scalar_character is_scalar_logical map
#' @importFrom tidyr gather nest spread unnest
#'
#' @export
market_factor <- function(data,
                          return_frequency = "day",
                          long = TRUE,
                          geometric = TRUE){

  if (! is.data.frame(data)) stop("Parameter 'data' must be supplied as a dataframe.")
  if (! is_scalar_character(return_frequency)) stop("Parameter 'return_frequency' must be supplied as a scalar character vector.")
  if (! is_scalar_logical(long)) stop("Parameter 'long' must be supplied as a scalar logical vector (TRUE or FALSE).")
  if (! is_scalar_logical(geometric)) stop("Parameter 'geometric' must be supplied as a scalar logical vector (TRUE or FALSE).")

  if (! all(c("active_contract_ticker", "date", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active_contract_ticker', 'date', 'TS_position', 'field' and 'value'.")
  if (! "PX_LAST" %in% (distinct(data, field) %>% flatten_chr()))
    stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
    stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")

  positions <- data %>%
    filter(field == "PX_LAST", TS_position == 1L) %>%
    group_by(date) %>%
    nest() %>%
    mutate(positions = map(data, function(x) select(x, active_contract_ticker, value) %>%
        filter(complete.cases(.)) %>%
        select(name = active_contract_ticker))) %>%
    unnest(positions) %>%
    mutate(position = if_else(long, "long", "short"))

  returns <- data %>%
    filter(field == "PX_LAST", TS_position == 1L) %>%
    group_by(date) %>%
    summarise(factor = mean(value, na.rm = TRUE)) %>%
    filter(!is.na(factor)) %>%
    mutate(factor = factor / lag(factor, 1L) - 1L,
           period = if(return_frequency == "year") paste0(year(date))
           else if(return_frequency == "day") paste(year(date), yday(date), sep = ".")
           else paste(year(date), get(paste0(return_frequency))(date), sep = ".")) %>%
    group_by(period) %>%
    nest() %>%
    mutate(returns = map(data, function(x) {
      tibble(date = x$date[nrow(x)], leg = "factor", return = apply(select(x, factor), function(y) Return.cumulative(y, geometric = geometric), MARGIN = 2L)) %>%
        spread(leg, return)
    })) %>%
    unnest(returns, .drop = TRUE) %>%
    select(date, factor) %>%
    mutate(factor = if (long) factor else factor * -1L)

  args <- as.list(match.call())[-1L]

  .AssetPricingFactor(name = "market",
                      positions = positions,
                      returns = returns,
                      data = data,
                      params =  args[names(args) != "data"] %>%
                        flatten_dfc())
}


