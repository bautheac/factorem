#' Get time series of long and short factor positions
#'
#' @param data A dataframe/tibble. Columns must include `name`, `date`, `field` and `value`.
#' @param update_frequency A unit length character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month", "week" or "day".
#' @param sort_variable A unit length character vector. Specifies the name of the variable to use for sorting. Must be found in the `field` columns of `data`.
#' @param sort_levels Logical. If `TRUE`, sort is done on `sort_variable`'s levels, else on relative changes. Default: `FALSE`.
#' @param ranking_period A unit length integer vector. Specifies number of periods in term of `update_frequency` looking backward for averaging `sort_variable`
#' @param short_threshold A unit length numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param long_threshold A unit length numeric vector. Specifies the threshold for long positions. Default: 0.5.
#'
#' @return A tibble with columns `date`, `name` and `position`.
#'
#' @importFrom dplyr arrange desc filter group_by mutate select slice row_number ungroup
#' @importFrom lubridate yday month quarter semester week year
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr flatten_dbl pmap
#' @importFrom RcppRoll roll_meanr
#' @importFrom tibble tibble
#' @importFrom tidyr nest spread unnest

factor_positions <- function(data,
                             update_frequency = c("year", "semester", "quarter", "month", "week", "day"),
                             sort_variable = "PX_LAST",
                             sort_levels = FALSE,
                             ranking_period = 0L,
                             long_threshold = 0.5,
                             short_threshold = 0.5){

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
    mutate(value = if(sort_levels) value else log(value/dplyr::lag(value, 1L))) %>%
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
#' @param update_frequency A unit length character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month", "week" or "day".
#' @param return_frequency A unit length character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day".
#' @param price_variable A unit length character vector. Specifies the name of the variable hosting asset prices. Must be found in the `field` columns of `data`.
#' @param geometric Logical. If `TRUE` geometric returns are returned, else arithmetic. Default: `TRUE`.
#'
#' @return A tibble with columns `date`, `long` , `short` and `factor`.
#'
#' @importFrom dplyr distinct filter full_join group_by lag lead left_join mutate select summarise ungroup
#' @importFrom lubridate yday month quarter semester week year
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom PerformanceAnalytics Return.cumulative
#' @importFrom purrr map
#' @importFrom tidyr nest unnest

factor_returns <- function(data,
                           positions,
                           update_frequency = c("year", "semester", "quarter", "month", "week", "day"),
                           return_frequency = c("year", "semester", "quarter", "month", "week", "day"),
                           price_variable = "PX_LAST",
                           geometric = TRUE){

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
#' @param name A unit character vector specifying the name to use for the factor.
#' @param ... Factor construction parameters:
#'   \itemize{
#'     \item{data: a dataframe/tibble. Columns must include name, date, field and value.}
#'     \item{update_frequency: a unit length character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month", "week" or "day".}
#'     \item{return_frequency: a unit length character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day".}
#'     \item{sort_variable: a unit length character vector. Specifies the name of the variable to use for sorting. Must be found in the `field` columns of `data`.}
#'     \item{price_variable: a unit length character vector. Specifies the name of the variable hosting asset prices. Must be found in the `field` columns of `data`.}
#'     \item{sort_levels: logical. If `TRUE`, sort is done on `sort_variable`'s levels, else on relative changes. Default: `FALSE`.}
#'     \item{ranking_period: a unit length integer vector. Specifies number of periods in term of `update_frequency` looking backward for averaging `sort_variable`.}
#'     \item{long_threshold: a unit length numeric vector. Specifies the threshold for short positions. Default: 0.5.}
#'     \item{short_threshold: a unit length numeric vector. Specifies the threshold for long positions. Default: 0.5.}
#'   }
#'
#' @return An S4 object of class \linkS4class{AssetPricingFactor} with slots:
#'   \itemize{
#'     \item{name: a unit character vector specifying the name to use for the factor.}
#'     \item{returns: a tibble with columns `date`, `long` , `short` and `factor`.}
#'     \item{positions: a tibble with columns `date`, `name` and `position`.}
#'     \item{data: a tibble containing the original dataset used for factor construction.}
#'     \item{params: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr flatten_dfc
#' @importFrom magrittr "%>%"
#'
#' @export

factorem <- function(name = NA, ...){

  positions <- do.call("factor_positions", list(...)[c("data", "update_frequency", "sort_variable", "sort_levels", "ranking_period", "long_threshold", "short_threshold")])
  returns <- do.call("factor_returns", modifyList(x = list(...)[c("data", "price_variable", "update_frequency", "return_frequency")],
                                                  val = list(positions = positions)))

  .AssetPricingFactor(name = name,
                      positions = positions,
                      returns = returns,
                      data = list(...)[["data"]],
                      params = list(...)[c("update_frequency",
                                           "return_frequency",
                                           "sort_variable",
                                           "price_variable",
                                           "sort_levels",
                                           "geometric",
                                           "ranking_period",
                                           "long_threshold",
                                           "short_threshold")]%>%
                        flatten_dfc())
}


#' Construct CHP factor
#'
#' @description Given a reference dataset and a set of parameters.
#'
#' @param price_data A dataframe/tibble. Columns must include `name`, `date`, `field` and `value`. `field` must contain `PX_LAST` variable.
#' @param CHP_data A dataframe/tibble. Columns must include:
#'   \itemize{
#'     \item{format: must contain `legacy`.}
#'     \item{underlying: must contain `futures only`.}
#'     \item{unit: must contain `# positions`.}
#'     \item{participant: must contain `commercial`.}
#'     \item{position: must contain `long` & `short`.}
#'     \item{PX_LAST: contains the corresponding values.}
#'   }
#' @param update_frequency A unit length character vector. Specifies the rebalancing frequency. Must be one of "year", "semester", "quarter", "month" or "week".
#' @param return_frequency A unit length character vector. Specifies the frequency of the returns output. Must be one of "year", "semester", "quarter", "month", "week" or "day".
#' @param sort_levels Logical. If `TRUE`, sort is done on `sort_variable`'s levels, else on relative changes. Default: `FALSE`.
#' @param ranking_period A unit length integer vector. Specifies number of periods in term of `update_frequency` looking backward for averaging `sort_variable`.
#' @param long_threshold A unit length numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold A unit length numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric Logical. If `TRUE` geometric returns are returned, else arithmetic. Default: `TRUE`.
#'
#' @return an \linkS4class{AssetPricingFactor} object with slots:
#'   \itemize{
#'     \item{name: a unit character vector: "CHP factor".}
#'     \item{returns: a tibble with columns `date`, `long` , `short` and `factor`.}
#'     \item{positions: a tibble with columns `date`, `name` and `position`.}
#'     \item{data: a tibble containing the original dataset used for factor construction.}
#'     \item{params: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr filter flatten_dfc mutate select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom tidyr gather spread
#'
#' @export
CHP_factor <- function(price_data,
                       CHP_data,
                       update_frequency = c("year", "semester", "quarter", "month", "week"),
                       return_frequency = c("year", "semester", "quarter", "month", "week", "day"),
                       sort_levels = FALSE,
                       ranking_period = 0L,
                       long_threshold = 0.5,
                       short_threshold = 0.5,
                       geometric = TRUE){

  price_data %<>%
    filter(TS_position == 1L) %>%
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
           sort_variable = "1 / CHP",
           sort_levels = sort_levels,
           price_variable = "PX_LAST",
           ranking_period = ranking_period,
           long_threshold = long_threshold,
           short_threshold = short_threshold,
           geometric = geometric)
}

