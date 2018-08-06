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
#' @importFrom dplyr arrange desc distinct filter group_by mutate n row_number select slice ungroup
#' @importFrom lubridate yday month quarter semester week year
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr flatten_dbl is_scalar_character is_scalar_double is_scalar_integer is_scalar_logical pmap
#' @importFrom RcppRoll roll_meanr
#' @importFrom stats complete.cases
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
#' @param data a dataframe/tibble. Columns must include \code{name}, \code{date}, \code{field} and \code{value}.
#' @param positions  a dataframe/tibble. Returned by function \code{factor_positions}.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'month'.
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'day'.
#' @param price_variable a scalar character vector. Specifies the name of the variable hosting asset prices. Must be found in the \code{field} columns of \code{data}.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @return A tibble with columns \code{date}, \code{long} , \code{short} and \code{factor}.
#'
#' @importFrom dplyr distinct filter full_join group_by lag lead left_join mutate select summarise ungroup
#' @importFrom lubridate yday month quarter semester week year
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom PerformanceAnalytics Return.cumulative
#' @importFrom purrr flatten_chr is_scalar_character is_scalar_logical map
#' @importFrom stats complete.cases
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

  if(getRversion() >= "2.15.1")  utils::globalVariables(c("date", "field", "name", "period", "value"))

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
#' @importFrom purrr flatten_dfc is_scalar_character
#' @importFrom utils modifyList
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

  if (! is_scalar_character(name)) stop("Parameter 'name' must be supplied as a scalar character vector.")

  positions <- factor_positions(data = data, update_frequency = update_frequency, sort_variable = sort_variable, ranking_period = ranking_period,
                                long_threshold = long_threshold, short_threshold = short_threshold)
  returns <- factor_returns(data = data, positions = positions, price_variable = price_variable, update_frequency = update_frequency,
                            return_frequency =return_frequency)

  params <- list(`update frequency` = update_frequency, `return frequency` = return_frequency, `price variable` = price_variable,
                 `sort variable` = sort_variable, `sort levels` = sort_levels, geometric = geometric, `ranking period` = ranking_period,
                 `long threshold` = long_threshold, `short threshold` = short_threshold) %>%
    flatten_dfc()

  .AssetPricingFactor(name = name,
                      positions = positions,
                      returns = returns,
                      data = data,
                      params = params
                      )
}


#' Construct CHP factor
#'
#' @description Given a reference dataset and a set of parameters.
#'
#' @param price_data a dataframe/tibble. Columns must include \code{active contract ticker}, \code{date}, \code{TS position}, \code{field} and \code{value}. \code{field} must contain \code{PX_LAST} variable.
#' @param CHP_data a dataframe/tibble. Columns must include:
#'   \itemize{
#'     \item{\code{format}: must contain 'legacy'.}
#'     \item{\code{underlying}: must contain 'futures only'.}
#'     \item{\code{unit}: must contain '# positions'.}
#'     \item{\code{participant}: must contain 'commercial'.}
#'     \item{\code{position}: must contain 'long' & 'short'.}
#'     \item{\code{PX_LAST}: contains the corresponding values.}
#'   }
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', 'semester', 'quarter', 'month' or 'week'. Defaults to 'month'.
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for CHP average calculation.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector: "CHP factor".}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long} , \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset used for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr filter mutate select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr flatten_dfc
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

  if (! all(c("active contract ticker", "date", "TS position", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active contract ticker', 'date', 'TS position', 'field' and 'value'.")
  if (! "PX_LAST" %in% (distinct(data, field) %>% flatten_chr()))
    stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column.")
  if (! c("format", "underlying", "unit", "participant", "position", "PX_LAST") %in% all(names(CHP_data)))
    stop("The columns of the dataframe supplied in 'CHP_data' must include 'format', 'underlying', 'unit', 'participant', 'position' and 'PX_LAST'.")
  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1.")

  price_data %<>%
    filter(`TS position` == 1L, field == "PX_LAST") %>%
    select(name = `active contract ticker`, date, field, value)

  CHP_data %<>%
    filter(format == "legacy", underlying == "futures only", unit == "# positions", participant == "commercial", position %in% c("long", "short")) %>%
    select(name = `active contract ticker`, position, date, PX_LAST) %>%
    mutate(PX_LAST = abs(PX_LAST)) %>%
    spread(position, PX_LAST) %>%
    mutate(`inverse CHP` = (long + short) / long) %>%
    select(name, date, `inverse CHP`) %>%
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
           sort_variable = "inverse CHP",
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
#' @param data a dataframe/tibble. Columns must include \code{active contract ticker}, \code{date}, \code{TS position}, \code{field} and \code{value}. \code{field} must contain \code{PX_LAST} and \code{OPEN_INT} variables.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param aggregate a scalar logical vector. If \code{TRUE} open interest is aggregated over the whole term structure supplied in \code{data}. Defaults to \code{FALSE}.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector: "OI factor".}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long}, \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset used for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr everything filter group_by select summarise
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

  if(getRversion() >= "2.15.1")  utils::globalVariables(c("active contract ticker", "date", "field", "name", "period", "TS position", "value"))

  if (! all(c("active contract ticker", "date", "TS position", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active contract ticker', 'date', 'TS position', 'field' and 'value'.")
  if (! all(c("PX_LAST", "OPEN_INT") %in% (distinct(data, field) %>% flatten_chr())))
    stop("The dataframe supplied in 'data' doesn't contain the required variables 'PX_LAST' amd 'OPEN_INT' in the 'field' column.")
  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1.")

  OI <- data %>%
    filter(field == "OPEN_INT") %>%
    {
      if (aggregate) group_by(., `active contract ticker`, date, field) %>%
        summarise(value = sum(value)) %>%
        select(name = `active contract ticker`, everything())
      else
        filter(., `TS position` == 1L) %>%
        select(name = `active contract ticker`, date, field, value)
    }

  price <- data %>%
    filter(`TS position` == 1L, field == "PX_LAST") %>%
    select(name = `active contract ticker`, date, field, value)

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
#' @param data a dataframe/tibble. Columns must include \code{active contract ticker}, \code{date}, \code{TS position}, \code{field} and \code{value}.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector: "momentum factor".}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long} , \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset used for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
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

  if(getRversion() >= "2.15.1")  utils::globalVariables(c("active contract ticker", "date", "field", "name", "period", "TS position", "value"))

  if (! all(c("active contract ticker", "date", "TS position", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active contract ticker', 'date', 'TS position', 'field' and 'value'.")
  if (! "PX_LAST" %in% (distinct(data, field) %>% flatten_chr()))
    stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column.")
  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1.")

  data %<>%
    filter(field == "PX_LAST", `TS position` == 1L) %>%
    select(name = `active contract ticker`, date, field, value)

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
#' @param data a dataframe/tibble. Columns must include \code{active contract ticker}, \code{date}, \code{TS position}, \code{field} and \code{value}.
#' @param update_frequency a scalar character vector. Specifies the rebalancing frequency. Must be one of 'year', "semester", "quarter", "month" or "week". Defaults to "month".
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param front a scalar integer vector. Specifies the term structure position to use as front contract in roll yield (sort variable) calculation. Defaults to 1.
#' @param back a scalar integer vector. Specifies the term structure position to use as back contract in roll yield (sort variable) calculation. Defaults to 2.
#' @param ranking_period a scalar integer vector. Specifies number of periods in term of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#' @param long_threshold a scalar numeric vector. Specifies the threshold for short positions. Default: 0.5.
#' @param short_threshold a scalar numeric vector. Specifies the threshold for long positions. Default: 0.5.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector: "term structure factor".}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long} , \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset used for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr filter mutate rename select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr flatten_int is_scalar_character is_scalar_double is_scalar_integer is_scalar_logical
#' @importFrom stats complete.cases
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

  if(getRversion() >= "2.15.1")  utils::globalVariables(c("active contract ticker", "date", "field", "name", "period", "TS position", "value"))

  if (! all(c("active contract ticker", "date", "TS position", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active contract ticker', 'date', 'TS position', 'field' and 'value'.")
  if (! all(c(front, back) %in% (distinct(data, `TS position`) %>% flatten_int())))
    stop("The dataframe supplied in 'data' doesn't contain data for the term structure positions supplied in the `front` and `back` parameters.")
  if (! "PX_LAST" %in% (distinct(data, field) %>% flatten_chr()))
    stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column.")
  if (! update_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'update_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day")) stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")
  if (front > back) stop("The front contract must come before the back contract on the term strucutre (front < back).")
  if (! all(long_threshold >= 0L, long_threshold <= 1L, short_threshold >= 0L, short_threshold <= 1L)) stop("Parameters 'long_threshold' and 'short_threshold' must be between 0 and 1.")


  data %<>%
    filter(field == "PX_LAST", `TS position` %in% c(front, back)) %>%
    select(name = `active contract ticker`, date, field, `TS position`, value) %>%
    spread(field, value) %>%
    spread(`TS position`, PX_LAST) %>%
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
#' @param data a dataframe/tibble. Columns must include \code{active contract ticker}, \code{date}, \code{TS position}, \code{field} and \code{value}.
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param long a scalar logical vector. If \code{TRUE} long only, else short only. Default: \code{TRUE}.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector: "market factor".}
#'     \item{\code{returns}: a tibble with columns \code{date} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset used for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
#'
#' @importFrom dplyr filter group_by lag mutate rename select summarise
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr flatten_dfc is_scalar_character is_scalar_logical map
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

  if(getRversion() >= "2.15.1")  utils::globalVariables(c("active contract ticker", "date", "field", "name", "period", "TS position", "value"))

  if (! all(c("active contract ticker", "date", "field", "value") %in% names(data)))
    stop("The columns of the dataframe supplied in 'data' must include 'active contract ticker', 'date', 'TS position', 'field' and 'value'.")
  if (! "PX_LAST" %in% (distinct(data, field) %>% flatten_chr()))
    stop("The dataframe supplied in 'data' doesn't contain data for the require variable 'PX_LAST' in the field column.")
  if (! return_frequency %in% c("year", "semester", "quarter", "month", "week", "day"))
    stop("'return_frequency' must be one of 'year', 'semester', 'quarter', 'month', 'week' or 'day'.")

  positions <- data %>%
    filter(field == "PX_LAST", `TS position` == 1L) %>%
    group_by(date) %>%
    nest() %>%
    mutate(positions = map(data, function(x) select(x, `active contract ticker`, value) %>%
        filter(complete.cases(.)) %>%
        select(name = `active contract ticker`))) %>%
    unnest(positions) %>%
    mutate(position = if_else(long, "long", "short"))

  returns <- data %>%
    filter(field == "PX_LAST", `TS position` == 1L) %>%
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


