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
#' @importFrom lubridate week month quarter semester year
#' @importFrom magrittr "%<>%"
factor_positions <- function(data, update_frequency, sort_variable, sort_levels, ranking_period,
                             long_threshold, short_threshold, weighted){

  data <- dplyr::filter(data, field == !! sort_variable) %>% dplyr::mutate(year = lubridate::year(date))
  data <- if (update_frequency == "day") { dplyr::mutate(data, unit = lubridate::yday(date)) }
  else { dplyr::mutate(data, unit = do.call(what = !! update_frequency, args = list(date))) }

  positions <- dplyr::select(data, name = ticker, date, year, unit, value) %>%
    dplyr::group_by(name, year, unit) %>%  dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup()
  positions <- if (sort_levels){
    dplyr::group_by(positions, name) %>%
      dplyr::mutate(average = RcppRoll::roll_meanr(value, n = ranking_period, na.rm = T)) %>%
      dplyr::slice(ranking_period:dplyr::n()) %>%
      dplyr::select(name, date, year, unit, average) %>% dplyr::ungroup()
  } else {
    dplyr::group_by(positions, name) %>% dplyr::mutate(value = (value/dplyr::lag(value, 1L)) - 1L) %>%
      dplyr::slice(2L:dplyr::n()) %>%
      dplyr::mutate(average = RcppRoll::roll_meanr(value, n = ranking_period, na.rm = T)) %>%
      dplyr::slice(ranking_period:dplyr::n()) %>%
      dplyr::select(name, date, year, unit, average) %>% dplyr::ungroup()
  }

  dplyr::group_by(positions, year, unit) %>% dplyr::do({

    data <- dplyr::select(., date, name, average) %>% dplyr::filter(complete.cases(.)) %>%
      dplyr::arrange(dplyr::desc(average))

    long <- factor_longs(data = data, long_threshold = !! long_threshold, weighted = !! weighted)
    short <- factor_shorts(data = data, short_threshold = !! short_threshold, weighted = !! weighted)
    rbind(long, short)

  }) %>% dplyr::ungroup() %>% dplyr::arrange(year, unit) %>%
    dplyr::select(date, year, unit, dplyr::everything()) %>% data.table::as.data.table()
}


factor_longs <- function(data, long_threshold, weighted){
  long <-  dplyr::slice(data, 1L:(floor(nrow(data) * (1L - long_threshold)))) %>%
    dplyr::select(date, name, weight = average) %>% dplyr::mutate(position = "long")
  if (weighted){
    if (nrow(long) == 1L){ dplyr::mutate(long, weight = 1L) } else {
      averages <- scales::rescale(long$weight); i <- floor(NROW(averages) / 2L)
      weights <- sapply(1L:i, function(x){
        y <- (averages[x] - averages[NROW(averages) - (x - 1L)]) / mean(averages, na.rm = T)
        c((1L/NROW(averages)) * y, (1L/NROW(averages)) / y)
      }) %>% as.vector()
      weights <- if (NROW(averages) %% 2L != 0L) c(weights, 1L/NROW(averages)) else weights
      dplyr::mutate(long, weight = sort(weights, decreasing = T) / sum(weights))
    }
  } else { dplyr::select(long, date, name, position) }
}

factor_shorts <- function(data, short_threshold, weighted){
  short <- dplyr::slice(data, (ceiling(nrow(data) * (1L - short_threshold)) + 1L):nrow(data)) %>%
    dplyr::arrange(average) %>% dplyr::select(date, name, weight = average) %>%
    dplyr::mutate(position = "short")
  if (weighted){
    if (nrow(short) == 1L){ dplyr::mutate(short, weight = 1L) } else {
      averages <- scales::rescale(short$weight); i <- floor(NROW(averages) / 2L)
      weights <- sapply(1L:i, function(x){
        y <- (averages[x] - averages[NROW(averages) - (x - 1L)]) / mean(averages, na.rm = T)
        c((1L/NROW(averages)) * y, (1L/NROW(averages)) / y)
      }) %>% as.vector()
      weights <- if (NROW(averages) %% 2L != 0L) c(weights, 1L/NROW(averages)) else weights
      dplyr::mutate(short, weight = sort(weights, decreasing = F) / sum(weights))
    }
  } else { dplyr::select(short, date, name, position) }
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
#' @return A \code{\link[data.table]{data.table}} with factor returns by
#'   date and factor leg. Columns include \code{date}, \code{name} and
#'   \code{position}.
#'
#'
#' @importFrom lubridate month quarter semester week
#' @importFrom magrittr "%<>%"
factor_returns <- function(data, positions, update_frequency, return_frequency, price_variable, weighted){

  price <- dplyr::filter(data, field == !! price_variable) %>% dplyr::mutate(year = lubridate::year(date))
  data <- if (update_frequency == "day") { dplyr::mutate(price, unit = lubridate::yday(date)) }
  else { dplyr::mutate(price, unit = do.call(what = !! update_frequency, args = list(date))) }
  data <-  dplyr::select(data, name = ticker, date, year, unit, value) %>%
    dplyr::group_by(name) %>% dplyr::mutate(return = value/dplyr::lag(value, 1L) - 1L ) %>%
    dplyr::slice(2L:dplyr::n()) %>% dplyr::ungroup() %>% dplyr::select(name, date, year, unit, return)

  positions <- tibble::as_tibble(positions) %>% tidyr::nest(name:position) %>%
    dplyr::mutate(data = dplyr::lag(data, 1L)) %>% dplyr::slice(2L:dplyr::n()) %>% tidyr::unnest()

  returns <- dplyr::left_join(data, positions, by = c("name", "year", "unit")) %>%
    dplyr::filter(! is.na(position)) %>% dplyr::group_by(position, date) %>%
    dplyr::summarise(mean = if(weighted) return %*% weight else mean(return, na.rm = T)) %>%
    dplyr::ungroup() %>% tidyr::spread(position, mean) %>%
    dplyr::mutate(short = short * -1L, year = lubridate::year(date)) %>%
    dplyr::mutate(factor = apply(.[, c("long", "short")], mean, MARGIN = 1L, na.rm = T))
  returns <- if (return_frequency == "day") { dplyr::mutate(returns, unit = lubridate::yday(date)) }
  else { dplyr::mutate(returns, unit = do.call(what = !! return_frequency, args = list(date))) }
  returns <- dplyr::group_by(returns, year, unit) %>%
    dplyr::summarise_at(dplyr::vars(long, short, factor), dplyr::funs(return_cumulative)) %>%
    dplyr::ungroup()

  data <- if (return_frequency == "day") { dplyr::mutate(price, unit = lubridate::yday(date)) }
  else { dplyr::mutate(price, unit = do.call(what = !! return_frequency, args = list(date))) }
  data <- dplyr::select(data, date, year, unit) %>% dplyr::group_by(year, unit) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>% dplyr::ungroup()

  dplyr::left_join(returns, data, by = c("year", "unit")) %>% dplyr::select(date, long, short, factor) %>%
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
#' @param data a dataframe/tibble. Columns must include \code{ticker}, \code{date},
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
#'   \code{sort_variable}'s levels, else on relative changes. Default: \code{TRUE}.
#'
#' @param weighted a scalar \code{\link[base]{logical}} vector.
#'   If \code{TRUE} adjusts portoflio weights with respect to sorting variable, else
#'   equal weights are used. Defaults to \code{FALSE}.
#'
#' @param ranking_period a scalar integer vector. Specifies number of periods in term
#'   of \code{update_frequency} looking backward for averaging \code{sort_variable}.
#'   Default: 1 (sort on last observation).
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
                     price_variable = "PX_LAST", sort_variable = "PX_LAST",
                     sort_levels = T, weighted = T,
                     ranking_period = 1L, long_threshold = 0.5, short_threshold = 0.5){

  check_params(name = name, data = data, update_frequency = update_frequency,
               return_frequency = return_frequency, price_variable = price_variable,
               price_variables = unique(data$field), sort_variable = sort_variable,
               sort_variables = unique(data$field), sort_levels = sort_levels,
               ranking_period = ranking_period, long_threshold = long_threshold,
               short_threshold = short_threshold)

  positions <- factor_positions(data = data, update_frequency = update_frequency,
                                sort_variable = sort_variable, sort_levels = sort_levels,
                                ranking_period = ranking_period, long_threshold = long_threshold,
                                short_threshold = short_threshold, weighted = weighted)

  returns <- factor_returns(data = data, positions = positions[, !"date", with = F],
                            price_variable = price_variable, update_frequency = update_frequency,
                            return_frequency = return_frequency, weighted = weighted)


  params <- data.table::data.table(parameter = c("name", "update frequency", "return frequency", "price variable",
                                            "sort variable", "sort levels", "weighted", "ranking_period",
                                            "long_threshold", "short_threshold"),
                                   value = list(name, update_frequency, return_frequency, price_variable,
                                                sort_variable, sort_levels, weighted, ranking_period,
                                                long_threshold, short_threshold))

  methods::new("AssetPricingFactor", name = name, positions = positions[, !c("year", "unit"), with = F],
               returns = returns, data = data.table::as.data.table(data), parameters = params, call = match.call())
}


#' Fama-McBeth cross-sectional regression
#'
#'
#' @description Given two dataframes of assets returns and factor returns respectively,
#'   performs a Fama-McBeth two-pass cross-sectional regression.
#'
#'
#' @param assets_returns a dataframe with dates in the first column and assets returns
#'   in subsequent columns.
#'
#' @param factor_returns a dataframe with dates in the first column and factor returns
#'   in the second.
#'
#' @return An S4 object of class \code{\linkS4class{FamaMcBeth}}.
#'
#'
#' @export
famamcbeth <- function(assets_returns, factor_returns){

  check_params(factor_returns = factor_returns, assets_dates = assets_returns$date,
               factor_dates = factor_returns$date)

  tickers <- names(assets_returns)[names(assets_returns) != "date" ]
  data <- dplyr::full_join(assets_returns, factor_returns, by = "date")

  betas <- lapply(tickers, function(x){ formula <- as.formula(paste0("`", x, "` ~ factor"))
  lm(formula, data = data) }) %>% setNames(tickers)
  betas <- data.table::data.table(ticker = tickers, beta = betas)
  means <- data.table::data.table(ticker = tickers, mean = apply(dplyr::select(data, tickers),
                                                                 mean, MARGIN = 2L, na.rm = T))

  coefficients <- dplyr::mutate(betas, beta = purrr::map2(ticker, beta, function(x, y) broom::tidy(y) %>%
                                                            dplyr::filter(term == "factor") %>%
                                                            dplyr::select(estimate) %>% purrr::flatten_dbl())) %>%
    tidyr::unnest(beta)

  data <- dplyr::full_join(means, coefficients, by = "ticker") %>% data.table::as.data.table()
  formula <- as.formula("mean ~ beta"); lambda <- lm(formula, data = data)

  methods::new("FamaMcBeth", betas = betas, means = means, lambda = lambda, data = data, call = match.call())
}
