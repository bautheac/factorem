
# factor construction ####

## CHP ####
#' Commercial hedging pressure (CHP) factor
#'
#'
#' @description Provided with futures contract front price and term
#'   structure aggregated position data from Bloomberg retrieved with
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}},
#'   construct commercial hedging pressure factor.
#'
#'
#' @param price_data an S4 object of class \code{\linkS4class{FuturesTS}}.
#'   \code{\linkS4class{FuturesTS}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_TS}} function in the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param CHP_data an S4 object of class \code{\linkS4class{FuturesCFTC}}.
#'   \code{\linkS4class{FuturesCFTC}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_CFTC}} function in the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param update_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the rebalancing frequency. Must be one of 'year', 'semester',
#'   'quarter', 'month' or 'week'. Defaults to 'month'.
#'
#' @param return_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the frequency of the returns output. Must be one of 'year',
#'   'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'day'.
#'
#' @param ranking_period a scalar \code{\link[base]{integer}} vector.
#'   Specifies number of periods in term of \code{update_frequency}
#'   looking backward for average CHP calculation. Defaults to 0 where
#'   sort is done on last observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for long positions. Default: 0.5.
#'
#' @param geometric a scalar \code{\link[base]{logical}} vector. If \code{TRUE}
#'   geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#'
#' @return An S4 object of class \code{\linkS4class{CHPFactor}}.
#'
#'
#' @docType methods
#' @rdname CHP_factor-methods
#'
#'
#' @importClassesFrom pullit FuturesCFTC FuturesTS
#' @importFrom data.table data.table
#' @importFrom tibble tibble
#' @import BBGsymbols
#'
#'
#' @export
setGeneric("CHP_factor",
           function(price_data, CHP_data, update_frequency, return_frequency, ranking_period,
                    long_threshold, short_threshold, geometric) standardGeneric("CHP_factor"))



## OI nearby ####
#' Open interest growth (OI) nearby factor
#'
#'
#' @description Provided with futures contract front price and open interest data from
#'   Bloomberg retrieved with \href{https://github.com/bautheac/pullit/}{\pkg{pullit}},
#'   construct open interest growth (OI) nearby factor.
#'
#'
#' @param data an S4 object of class \code{\linkS4class{FuturesTS}}.
#'   \code{\linkS4class{FuturesTS}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_TS}} function from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param update_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the rebalancing frequency. Must be one of 'year', 'semester',
#'   'quarter', 'month' or 'week'. Defaults to 'month'.
#'
#' @param return_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the frequency of the returns output. Must be one of 'year',
#'   'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'day'.
#'
#' @param ranking_period a scalar \code{\link[base]{integer}} vector.
#'   Specifies number of periods in term of \code{update_frequency}
#'   looking backward for nearby open interest average growth calculation.
#'   Defaults to 0 where sort is done on last observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for long positions. Default: 0.5.
#'
#' @param geometric a scalar \code{\link[base]{logical}} vector. If \code{TRUE}
#'   geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#'
#' @description Following Hong & Yogo.
#'
#'
#' @return An S4 object of class \code{\linkS4class{OIFactor}}.
#'
#'
#' @docType methods
#' @rdname OI_nearby_factor-methods
#'
#'
#' @importClassesFrom pullit FuturesTS
#' @importFrom data.table data.table
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("OI_nearby_factor",
           function(data, update_frequency, return_frequency, ranking_period, long_threshold,
                    short_threshold, geometric) standardGeneric("OI_nearby_factor"))


## OI aggregate ####
#' Construct futures open interest growth (OI) aggregate factor
#'
#'
#' @description Provided with futures contract front price and aggregate open
#'   interest data from Bloomberg retrieved with
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}, construct
#'   aggregate open interest growth (OI) factor.
#'
#'
#' @param price_data an S4 object of class \code{\linkS4class{FuturesTS}}.
#'   \code{\linkS4class{FuturesTS}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_TS}} function from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param aggregate_data an S4 object of class \code{\linkS4class{FuturesAggregate}}.
#'   \code{\linkS4class{FuturesAggregate}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_aggregate}} function from the
#'   \code{pullit} package.
#'
#' @param update_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the rebalancing frequency. Must be one of 'year', 'semester',
#'   'quarter', 'month' or 'week'. Defaults to 'month'.
#'
#' @param return_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the frequency of the returns output. Must be one of 'year',
#'   'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'day'.
#'
#' @param ranking_period a scalar \code{\link[base]{integer}} vector.
#'   Specifies number of periods in term of \code{update_frequency}
#'   looking backward for aggregate open interest average growth calculation.
#'   Defaults to 0 where sort is done on last observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for long positions. Default: 0.5.
#'
#' @param geometric a scalar \code{\link[base]{logical}} vector. If \code{TRUE}
#'   geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#'
#' @description Basu & Bautheac.
#'
#'
#' @return An S4 object of class \code{\linkS4class{OIFactor}}.
#'
#'
#' @docType methods
#' @rdname OI_aggregate_factor-methods
#'
#'
#' @importClassesFrom pullit FuturesAggregate FuturesTS
#' @importFrom data.table data.table
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("OI_aggregate_factor",
           function(price_data, aggregate_data, update_frequency, return_frequency, ranking_period,
                    long_threshold, short_threshold, geometric) standardGeneric("OI_aggregate_factor"))


## momentum ####
#' Construct momentum factor
#'
#'
#' @description Provided with futures contract front or equity price data
#'   from Bloomberg retrieved with
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}, construct
#'   momentum factor.
#'
#'
#' @param data an S4 object of class \code{\linkS4class{FuturesTS}} or
#'   \code{\linkS4class{EquityMarket}}. \code{\linkS4class{FuturesTS}} and
#'   \code{\linkS4class{EquityMarket}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_TS}} and \code{\link[pullit]{BBG_equity_market}}
#'   functions from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package respectively.
#'
#' @param update_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the rebalancing frequency. Must be one of 'year', 'semester',
#'   'quarter', 'month' or 'week'. Defaults to 'month'.
#'
#' @param return_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the frequency of the returns output. Must be one of 'year',
#'   'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'day'.
#'
#' @param ranking_period a scalar \code{\link[base]{integer}} vector. Specifies
#'   number of periods in term of \code{update_frequency} looking backward for
#'   average price return calculation. Defaults to 0 where sort is done on last
#'   observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector. Specifies
#'   the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector. Specifies
#'   the threshold for long positions. Default: 0.5.
#'
#' @param geometric a scalar \code{\link[base]{logical}} vector. If \code{TRUE}
#'   geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#'
#' @description Sorts on past returns.
#'
#'
#' @return An S4 object of class \code{\linkS4class{MomentumFactor}}.
#'
#'
#' @docType methods
#' @rdname momentum_factor-methods
#'
#'
#' @importClassesFrom pullit EquityMarket FuturesTS
#' @importFrom data.table data.table
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("momentum_factor",
           function(data, update_frequency, return_frequency, ranking_period,
                    long_threshold, short_threshold, geometric) standardGeneric("momentum_factor"))



## term structure ####
#' Term structure factor
#'
#'
#' @description Provided with futures contract front price data
#'   from Bloomberg retrieved with
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}, construct
#'   term structure factor.
#'
#'
#' @param data an S4 object of class \code{\linkS4class{FuturesTS}}.
#'   \code{\linkS4class{FuturesTS}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_TS}} function from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param update_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the rebalancing frequency. Must be one of 'year', 'semester',
#'   'quarter', 'month' or 'week'. Defaults to 'month'.
#'
#' @param return_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the frequency of the returns output. Must be one of 'year',
#'   'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'day'.
#'
#' @param front a scalar \code{\link[base]{integer}} vector. Specifies the
#'   term structure position to use as front contract in roll yield
#'   (sort variable) calculation. Defaults to 1.
#'
#' @param back a scalar \code{\link[base]{integer}} vector. Specifies the
#'   term structure position to use as back contract in roll yield
#'   (sort variable) calculation. Defaults to 2.
#'
#' @param ranking_period a scalar \code{\link[base]{integer}} vector. Specifies
#'   number of periods in term of \code{update_frequency} looking backward for
#'   average roll yield calculation. Defaults to 0 where sort is done on last
#'   observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector. Specifies
#'   the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector. Specifies
#'   the threshold for long positions. Default: 0.5.
#'
#' @param geometric a scalar \code{\link[base]{logical}} vector. If \code{TRUE}
#'   geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#'
#' @description Sorts on relative price difference between specified front and back.
#'
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}}.
#'
#'
#' @docType methods
#' @rdname TS_factor-methods
#'
#'
#' @importClassesFrom pullit FuturesTS
#' @importFrom data.table data.table
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("TS_factor",
           function(data, update_frequency, return_frequency, front, back, ranking_period,
                    long_threshold, short_threshold, geometric) standardGeneric("TS_factor"))


## market ####
#' Market factor
#'
#'
#' @description Provided with futures contract front or equity price data
#'   from Bloomberg retrieved with
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}, construct
#'   market factor.
#'
#'
#' @param data an S4 object of class \code{\linkS4class{FuturesTS}} or
#'   \code{\linkS4class{EquityMarket}}. \code{\linkS4class{FuturesTS}} and
#'   \code{\linkS4class{EquityMarket}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_TS}} and
#'   \code{\link[pullit]{BBG_equity_market}} functions from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package
#'   respectively.
#'
#' @param return_frequency a scalar \code{\link[base]{character}} vector.
#'   Specifies the frequency of the returns output. Must be one of 'year',
#'   'semester', 'quarter', 'month', 'week' or 'day'. Defaults to 'day'.
#'
#' @param long a scalar logical vector. If \code{TRUE} long only, else
#'   short only. Default: \code{TRUE}.
#'
#' @param geometric a scalar \code{\link[base]{logical}} vector. If \code{TRUE}
#'   geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}}.
#'
#'
#' @docType methods
#' @rdname market_factor-methods
#'
#'
#' @importClassesFrom pullit EquityMarket FuturesTS
#' @importFrom data.table data.table
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("market_factor",
           function(data, return_frequency, long, geometric) standardGeneric("market_factor"))
