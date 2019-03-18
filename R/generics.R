# accessors ####

## betas ####
#' Accesses the \code{betas} slot of S4 objects of class \linkS4class{FamaMcBeth}
#'   (\href{https://bautheac.github.io/factorem/}{\pkg{factorem}}).
#'
#' @description Access method for the \code{betas} slot of class \linkS4class{FamaMcBeth}
#'   (\href{https://bautheac.github.io/factorem/}{\pkg{factorem}}).
#'
#' @param object an S4 object of class \linkS4class{FamaMcBeth}.
#'
#' @return A data.table with asset betas.
#'
#' @docType methods
#'
#' @rdname get_betas-methods
#'
#' @export
setGeneric(name = "get_betas", function(object) standardGeneric("get_betas"))

## call ####
#' Accesses the \code{call} slot of S4
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} objects.
#'
#' @description Access method for the \code{call} slot of various S4 objects from the
#'   (\href{https://github.com/bautheac/factorem/}{\pkg{factorem}}) package.
#'
#' @param object an S4 object of classes
#'   \itemize{
#'     \item{\linkS4class{AssetPricingFactor}.}
#'     \item{\linkS4class{FamaMcBeth}.}
#'   }
#'
#' @return The original call to the constructor function.
#'
#' @docType methods
#'
#' @rdname get_call-methods
#'
#' @export
setGeneric(name = "get_call", function(object) standardGeneric("get_call"))

# data ####
#' Accesses the \code{data} slot of S4 objects of class \linkS4class{AssetPricingFactor}
#'   (\href{https://bautheac.github.io/factorem/}{\pkg{factorem}}).
#'
#'
#' @description Access method for the \code{data} slot of S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://bautheac.github.io/factorem/}{\pkg{factorem}} package.
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A \link[data.table]{data.table} showing for each Bloomberg ticker and
#'   Bloomberg field the data contained in the \code{object} provided.
#'
#' @docType methods
#'
#' @rdname get_data-methods
#'
#' @importClassesFrom data.table data.table
#'
#' @export
setGeneric("get_data", function(object) standardGeneric("get_data"))

## lambda ####
#' Accesses the \code{lambda} slot of S4 objects of class \linkS4class{FamaMcBeth}
#'   (\href{https://bautheac.github.io/factorem/}{\pkg{factorem}}).
#'
#' @description Access method for the \code{lambda} slot of S4 objects of class
#'   \linkS4class{FamaMcBeth} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
#'
#' @param object an S4 object of class \linkS4class{FamaMcBeth}.
#'
#' @return An object of class 'lm' with Fama-McBeth
#'   cross-sectional regression results.
#'
#' @docType methods
#'
#' @rdname get_lambda-methods
#'
#' @export
setGeneric("get_lambda", function(object) standardGeneric("get_lambda"))

## means ####
#' Accesses the \code{means} slot of S4 objects of class \linkS4class{FamaMcBeth}
#'   (\href{https://bautheac.github.io/factorem/}{\pkg{factorem}}).
#'
#' @description Access method for the \code{means} slot of S4 objects of class
#'   \linkS4class{FamaMcBeth} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
#'
#' @param object an S4 object of class \linkS4class{FamaMcBeth}.
#'
#' @return A data.table of asset means.
#'
#' @docType methods
#'
#' @rdname get_means-methods
#'
#' @export
setGeneric("get_means", function(object) standardGeneric("get_means"))

## name ####
#' Accesses the \code{name} slot of S4 objects of class \linkS4class{AssetPricingFactor}
#'   (\href{https://bautheac.github.io/factorem/}{\pkg{factorem}}).
#'
#' @description Access method for the \code{name} slot of S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A scalar \code{\link[base]{character}} vector containing the name of the factor.
#'
#' @docType methods
#'
#' @rdname get_name-methods
#'
#' @export
setGeneric("get_name", function(object) standardGeneric("get_name"))

## parameters ####
#' Accesses the \code{parameters} slot of S4 objects of class \linkS4class{AssetPricingFactor}
#'   (\href{https://github.com/bautheac/factorem/}{\pkg{factorem}}).
#'
#' @description Access method for the \code{parameters} slot of S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A \code{\link[data.table]{data.table}} containing the original parameters
#'   supplied for factor construction.
#'
#' @docType methods
#'
#' @rdname get_parameters-methods
#'
#' @importFrom data.table data.table
#'
#' @export
setGeneric("get_parameters", function(object) standardGeneric("get_parameters"))

## positions ####
#' Accesses the \code{positions} slot of S4 objects of class \linkS4class{AssetPricingFactor}
#'   (\href{https://github.com/bautheac/factorem/}{\pkg{factorem}}).
#'
#' @description Access method for the \code{positions} slot of S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A \code{\link[data.table]{data.table}} of factor positions.
#'
#' @docType methods
#'
#' @rdname get_positions-methods
#'
#' @importFrom data.table data.table
#'
#' @export
setGeneric("get_positions", function(object) standardGeneric("get_positions"))

## returns ####
#' Accesses the \code{returns} slot of S4 objects of class \linkS4class{AssetPricingFactor}
#'   (\href{https://github.com/bautheac/factorem/}{\pkg{factorem}}).
#'
#' @description Access method for the \code{returns} slot of S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A \code{\link[data.table]{data.table}} of factor returns historical returns.
#'
#' @docType methods
#'
#' @rdname get_returns-methods
#'
#' @importFrom data.table data.table
#'
#' @export
setGeneric("get_returns", function(object) standardGeneric("get_returns"))



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
#'   looking backward for average CHP calculation. Defaults to 1 where
#'   sort is done on last observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for long positions. Default: 0.5.
#'
#' @param weighted a scalar \code{\link[base]{logical}} vector.
#'   If 'TRUE' adjusts portoflio weights with respect to pressure, else
#'   equal weights are used. Defaults to 'FALSE'.
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
#' @importFrom Rdpack reprompt
#' @importFrom tibble tibble
#' @import BBGsymbols
#'
#'
#' @export
setGeneric("CHP_factor",
           function(price_data, CHP_data, update_frequency = "month", return_frequency = "day",
                    ranking_period = 6L, long_threshold = 0.5, short_threshold = 0.5,
                    weighted = F) standardGeneric("CHP_factor"))



## pressure ####
#' Market participant pressure factor
#'
#'
#' @description Provided with futures contract front price and term
#'   structure aggregated position data from Bloomberg retrieved with
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}},
#'   constructs market participant pressure factors.
#'
#'
#' @param price_data an S4 object of class \code{\linkS4class{FuturesTS}}.
#'   \code{\linkS4class{FuturesTS}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_TS}} function in the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param position_data an S4 object of class \code{\linkS4class{FuturesCFTC}}.
#'   \code{\linkS4class{FuturesCFTC}} objects are returned by the
#'   \code{\link[pullit]{BBG_futures_CFTC}} function in the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param format a scalar \code{\link[base]{character}} vector.
#'   Specifies the CFTC report format to use for calculating participant
#'   pressure: 'disaggregated', 'legacy', 'supplemental' or
#'   'traders in financial futures'. Defaults to 'disaggregated'.
#'
#' @param underlying a scalar \code{\link[base]{character}} vector.
#'   Specifies the underlying instrument type to use for calculating participant
#'   pressure: 'futures only' or 'futures & options'. Defaults to 'futures only'.
#'
#' @param unit a scalar \code{\link[base]{character}} vector.
#'   Specifies the unit to use for calculating participant
#'   pressure: 'contracts' or 'traders'. Defaults to 'contracts'.
#'
#' @param participant a scalar \code{\link[base]{character}} vector.
#'   Specifies the participant to use for calculating market pressure.
#'   Format specific: 'producer/merchant/processor/user', 'managed money',
#'   'swap dealers' or 'other reportables' (disaggregated); 'commercial',
#'   'non-commercial', 'non-reportable' or 'total' (legacy);
#'   'commercial - non-CIT', 'non-commercial - non-CIT', or 'index traders'
#'   (supplemental); 'asset manager/institutional', 'dealer/intermediary',
#'   'leveraged funds', 'other reportables' (traders in financial futures).
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
#'   looking backward for average CHP calculation. Defaults to 1 where
#'   sort is done on last observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for long positions. Default: 0.5.
#'
#' @param sort_levels a scalar \code{\link[base]{logical}} vector.
#'   If \code{TRUE} sorts on pressure levels, else
#'   on relative changes in pressure. Defaults to \code{TRUE}.
#'
#' @param weighted a scalar \code{\link[base]{logical}} vector.
#'   If \code{TRUE} adjusts portoflio weights with respect to pressure, else
#'   equal weights are used. Defaults to \code{FALSE}.
#'
#' @return An S4 object of class \code{\linkS4class{PressureFactor}}.
#'
#'
#' @docType methods
#' @rdname pressure_factor-methods
#'
#'
#' @importClassesFrom pullit FuturesCFTC FuturesTS
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#' @importFrom tibble tibble
#' @import BBGsymbols
#'
#'
#' @export
setGeneric("pressure_factor",
           function(price_data, position_data, format = "disaggregated", underlying = "futures only",
                    unit = "contracts", participant = "producer/merchant/processor/user",
                    update_frequency = "month", return_frequency = "day", ranking_period = 6L,
                    long_threshold = 0.5, short_threshold = 0.5,
                    sort_levels = T, weighted = F) standardGeneric("pressure_factor"))



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
#'   Defaults to 1 where sort is done on last observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for long positions. Default: 0.5.
#'
#' @param weighted a scalar \code{\link[base]{logical}} vector.
#'   If 'TRUE' adjusts portoflio weights with respect to sorting variable
#'   values, else equal weights are used. Defaults to 'FALSE'.
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
#' @importFrom Rdpack reprompt
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("OI_nearby_factor",
           function(data, update_frequency = "month", return_frequency = "day",
                    ranking_period = 1L,
                    long_threshold = 0.5, short_threshold = 0.5,
                    weighted = F) standardGeneric("OI_nearby_factor"))


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
#'   Defaults to 1 where sort is done on last observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector.
#'   Specifies the threshold for long positions. Default: 0.5.
#'
#' @param weighted a scalar \code{\link[base]{logical}} vector.
#'   If 'TRUE' adjusts portoflio weights with respect to sorting variable
#'   values, else equal weights are used. Defaults to 'FALSE'.
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
#' @importFrom Rdpack reprompt
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("OI_aggregate_factor",
           function(price_data, aggregate_data,
                    update_frequency = "month", return_frequency = "day",
                    ranking_period = 1L,
                    long_threshold = 0.5, short_threshold = 0.5,
                    weighted = F) standardGeneric("OI_aggregate_factor"))


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
#'   average price return calculation. Defaults to 1 where sort is done on last
#'   observed return.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector. Specifies
#'   the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector. Specifies
#'   the threshold for long positions. Default: 0.5.
#'
#' @param weighted a scalar \code{\link[base]{logical}} vector.
#'   If 'TRUE' adjusts portoflio weights with respect to sorting variable
#'   values, else equal weights are used. Defaults to 'FALSE'.
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
#' @importFrom Rdpack reprompt
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("momentum_factor",
           function(data, update_frequency = "week", return_frequency = "day",
                    ranking_period = 4L,
                    long_threshold = 0.5, short_threshold = 0.5,
                    weighted = F) standardGeneric("momentum_factor"))



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
#'   average roll yield calculation. Defaults to 1 where sort is done on last
#'   observation only.
#'
#' @param long_threshold a scalar \code{\link[base]{numeric}} vector. Specifies
#'   the threshold for short positions. Default: 0.5.
#'
#' @param short_threshold a scalar \code{\link[base]{numeric}} vector. Specifies
#'   the threshold for long positions. Default: 0.5.
#'
#' @param weighted a scalar \code{\link[base]{logical}} vector.
#'   If 'TRUE' adjusts portoflio weights with respect to sorting variable
#'   values, else equal weights are used. Defaults to 'FALSE'.
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
#' @importFrom Rdpack reprompt
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("TS_factor",
           function(data, update_frequency = "month", return_frequency = "day",
                    front = 1L, back = 2L, ranking_period = 1L,
                    long_threshold = 0.5, short_threshold = 0.5,
                    weighted = F) standardGeneric("TS_factor"))


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
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}}.
#'
#'
#' @docType methods
#' @rdname market_factor-methods
#'
#'
#' @importClassesFrom pullit EquityMarket FuturesTS
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("market_factor",
           function(data, return_frequency = "month", long = T) standardGeneric("market_factor"))



# summary ####

## performance summary ####
#' Factor performance summary
#'
#'
#' @description Factor performance summary method. Returns factor returns
#'   by month and year.
#'
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @param leg a character vector. 'long', 'short' and 'factor' display
#'   the returns for the long, short and the factor itself respectively.
#'   Defaults to 'factor'.
#'
#'
#' @return A \code{\link[base]{data.frame}} with months as colums and years as rows.
#'   Column total shows the cumulated return for the corresponding year.
#'
#'
#' @docType methods
#'
#' @rdname summary-methods
#'
#'
#' @export
setGeneric("summary", function(object, leg = "factor") standardGeneric("summary"))




