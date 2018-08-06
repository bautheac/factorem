# accessors ####

#' Generic method for accessing factor name
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A unit character vector containing the name of the factor.
#'
#' @export
setGeneric("name", function(factor) standardGeneric("name"))

#' Generic method for accessing factor returns
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A tibble of factor returns.
#'
#' @export
setGeneric("returns", function(factor) standardGeneric("returns"))

#' Generic method for accessing positions
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A tibble of factor positions.
#'
#' @export
setGeneric("positions", function(factor) standardGeneric("positions"))

#' Generic method for accessing factor data construction
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A tibble containing the original dataset used for factor construction.
#'
#' @export
setGeneric("dataset", function(factor) standardGeneric("dataset"))

#' Generic method for accessing factor data construction
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A tibble containing the original parameters supplied for factor construction.
#'
#' @export
setGeneric("parameters", function(factor) standardGeneric("parameters"))

# plots ####

#' Generic method factor performance summary plot by leg
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A line plot showing the wealth index for the factor itself as well as that of both long and short legs separately.
#'
#' @export
setGeneric("plot_performance", function(factor) standardGeneric("plot_performance"))

#' Generic method factor positions summary plot by leg
#'
#' @param factor an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A bar plot showing the proportion of time individual names appear in the factor.
#'
#' @export
setGeneric("plot_positions", function(factor) standardGeneric("plot_positions"))


# factor construction ####

#' Construct commercial hedging pressure (CHP) factor
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
#' @description Sorts inverse CHP.
#'
#' @return An S4 object of class \code{\linkS4class{CHPFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector: "CHP factor".}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long} , \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset supplied for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
setGeneric("CHP_factor",
           function(price_data, CHP_data, update_frequency, return_frequency, ranking_period, long_threshold, short_threshold, geometric) standardGeneric("CHP_factor"))

#' Construct futures open interest growth (OI) factor
#'
#' @param data an S4 object of class \code{\linkS4class{FuturesTS}} or \code{\linkS4class{FuturesAggregate}}. \code{\linkS4class{FuturesTS}} and
#'   \code{\linkS4class{FuturesAggregate}} objects are returned by the \code{\link[pullit]{bbg_futures_TS}} and \code{\link[pullit]{bbg_futures_aggregate}}
#'   functions from the \code{pullit} package respectively.
#'
#' @description Following Hong & Yogo (nearby) as well as Basu & Bautheac (aggregate).
#'
#' @return An S4 object of class \code{\linkS4class{OIFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector showing the factor name.}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long}, \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset supplied for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
setGeneric("OI_factor",
           function(data, update_frequency, return_frequency, ranking_period, long_threshold, short_threshold, geometric) standardGeneric("OI_factor"))

#' Construct momentum factor
#'
#' @param data an S4 object of class \code{\linkS4class{FuturesTS}} or \code{\linkS4class{EquityMarket}}. \code{\linkS4class{FuturesTS}} and
#'   \code{\linkS4class{EquityMarket}} objects are returned by the \code{\link[pullit]{bbg_futures_TS}} and \code{\link[pullit]{bbg_equity_market}}
#'   functions from the \code{pullit} package respectively.
#'
#' @description Sorts on past returns.
#'
#' @return An S4 object of class \code{\linkS4class{MomentumFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector showing the factor name.}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long}, \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset supplied for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
setGeneric("momentum_factor",
           function(data, update_frequency, return_frequency, ranking_period, long_threshold, short_threshold, geometric) standardGeneric("momentum_factor"))


#' Construct term structure factor
#'
#' @param data an S4 object of class \code{\linkS4class{FuturesTS}}. \code{\linkS4class{FuturesTS}} objects are returned by the \code{\link[pullit]{bbg_futures_TS}}
#'   function from the \code{pullit} package.
#'
#' @description Sorts on relative difference between specified front and back.
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector showing the factor name.}
#'     \item{\code{returns}: a tibble with columns \code{date}, \code{long} , \code{short} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset used for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
setGeneric("TS_factor",
           function(data, update_frequency, return_frequency, front, back, ranking_period, long_threshold, short_threshold, geometric) standardGeneric("TS_factor"))

#' Construct market factor
#'
#' @param data an S4 object of class \code{\linkS4class{FuturesTS}} or \code{\linkS4class{EquityMarket}}. \code{\linkS4class{FuturesTS}} and
#'   \code{\linkS4class{EquityMarket}} objects are returned by the \code{\link[pullit]{bbg_futures_TS}} and \code{\link[pullit]{bbg_equity_market}}
#'   functions from the \code{pullit} package respectively.
#' @param return_frequency a scalar character vector. Specifies the frequency of the returns output. Must be one of 'year', "semester", "quarter", "month", "week" or "day". Defaults to "day".
#' @param long a scalar logical vector. If \code{TRUE} long only, else short only. Default: \code{TRUE}.
#' @param geometric a scalar logical vector. If \code{TRUE} geometric returns are returned, else arithmetic. Default: \code{TRUE}.
#'
#' @description Long only or short only equally weighted portfolio.
#'
#' @return An S4 object of class \code{\linkS4class{AssetPricingFactor}} with slots:
#'   \itemize{
#'     \item{\code{name}: a scalar character vector: "market factor".}
#'     \item{\code{returns}: a tibble with columns \code{date} and \code{factor}.}
#'     \item{\code{positions}: a tibble with columns \code{date}, \code{name} and \code{position}.}
#'     \item{\code{data}: a tibble containing the original dataset used for factor construction.}
#'     \item{\code{params}: a tibble containing the original parameters supplied for factor construction.}
#'   }
setGeneric("market_factor",
           function(data, return_frequency, long, geometric) standardGeneric("market_factor"))
