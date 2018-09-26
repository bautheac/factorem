# if(getRversion() >= "2.15.1")
#   utils::globalVariables(c(".", "active contract ticker", "average", "back", "tickers_cftc", "data", "date", "field", "front", "inverse CHP", "leg", "long",
#                            "long_threshold",
#                            "n", "name", "new", "params", "participant", "period", "position", "proportion", "PX_LAST", "ranking_period", "returns", "roll yield",
#                            "short", "short_threshold", "ticker", "total", "TS position", "underlying", "update_frequency",
#                            "unit", "value", "wealth index"), add = FALSE)

setOldClass(c("tbl_df", "tbl", "data.frame"))

#' S4 class for asset pricing factor objects
#' @export
setClass("AssetPricingFactor",
         representation(name = "character", returns = "data.table", positions = "data.table", data = "data.table", parameters = "tbl_df", call = "call"))

#' S4 class for commercial hedging pressure (CHP) factor objects
#' @export
setClass("CHPFactor", contains = "AssetPricingFactor")

#' S4 class for open interest (OI) factor objects
#' @export
setClass("OIFactor", contains = "AssetPricingFactor")

#' S4 class for momentum factor objects
#' @export
setClass("MomentumFactor", contains = "AssetPricingFactor")

#' S4 class for term structure factor objects
#' @export
setClass("TSFactor", contains = "AssetPricingFactor")

#' S4 class for term structure factor objects
#' @export
setClass("MarketFactor", contains = "AssetPricingFactor")
