if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "back", "data", "front", "leg", "long_threshold", "n", "name", "new", "params", "position", "positions",
                                                        "proportion", "ranking_period", "returns", "short_threshold", "ticker", "total", "update_frequency", "value",
                                                        "wealth index"))

setOldClass(c("tbl_df", "tbl", "data.frame"))


#' S4 class for asset pricing factor objects
#' @export
setClass("AssetPricingFactor",
         representation(name = "character", returns = "tbl_df", positions = "tbl_df", data = "tbl_df", params = "tbl_df"))

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
