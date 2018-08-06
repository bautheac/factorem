
setOldClass(c("tbl_df", "tbl", "data.frame"))

#' S4 class for asset pricing factor object
#'
#' @export
.AssetPricingFactor <- setClass("AssetPricingFactor",
                                slots = list(name = "character",
                                             returns = "tbl_df",
                                             positions = "tbl_df",
                                             data = "tbl_df",
                                             params = "tbl_df"))

#' Show method for S4 object of class `AssetPricingFactor`.
#'
#' @export
setMethod("show", "AssetPricingFactor", function(object) {
  cat(is(object)[[1]], "\n",
      "  name: ", paste(object@name, "factor", " "), "\n",
      "  parameters\n",
      paste0(rep("    ", ncol(object@params)),
             gsub(pattern = "_", replacement = " ", names(object@params)),
             sapply(names(object@params), function(x) if (nchar(x) <= 10L) ":\t\t " else ":\t "),
             object@params[1L, ],
             rep("\n", ncol(object@params))),
      sep = ""
  )
})









setGeneric("name", function(factor) standardGeneric("name"))

#' Accessor method for factor name
#'
#' @param factor An S4 object of class 'AssetPricingFactor'.
#'
#' @return A unit character vector containing the name of the factor.
#'
#' @export
setMethod("name", "AssetPricingFactor", function(factor) factor@name)


setGeneric("returns", function(factor) standardGeneric("returns"))

#' Accessor method for factor returns
#'
#' @param factor An S4 object of class 'AssetPricingFactor'.
#'
#' @return A tibble of factor returns.
#'
#' @export
setMethod("returns", "AssetPricingFactor", function(factor) factor@returns)


setGeneric("positions", function(factor) standardGeneric("positions"))

#' Accessor method for factor positions
#'
#' @param factor An S4 object of class 'AssetPricingFactor'.
#'
#' @return A tibble of factor positions.
#'
#' @export
setMethod("positions", "AssetPricingFactor", function(factor) factor@positions)


setGeneric("data", function(factor) standardGeneric("data"))

#' Accessor method for factor data construction
#'
#' @param factor An S4 object of class 'AssetPricingFactor'.
#'
#' @return A tibble containing the original dataset used for factor construction.
#'
#' @export
setMethod("data", "AssetPricingFactor", function(factor) factor@data)


setGeneric("parameters", function(factor) standardGeneric("parameters"))
#' Accessor method for factor parameters
#'
#' @param factor An S4 object of class 'AssetPricingFactor'.
#'
#' @return A tibble containing the original parameters supplied for factor construction.
#'
#' @export
setMethod("parameters", "AssetPricingFactor", function(factor) factor@params)


setGeneric("plot_performance", function(factor) standardGeneric("plot_performance"))



#' Summary plot of factor performance by leg
#'
#' @param factor An S4 object of class 'AssetPricingFactor'.
#'
#' @return A line plot showing the wealth index for the factor itself as well as that of both long and short legs separately.
#'
#' @importFrom dplyr mutate_at mutate
#' @importFrom ggplot2 aes facet_wrap geom_line ggplot labs theme
#' @importFrom ggthemes theme_tufte
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom tidyr gather
#'
#' @export
setMethod("plot_performance", "AssetPricingFactor", function(factor) {
  data <- factor@returns
  data[, matches("long|short|factor", vars = names(data))] <- data[, matches("long|short|factor", vars = names(data))] + 1L
  data %<>%
    mutate_at(vars(matches("long|short|factor")), funs(c(1L, sapply(2L:NROW(.), function(x) .[x] * .[x - 1L]))))%>%
    gather(leg, `wealth index`, -date) %>%
    mutate(leg = ifelse(leg %in% c("long", "short"), paste(leg, "leg", sep = " "), paste(factor@name, leg, sep = " ")),
           leg = factor(leg, levels = c(paste(factor@name, "factor", sep = " "), "long leg", "short leg")))

  ggplot(data, aes(x = date, y = `wealth index`, colour = leg)) +
    geom_line(size = 1L) +
    geom_line(aes(y = 1L), colour = "black", show.legend = FALSE) +
    theme_tufte() +
    theme(legend.position = "none")+
    labs(x = NULL, y = NULL) +
    facet_wrap(~leg, ncol = 1L)

})




setGeneric("plot_positions", function(factor) standardGeneric("plot_positions"))

#' Summary plot of factor positions by leg
#'
#' @param factor An S4 object of class 'AssetPricingFactor'.
#'
#' @return A bar plot showing the proportion of time individual names appear in the factor.
#'
#' @importFrom dplyr group_by mutate rename tally
#' @importFrom ggplot2 aes element_blank facet_wrap geom_bar ggplot labs scale_x_discrete scale_y_continuous theme
#' @importFrom ggthemes theme_tufte
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr map
#' @importFrom scales percent
#' @importFrom tidyr nest unnest
#'
#' @export
setMethod("plot_positions", "AssetPricingFactor", function(factor) {
  data <- factor@positions
  data %<>%
    group_by(position) %>%
    nest() %>%
    mutate(proportions = map(data, function(x) x %>%
                               group_by(name) %>%
                               tally() %>%
                               mutate(n = n / nrow(x))
    )) %>%
    unnest(proportions) %>%
    rbind(data %>%
            group_by(name) %>%
            tally() %>%
            mutate(position = "factor", n = n / nrow(factor@positions))) %>%
    rename(proportion = n) %>%
    mutate(leg = ifelse(position %in% c("long", "short"), paste(position, "leg", sep = " "), paste(factor@name, position, sep = " ")),
           leg = factor(leg, levels = c(paste(factor@name, "factor", sep = " "), "long leg", "short leg")))

  ggplot(data, aes(name, proportion, fill = name)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(breaks = NULL) +
    scale_y_continuous(labels = percent) +
    facet_wrap(~leg, ncol = 1L) +
    theme_tufte() +
    theme(legend.title = element_blank())

})


#' Summary of factor returns by month and year
#'
#' @param object An S4 object of class 'AssetPricingFactor'.
#' @param leg A character vector. 'long', 'short' and 'factor' display the returns for the long, short and the factor itself respectively.
#'
#' @return A dataframe with months as colums and years as rows. Column total shows the cumulated return for the corresponding year.
#'
#' @importFrom dplyr select rename
#' @importFrom magrittr "%>%" "%<>%" set_colnames
#' @importFrom PerformanceAnalytics table.CalendarReturns
#' @importFrom xts xts
#'
#' @export
setMethod("summary", "AssetPricingFactor", function(object, leg = "factor") {
  returns <- xts(x = object@returns %>%
                   select(return = !! leg),
                 order.by = object@returns$date)

  table.CalendarReturns(returns, digits = 1, as.perc = TRUE, geometric = object@params$geometric) %>%
    rename(total = return) %>%
    set_colnames(value = tolower(names(.)))

})



