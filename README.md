[![Travis-CI Build Status](https://travis-ci.org/bautheac/factorem.svg?branch=master)](https://travis-ci.org/bautheac/factorem)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bautheac/factorem?branch=master&svg=true)](https://ci.appveyor.com/project/bautheac/factorem)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)


# factor'em!

Construct classic asset pricing factors.


## Installation

Install the development version from [github](https://github.com/bautheac/factorem/) with:

``` r

devtools::install_github(repo = "factorem", username = "bautheac")

```


## Example

Commodity nearby futures momentum factor:

``` r

library(finRes)

GSCI_commos <- c("W A Comdty", "KWA Comdty", "C A Comdty", "S A Comdty", "KCA Comdty", 
             "SBA Comdty", "CCA Comdty", "CTA Comdty",
             "LHA Comdty", "LCA Comdty", "FCA Comdty",
             "CLA Comdty", "HOA Comdty", "XBA Comdty", "COA Comdty", "QSA Comdty", "NGA Comdty",
             "LAA Comdty", "LPA Comdty", "LNA Comdty", "LLA Comdty", "LXA Comdty", "GCA Comdty", "SIA Comdty")

term_structure <- BBG_futures_market(type = 'term structure', 
  active_contract_tickers = GSCI_commos, 
  start = "2007-01-01", end = as.character(Sys.Date()), 
  TS_positions = 1L, 
  roll_type = "A", roll_days = 0L,  roll_months = 0L, roll_adjustment = "N")

momentum <- momentum_factor(term_structure)

```




