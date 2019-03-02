factor’em\!
================

<style> body {text-align: justify} </style>

[![Travis-CI Build
Status](https://travis-ci.org/bautheac/factorem.svg?branch=master)](https://travis-ci.org/bautheac/factorem)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/factorem/pullit?branch=master&svg=true)](https://ci.appveyor.com/project/factorem/pullit)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## factor’em\!

factorem belongs to the [finRes](https://bautheac.github.io/finRes/)
suite where it facilitates asset pricing research and factor investment
backtesting.  
The package is organised around a workhorse function and a series of
wrappers for asset pricing factors popular in the literature. These
functions get raw financial data retrieved from Bloomberg using the
[pullit](https://bautheac.github.io/pullit/) package and return S4
objects that carry data belonging to the corresponding factor including
positions and return time series. See the eponym vignette for details:
`vignette("factorem", package = "factorem")`.
