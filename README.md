factor’em\!
================

<style> body {text-align: justify} </style>

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

[factorem](https://bautheac.github.io/factorem/) belongs to the
[finRes](https://bautheac.github.io/finRes/) suite where it facilitates
asset pricing research and factor investment back-testing. The package
is organised around a workhorse function and a series of wrappers for
asset pricing factors popular in the literature. These functions get raw
financial data retrieved from Bloomberg using the
[pullit](https://bautheac.github.io/pullit/) package and return S4
objects that carry data belonging to the corresponding factor including
positions and return time series.  
See the eponym vignette for details: `vignette("factorem", package =
"factorem")`.  
Install the development version from github with
`devtools::install_github("bautheac/factorem")`.
