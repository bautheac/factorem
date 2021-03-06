% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/methods.r
\docType{methods}
\name{get_positions}
\alias{get_positions}
\alias{get_positions,AssetPricingFactor-method}
\alias{get_positions,AssetPricingFactor}
\title{Accesses the \code{positions} slot of S4 objects of class \linkS4class{AssetPricingFactor}
  (\href{https://github.com/bautheac/factorem/}{\pkg{factorem}}).}
\usage{
get_positions(object)

\S4method{get_positions}{AssetPricingFactor}(object)
}
\arguments{
\item{object}{an S4 object of class \linkS4class{AssetPricingFactor}.}
}
\value{
A \code{\link[data.table]{data.table}} of factor positions.
}
\description{
Access method for the \code{positions} slot of S4 objects of class
  \linkS4class{AssetPricingFactor} from the
  \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
}
