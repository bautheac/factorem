% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/methods.r
\docType{methods}
\name{get_parameters}
\alias{get_parameters}
\alias{get_parameters,AssetPricingFactor-method}
\alias{get_parameters,AssetPricingFactor}
\title{Accesses the \code{parameters} slot of S4 objects of class \linkS4class{AssetPricingFactor}
  (\href{https://github.com/bautheac/factorem/}{\pkg{factorem}}).}
\usage{
get_parameters(object)

\S4method{get_parameters}{AssetPricingFactor}(object)
}
\arguments{
\item{object}{an S4 object of class \linkS4class{AssetPricingFactor}.}
}
\value{
A \code{\link[data.table]{data.table}} containing the original parameters
  supplied for factor construction.
}
\description{
Access method for the \code{parameters} slot of S4 objects of class
  \linkS4class{AssetPricingFactor} from the
  \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
}
