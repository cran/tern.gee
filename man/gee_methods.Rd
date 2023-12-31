% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_methods.R
\name{gee_methods}
\alias{gee_methods}
\alias{VarCorr.tern_gee}
\alias{QIC.tern_gee}
\title{Methods for GEE Models}
\usage{
\method{VarCorr}{tern_gee}(x, sigma = 1, ...)

\method{QIC}{tern_gee}(object, ...)
}
\arguments{
\item{x}{(\code{tern_gee})\cr result of \code{\link[=fit_gee]{fit_gee()}}.}

\item{sigma}{an optional numeric value used as a multiplier for the
    standard deviations.  The default is \code{x$sigma} or \code{1}
    depending on \code{\link{class}(x)}.}

\item{...}{further optional arguments passed to other methods (none
    for the methods documented here).}

\item{object}{(\code{tern_gee})\cr result of \code{\link[=fit_gee]{fit_gee()}}.}
}
\value{
\code{VarCorr()} returns the estimated covariance matrix, and
\code{QIC()} returns the QIC value.
}
\description{
Additional methods which can simplify working with the GEE result object.
}
