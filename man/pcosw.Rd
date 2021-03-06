% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcosw.R
\name{pcosw}
\alias{pcosw}
\title{The cumulative density function of the CosWeibull probability distribution.}
\usage{
pcosw(q, alpha, lambda, lower = TRUE, log.p = FALSE)
}
\arguments{
\item{q}{Vector of quantiles.}

\item{alpha}{Alpha parameter.}

\item{lambda}{Lambda parameter.}

\item{lower}{Lower parameter.}

\item{log.p}{Log.p parameter.}
}
\value{
A vector with n observations of the CosWeibull distribution.
}
\description{
The cumulative density function of the CosWeibull probability distribution.
}
\examples{
pcosw(0.5,1,1,lower = TRUE,log.p = FALSE)
pcosw(0.5,1.5,1,lower = TRUE,log.p = FALSE)
}

