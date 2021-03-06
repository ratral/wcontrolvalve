% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DoseResponse.R
\name{inv_LL3}
\alias{inv_LL3}
\title{Inverse of the dose-response models.}
\usage{
inv_LL3(kv_kvs, b, d, e)
}
\arguments{
\item{kv_kvs}{valve position}

\item{b}{steepness}

\item{d}{upper value}

\item{e}{the effective dose}
}
\value{
position
}
\description{
Built-in the inverse of dose-response models.
}
\examples{
inv_LL3(0.4567872, -2.39, 1.39, 67.42 )
}
