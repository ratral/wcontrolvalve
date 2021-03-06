% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/valve_hydrailic_functions.R
\name{fp}
\alias{fp}
\title{Fp Piping geometry factor}
\usage{
fp(kv, dn, d1, d2)
}
\arguments{
\item{kv}{Flow coefficient in m³/h}

\item{dn}{diameter in millimeter (mm).}

\item{d1}{Inlet diameter reducer only in millimeter (mm).}

\item{d2}{Outlet diameter increase only in millimeter (mm).}
}
\value{
Fp Piping geometry factor, dimensionless
}
\description{
The piping geometry factor Fp accounts for fittings attached to
either the valve inlet or the outlet that disturb the flow to the extent that
valve capacity is affected. Fp is actually the ratio of the flow coefficient
of a valve with attached fittings to the flow coefficient (kv) of a valve
installed in a straight pipe of the same size as the valve.
}
\examples{
fp(kv = 7247.229, dn =0.5, d1 = 0.6, d2 = 0.6)

}
