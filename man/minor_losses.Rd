% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydrailic_functions.R
\name{minor_losses}
\alias{minor_losses}
\title{Minor Losses}
\usage{
minor_losses(flow, dn, zeta)
}
\arguments{
\item{flow}{cubic meter per second (m3/s)}

\item{dn}{inner diameter of pipe  (m)}

\item{zeta}{loss coefficient}
}
\value{
minor losses in meter
}
\description{
Although they often account for a major portion of the head loss,
especially in process piping, the additional losses due to entries and exits,
fittings and valves are traditionally referred to as minor losses.
These losses represent additional energy dissipation in the flow, usually
caused by secondary flows induced by curvature or recirculation.
The minor losses are any head loss present in addition to the head loss for
the same length of straight pipe.
allows for easy integration of minor losses into the Darcy-Weisbach equation.
Zeta is the sum of all of the loss coefficients in the length of pipe, each
contributing to the overall head loss. Although Zeta appears to be a constant
coefficient, it varies with different flow conditions.
Factors affecting the value of K include: the exact geometry of the component
in question; the flow Reynolds Number; proximity to other fittings, etc.
(Tabulated values of K are for components in isolation - with long straight
 runs of pipe upstream and downstream.)
}
\examples{
 minor_losses( flow = 0.042,
                dn = 0.150,
                zeta = 3)
}
