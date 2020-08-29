
#' @title Reynolds number factor for valve sizing.
#'
#' @description A \code{tibble} containing the values for the calculation and plot
#' of the Valve Reynolds Number Factor. The table  show the relationships between
#' FR and the valve Reynolds number Rev for the three types of problems that
#' may be encountered with viscous flow. These are:
#' a) Determining the required flow coefficient when selecting a control valve size
#' b) Predicting the flow rate that a selected valve will pass
#' c) Predicting the pressure differential that a selected valve will exhibit
#'
#' @format A \code{tibble} with 265 rows and 4 variables:
#' \describe{
#'   \item{problem_typ}{types of problems: "selection", "flow", "pressure"}
#'   \item{reynolds}{Valve Reynolds Number}
#'   \item{fr}{Valve Reynolds number factor}
#' }
#' @source ISA–75.01–1985 (R1995) "Flow Equations for Sizing Control Valves"
"reynolds_factor"
