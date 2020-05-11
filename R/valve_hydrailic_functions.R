#' @title Flow coefficient Kv of the Value
#' @description The valve flow coefficient Kv is defined as the number
#'   of cubic meters per hour of 5°C to 30°C water that will flow through
#'   a control valve at a specified position of the control valve (travel) h
#'   with a differential pressure Delta P (p1-p2) of 1bar  (105 Pa)
#'   across it.
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param dn diameter in meter [m]
#' @param zeta dimensionless quantity
#'
#' @return kv value in m³/h
#' @export
#'
#' @examples
#' kv_value( dn = 0.5, zeta = 1.9)

  kv_value <- function(dn, zeta){
    ((dn*1000)^2)/sqrt(626.3*zeta)
  }

#' @title  Resistance Coefficient "_Zeta value_"
#' @description Pressure drop or head loss is proportional to the velocity
#'   in valves or fittings. For the most engineering practices it can be assumed
#'   that pressure drop or head loss due to flow of fluids in turbulent range
#'   through valves and fittings is proportional to square of velocity.
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param kv Kv value
#' @param dn diameter in meter (m)
#'
#' @return Zeta Vaule
#'
#' @export
#'
#' @examples
#' zeta_vaule( dn =0.5, kv = 7247.229)
#'
  zeta_vaule  <- function(dn, kv){
    (1/626.3)*((dn*1000)^2/kv)^2
  }

#' @title Vapour pressure of water
#'
#' @description The vapour pressure of water is the pressure at which water vapour is in
#' thermodynamic equilibrium with its condensed state. At higher pressures water
#' would condense. The water vapour pressure is the partial pressure of water
#' vapour in any gas mixture in equilibrium with solid or liquid water.
#' As for other substances, water vapour pressure is a function of temperature
#' and can be determined with the Clausius–Clapeyron relation.
#' Approximation formula :
#' The Buck equation. where T is in °C and P is in kPa.
#' https://en.wikipedia.org/wiki/Vapour_pressure_of_water
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param temp is in °C
#'
#' @return Vapour pressure of water in (kPa)
#' @export
#'
#' @examples
#' vapour_pressure(25)

  vapour_pressure <- function(temp){
    0.61121*exp((18.678-temp/234.5)*(temp/(257.14+temp)))
  }

#' @title Barometric formula (Atm. Pressure)
#'
#' @description The barometric formula, sometimes called the exponential atmosphere or
#' isothermal atmosphere, is a formula used to model how the pressure
#' or density of the air changes with altitude. The pressure drops
#' approximately by 11.3 Pa per meter in first 1000 meters above sea level.
#' 1 Kilopascals (kPa)	=	0.101972 Meters of Water (mH2O)
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param masl metres above sea level [m]
#' @return Atmospheric pressure in Meters of Water (mH2O)
#'
#' @export
#'
#' @examples
#' atm_pressure(2600)

  atm_pressure  <- function(masl) {
    0.101972*101325*exp(-(0.02896*9.807)/(8.3143*288.15)*masl)
  }

