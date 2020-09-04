
#' @title Vapour pressure of water
#' @description The vapor pressure of water is the pressure at which water vapor is in
#' thermodynamic equilibrium with its condensed state. At higher pressures water
#' would condense. The water vapor pressure is the partial pressure of water
#' vapor in any gas mixture in equilibrium with solid or liquid water.
#' As for other substances, water vapor pressure is a function of temperature
#' and can be determined with the Clausius–Clapeyron relation.
#' Approximation formula :
#' The Buck equation. where T is in °C and P is in kPa.
#' https://en.wikipedia.org/wiki/Vapour_pressure_of_water
#' @author Dr. Raúl Trujillo Álvarez
#' @param temp is in °C
#' @return Vapour pressure of water in (bar)
#' @export
#' @examples
#' vapour_pressure(25)

vapour_pressure <- function(temp = 15){
  pv <- 0.61121*exp((18.678-temp/234.5)*(temp/(257.14+temp)))
  return(pv/100)
}

#' @title Barometric formula (Atm. Pressure)
#' @description The barometric formula, sometimes called the exponential atmosphere or
#' isothermal atmosphere, is a formula used to model how the pressure
#' or density of the air changes with altitude. The pressure drops
#' approximately by 11.3 Pa per meter in first 1000 meters above sea level.
#' 1 Kilopascals (kPa)	=	0.01 bar
#' https://www.math24.net/barometric-formula/
#' @author Dr. Raúl Trujillo Álvarez
#' @param masl metres above sea level [m]
#' @return Atmospheric pressure in bar
#' @export
#' @examples
#' atm_pressure(2600)

atm_pressure  <- function(masl = 0 ) {
  p_at <- 101.325*exp(-0.000118547*masl)*0.01
  return(p_at)
}

#' @title Water Dynamic Viscosity
#' @description Viscosity of the fluid is the internal resistance to flow. Fluid experiences
#' resistance over each layer of it when subjected to deformation.
#' viscosity is the measure of a fluid's resistance to flow
#' Dynamic vsicosity is the force required to maintain the fluid flow at a
#' particular rate. Dynamic viscosity is usually used when the fluid is
#' subjected to any external force. The unit of dynamic viscosit is Pa.s
#' where Pa is N/m^2 - SI unit of pressure
#' https://en.wikipedia.org/wiki/Temperature_dependence_of_viscosity
#' @author Dr. Raúl Trujillo Álvarez
#' @param temp is in °C
#' @return Dynamic Viscosity (mPa*s)
#' @export
#' @examples
#' dynamic_viscosity(14.5)
#'
dynamic_viscosity <- function(temp = 15.6){
  a <- 1.856e-11
  b <- 4209
  c <- 0.04527
  d <- -3.376e-05
  temp <- temp + 273.15  #T must be in K for approximation
  visc <- a * exp(b/temp + c * temp + d * temp^2)  # /1000 converts from  mPa·s to Pa-s (N s m-2)
  return(visc)
}

#' @title Saturated water Density
#' @description The density of water is about 1 gram per cubic centimetre (62 lb/cu ft):
#' this relationship was originally used to define the gram.
#' The density varies with temperature, but not linearly:
#' as the temperature increases, the density rises to a peak at 3.98 °C (39.16 °F)
#' and then decreases.
#' http://ddbonline.ddbst.de/DIPPR105DensityCalculation/DIPPR105CalculationCGI.exe
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#' @param temp  is in °C
#' @return density of water in (kg/m³)
#' @export
#' @examples
#' water_density(14.5)
#'
water_density <- function(temp = 15.6){
  a <- 0.14395
  b <- 0.0112
  c <- 649.727
  d <- 0.05107
  temp = temp + 273.15
  density <- a/(b^(1+(1-temp/c)^d))
  return(density)
}


#' @title Kinematic Viscosity
#' @description Kinematic viscosity is the ratio of - absolute (or dynamic) viscosity
#' to density - a quantity in which no force is involved.
#' The kinematic viscosity ν is simply the dynamic viscosity μ
#' divided by the fluid density ρ.
#' The units of μ are pascals-seconds (kg/m.s) vs. the units of ν which are m2/s.
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#' @param temp  is in °C
#' @return Kinematic Viscosity in (m2/s)*1e-6
#' @export
#' @examples
#' kinematic_viscosity(14.5)
#'
kinematic_viscosity <- function(temp = 15.6){
  k_viscosity <- dynamic_viscosity(temp)/(water_density(temp)/1000)
  return(k_viscosity)
}
