
#' @title  Velocity in a pipe
#'
#' @description This function calculates the velocity of the fluid in a
#'   circular pipe.
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param flow cubic meter per second [m³/s]
#' @param dn diameter in meter [m]
#'
#' @return velocity in meter per second [m/s]
#' @export
#'
#' @examples
#' velocity(1, 500/1000)

  velocity <- function(flow, dn){
    flow/((pi*dn^2)/4)
  }

#' @title Kv Value
#' @description Kv Value
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param dn diameter in meter [m]
#' @param zeta dimensionless quantity
#'
#' @return kv value
#' @export
#'
#' @examples
#'

  kv_value <- function(dn, zeta){
    ((dn*1000)^2)/sqrt(626.3*zeta)
  }



#' @title  Zeta Value
#' @description Zeta Value
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
#'
  zeta_vaule  <- function(kv,dn){
    (1/626.3)*((dn*1000)^2/kv)^2
  }


#' @title  Reynolds number
#'
#' @description The Reynolds number (Re) is an important dimensionless quantity in fluid
#' mechanics used to help predict flow patterns in different fluid flow situations.
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param flow cubic meter per second (m³/s)
#' @param dn diameter in meter (m)
#' @param temp temp is in °C
#'
#' @return reynolds number dimensionless quantity
#' @export
#'
#' @examples
#'
#'
  reynolds_number  <- function(flow,dn,temp=20){
    (4*flow)/(pi*dn*kinematic_viscosity(temp))
  }

#' @title Friction in Pipe
#' @description Friction calculation
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param flow cubic meter per second (m³/s)
#' @param dn diameter in meter (m)
#' @param roughness in (mm)
#' @param temp temp is in °C
#'
#' @return Friction value
#'
#' @export
#'
#' @examples
#'
#'
  friction_zigrang <- function(flow, dn, roughness, temp=20){
    re <- reynolds_number(flow,dn,temp)
    (-2*log((roughness/3.7)-(5.02/re)*log(roughness-(5.02/re)*log(roughness/3.7+13/re))))^(-2)
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


#' @title Water Dynamic Viscosity
#'
#' @description Viscosity of the fluid is the internal resistance to flow. Fluid experiences
#' resistance over each layer of it when subjected to deformation.
#' viscosity is the measure of a fluid's resistance to flow
#' Dynamic vsicosity is the force required to maintain the fluid flow at a
#' particular rate. Dynamic viscosity is usually used when the fluid is
#' subjected to any external force. The unit of dynamic viscosit is Pa.s
#' where Pa is N/m^2 - SI unit of pressure
#' http://ddbonline.ddbst.de/VogelCalculation/VogelCalculationCGI.exe
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#' @param temp is in °C
#'
#' @return Dynamic Viscosity (mPa*s)
#' @export
#'
#' @examples
#' dynamic_viscosity(25)
#'
  dynamic_viscosity <- function(temp){
    a = -3.7188
    b =  578.919
    c = -137.546
    temp = temp + 273.15
    exp(a+b/(c+temp))
  }


#' @title Saturated water Density
#'
#' @description The density of water is about 1 gram per cubic centimetre (62 lb/cu ft):
#' this relationship was originally used to define the gram.
#' The density varies with temperature, but not linearly:
#' as the temperature increases, the density rises to a peak at 3.98 °C (39.16 °F)
#' and then decreases.
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param temp  is in °C
#'
#' @return density of water in (kg/m³)
#'
#' @export
#'
#' @examples
#' water_density(25)
#'
  water_density <- function(temp){
    a <- 0.14395
    b <- 0.0112
    c <- 649.727
    d <- 0.05107
    temp = temp + 273.15
    a/(b^(1+(1-temp/c)^d))
  }


#' @title Kinematic Viscosity
#'
#' @description Kinematic viscosity is the ratio of - absolute (or dynamic) viscosity
#' to density - a quantity in which no force is involved.
#' The kinematic viscosity ν is simply the dynamic viscosity μ
#' divided by the fluid density ρ.
#' The units of μ are pascals-seconds (kg/m.s) vs. the units of ν which are m2/s.
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param temp  is in °C
#'
#' @return Kinematic Viscosity in (mm2/s)
#' @export
#'
#' @examples
#' kinematic_viscosity(25)
#'
  kinematic_viscosity <- function(temp){
    dynamic_viscosity(temp)/(water_density(temp)/1000)
  }
