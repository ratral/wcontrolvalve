
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
#' velocity(flow = 0.4, dn = 0.2)

  velocity <- function(flow, dn){
    flow/((pi*dn^2)/4)
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
#' reynolds_number(flow = 0.157, dn = 0.3,   temp = 20)
#' reynolds_number(flow = 3.72,  dn = 1.2,   temp = 14)
#' reynolds_number(flow = 0.042, dn = 0.150, temp = 14.5)
#'
  reynolds_number  <- function(flow,dn,temp=20){
    (4*flow)/(pi*dn*kinematic_viscosity(temp))
  }

#' @title Colebrook–White equation (Darcy friction factor formula)
#' @description The phenomenological Colebrook–White equation
#'   (or Colebrook equation) expresses the Darcy friction factor f as a function
#'   of Reynolds number Re and pipe relative roughness ε/Dh, fitting the data
#'   of experimental studies of turbulent flow in smooth and rough pipes.
#'   The equation can be used to (iteratively) solve for the Darcy–Weisbach
#'   friction factor f.
#'   [https://en.wikipedia.org/wiki/Darcy_friction_factor_formulae#Colebrook%E2%80%93White_equation]
#'
#' @param flow cubic meter per second (m³/s)
#' @param dn diameter in meter (m)
#' @param roughness roughness in (m)
#' @param temp temp is in °C
#'
#' @return the output from \code{\link{uniroot}} for the calculation of the
#'   root from the ecuation of Colebrook-White
#' @export
#' @importFrom stats uniroot
#' @examples
#' friction_colebrook(flow = 0.042, dn = 0.150, roughness = 1.5e-6, temp = 14.5)
#'
  friction_colebrook <- function(flow, dn, roughness, temp=20){
    re <- reynolds_number(flow,dn,temp)
    r_roughness <- roughness/dn
    f <- function(x){-2*log10(r_roughness/3.7 + 2.51/(re*sqrt(x)))-1/sqrt(x)}
    root <- uniroot(f, lower = 0, upper = 0.1)$root
    return(root)
  }


#' @title Friction loss in Pipe (Zigrand and Sylvester function)
#' @description In fluid flow, friction loss (or skin friction) is the loss of
#'    pressure or “head” that occurs in pipe or duct flow due to the effect of
#'    the fluid's viscosity near the surface of the pipe or duct.
#'    [https://en.wikipedia.org/wiki/Friction_loss]
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param flow cubic meter per second (m³/s)
#' @param dn diameter in meter (m)
#' @param roughness in (m)
#' @param temp temp is in °C
#'
#' @return Solve the ecuation of Colebrook-White with the ecuaton of
#'   Zigrand and Sylvester (1982)
#'
#' @export
#'
#' @examples
#' friction_zigrang(flow = 0.042, dn = 0.150, roughness = 1.5e-6, temp = 14.5)


  friction_zigrang <- function(flow, dn, roughness, temp=20){
    re <- reynolds_number(flow,dn,temp)
    r_roughness <- roughness/dn
    (-2*log10((r_roughness/3.7)-(5.02/re)*log10(r_roughness-(5.02/re)*log10(r_roughness/3.7+13/re))))^(-2)
  }

#' @title Friction loss in Pipe (Swanee-Jain)
#' @description In fluid flow, friction loss (or skin friction) is the loss of
#'    pressure or “head” that occurs in pipe or duct flow due to the effect of
#'    the fluid's viscosity near the surface of the pipe or duct.
#'    [https://en.wikipedia.org/wiki/Friction_loss]
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#' @param flow cubic meter per second (m³/s)
#' @param dn diameter in meter (m)
#' @param roughness internal roughness of the pipe in (m)
#' @param temp temperature is in °C
#'
#' @return Solve the ecuation of Colebrook-White with the ecuaton of
#'   Swanee-Jain (1976)
#' @export
#'
#' @examples
#' friction_swamee(flow = 0.042, dn = 0.150, roughness = 1.5e-6, temp = 14.5)
#'
  friction_swamee <- function(flow, dn, roughness, temp=20){
    re <- reynolds_number(flow,dn,temp)
    r_roughness <- roughness/dn
    (-2*log10(r_roughness/3.7+5.74/re^(0.9)))^(-2)
  }


#' @title Darcy–Weisbach equation
#' @description In fluid dynamics, the Darcy–Weisbach equation is an empirical
#'    equation, which relates the head loss, or pressure loss, due to friction
#'    along a given length of pipe to the average velocity of the fluid flow
#'    for an incompressible fluid.
#'    The Darcy–Weisbach equation contains a dimensionless friction factor,
#'    known as the Darcy friction factor. This is also variously called the
#'    Darcy–Weisbach friction factor, friction factor, resistance coefficient,
#'    or flow coefficient.
#'    [https://en.wikipedia.org/wiki/Darcy%E2%80%93Weisbach_equation]
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#' @param flow cubic meter per second (m³/s)
#' @param pipe_length length of pipe  (m)
#' @param dn inner diameter of pipe  (m)
#' @param roughness internal roughness of the pipe in (m)
#' @param temp temperature in °C
#'
#' @return head loss (m)
#' @export
#'
#' @examples
#' darcy_weisbach(flow = 0.4, pipe_length = 100,
#'                dn = 0.2, roughness = 1.5e-4, temp = 15)
#'
    darcy_weisbach <- function(flow, pipe_length, dn, roughness, temp=20){
      friction <- friction_zigrang (flow, dn, roughness, temp)
      v <- velocity(flow, dn)
      friction*(pipe_length/dn)*((v^2)/(2*9.807))
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
#' dynamic_viscosity(15)
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
#' water_density(15)
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
#' @return Kinematic Viscosity in (m2/s)
#' @export
#'
#' @examples
#' kinematic_viscosity(15)
#' kinematic_viscosity(14.5)
#'
    kinematic_viscosity <- function(temp){
      dynamic_viscosity(temp)/(water_density(temp)/1000)*1e-6
   }
