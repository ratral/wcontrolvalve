
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
    if (dn   <= 0.0) {
      stop("\nPositive value needed for diameter\n")
    }
    if (flow <= 0.0) {
      stop("Positive value needed for Flow")
    }
    velocity <- flow/((pi*dn^2)/4)
    return(velocity)
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
  reynolds_number  <- function(flow,dn,temp = 15){

    if (dn <= 0)   stop("Diameter must be greater than 0")
    if (flow <= 0) stop("Flow must be greater than 0")
    if (temp < 0 | temp > 100) {
      stop("\nTemperature outside range for liquid water.\n")
    }

    (4*flow)/(pi*dn*kinematic_viscosity(temp)*1e-6)
  }

#' @title Colebrook–White equation (Darcy friction factor formula)
#' @description The phenomenological Colebrook–White equation
#' (or Colebrook equation) expresses the Darcy friction factor f as a function
#' of Reynolds number Re and pipe relative roughness ε/Dh, fitting the data
#' of experimental studies of turbulent flow in smooth and rough pipes.
#' The equation can be used to (iteratively) solve for the Darcy–Weisbach
#' friction factor f.
#' [https://en.wikipedia.org/wiki/Darcy_friction_factor_formulae#Colebrook%E2%80%93White_equation]
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
  friction_colebrook <- function(flow, dn, roughness, temp = 15){

    if (roughness <= 0) stop("Roughness must be greater than 0")
    if (dn <= 0)        stop("Diameter must be greater than 0")
    if (flow <= 0)      stop("Flow must be greater than 0")
    if (temp < 0 | temp > 100) {
      stop("\nTemperature outside range for liquid water.\n")
    }

    re <- reynolds_number(flow,dn,temp)
    r_roughness <- roughness/dn

    if (roughness/dn > 0.01) {
      stop(sprintf("ks/dn: %.4f value > 0.01, outside applicable range\n", roughness/dn))
    }

    if (re <= 2200 ) {
      return(64/re)
    } else {
      f <- function(x){-2*log10(r_roughness/3.7 + 2.51/(re*sqrt(x)))-1/sqrt(x)}
      root <- uniroot(f, lower = 0.01, upper = 0.08, tol = 1e-10)$root
      return(root)
    }
  }

#' @title Darcy–Weisbach equation
#' @description In fluid dynamics, the Darcy–Weisbach equation is an empirical
#' equation, which relates the head loss, or pressure loss, due to friction
#' along a given length of pipe to the average velocity of the fluid flow
#' for an incompressible fluid.
#' The Darcy–Weisbach equation contains a dimensionless friction factor,
#' known as the Darcy friction factor. This is also variously called the
#' Darcy–Weisbach friction factor, friction factor, resistance coefficient,
#' or flow coefficient.
#' [https://en.wikipedia.org/wiki/Darcy%E2%80%93Weisbach_equation]
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
#' darcy_weisbach( flow = 0.042,
#'                 pipe_length = 970,
#'                 dn = 0.150,
#'                 roughness = 1.5e-6,
#'                 temp = 14.5)
#'
    darcy_weisbach <- function(flow, pipe_length, dn, roughness, temp = 15){

      if (pipe_length <= 0) stop("Pipe_length must be greater than 0")
      if (roughness <= 0)   stop("Roughness must be greater than 0")
      if (dn <= 0)          stop("Diameter must be greater than 0")
      if (flow <= 0)        stop("Flow must be greater than 0")
      if (temp < 0 | temp > 100) {
        stop("\nTemperature outside range for liquid water.\n")
      }


      friction <- friction_colebrook (flow, dn, roughness, temp)
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
#' https://en.wikipedia.org/wiki/Temperature_dependence_of_viscosity
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#' @param temp is in °C
#'
#' @return Dynamic Viscosity (mPa*s)
#' @export
#'
#' @examples
#' dynamic_viscosity(14.5)
#'
    dynamic_viscosity <- function(temp = 15){

      if (temp < 0 | temp > 100) {
        stop("\nTemperature outside range for liquid water.\n")
      }

      a <- 1.856e-11
      b <- 4209
      c <- 0.04527
      d <- -3.376e-05
      temp <- temp + 273.15  #T must be in K for approximation
      visc <- a * exp(b/temp + c * temp + d * temp^2)  # /1000 converts from  mPa·s to Pa-s (N s m-2)
}

#' @title Saturated water Density
#'
#' @description The density of water is about 1 gram per cubic centimetre (62 lb/cu ft):
#' this relationship was originally used to define the gram.
#' The density varies with temperature, but not linearly:
#' as the temperature increases, the density rises to a peak at 3.98 °C (39.16 °F)
#' and then decreases.
#' http://ddbonline.ddbst.de/DIPPR105DensityCalculation/DIPPR105CalculationCGI.exe
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
#' water_density(14.5)
#'
  water_density <- function(temp = 15){

    if (temp < 0 | temp > 100) {
      stop("\nTemperature outside range for liquid water.\n")
    }

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
#' @return Kinematic Viscosity in (m2/s)*1e-6
#' @export
#'
#' @examples
#' kinematic_viscosity(14.5)
#'
    kinematic_viscosity <- function(temp = 15){

      if (temp < 0 | temp > 100) {
        stop("\nTemperature outside range for liquid water.\n")
      }

      dynamic_viscosity(temp)/(water_density(temp)/1000)
   }
