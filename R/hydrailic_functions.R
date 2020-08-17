
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
  reynolds_number  <- function(flow,dn,temp = 15.6){
    reynolds <- (4*flow)/(pi*dn*kinematic_viscosity(temp)*1e-6)
    return(reynolds)
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
#' @examples
#' friction_colebrook(flow = 0.042, dn = 0.150, roughness = 1.5e-6, temp = 14.5)
#'
  friction_colebrook <- function(flow, dn, roughness, temp = 15.6){
    re <- reynolds_number(flow,dn,temp)
    r_roughness <- roughness/dn
    (-2*log10((r_roughness/3.7)-(5.02/re)*log10(r_roughness-(5.02/re)*log10(r_roughness/3.7+13/re))))^(-2)
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
    darcy_weisbach <- function(flow, pipe_length, dn, roughness, temp = 15.6){

      friction <- friction_colebrook (flow, dn, roughness, temp)
      v <- velocity(flow, dn)
      darcy <- friction*(pipe_length/dn)*((v^2)/(2*9.807))
      return(darcy)
    }

