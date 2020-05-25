#' @title Flow coefficient Kv Value in function of the Zeta  Value
#' @description The valve flow coefficient Kv is defined as the number
#' of cubic meters per hour of 5°C to 30°C water that will flow through
#' a control valve at a specified position of the control valve (travel) h
#' with a differential pressure Delta P (p1-p2) of 1bar  (105 Pa)
#' across it.
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
    if (dn <= 0)   stop("Diameter must be greater than 0")
    if (zeta <= 0) stop("Zeta value must be greater than 0")
    ((dn*1000)^2)/sqrt(626.3*zeta)
  }

#' @title  Resistance Coefficient Zeta in function of Kv
#' @description Pressure drop or head loss is proportional to the velocity
#' in valves or fittings. For the most engineering practices it can be assumed
#' that pressure drop or head loss due to flow of fluids in turbulent range
#' through valves and fittings is proportional to square of velocity.
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
    if (dn <= 0) stop("Diameter must be greater than 0")
    if (kv <= 0) stop("kv value must be greater than 0")
    (1/626.3)*((dn*1000)^2/kv)^2
  }


#' @title Liquid critical pressure ratio factor
#' @description FF is the liquid critical pressure ratio factor. This factor is
#' the ratio of the apparent vena contracta pressure at choked flow conditions
#' to the vapor pressure of the liquid at inlet temperature. [ISA-75.01.01-2007]
#' At vapor pressures near zero, this factor is 0.96.

#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#' @param temp is in °C
#'
#' @return Liquid critical pressure ratio factor Dimensionless
#' @export
#'
#' @examples
#' ff(15)
#'
  ff <- function(temp = 15){
    pv <- vapour_pressure(temp) * 0.01
    #  the critical thermodynamic pressure for water is 221.2 bar
    pc <- 221.2 # Thermodynamic critical pressure of water in kPa
    return(0.96-0.28*sqrt(pv/pc))
  }


#' @title Piping geometry factor
#' @description The piping geometry factor Fp accounts for fittings attached to
#' either the valve inlet or the outlet that disturb the flow to the extent that
#' valve capacity is affected. Fp is actually the ratio of the flow coefficient
#' of a valve with attached fittings to the flow coefficient (kv) of a valve
#' installed in a straight pipe of the same size as the valve.
#'
#' @param kv Flow coefficient in m³/h
#' @param dn diameter in meter [m]
#' @param d1 Inlet diameter reducer only in meter [m]
#' @param d2 Outlet diameter increaser only in meter [m]
#'
#' @return Piping geometry factor, dimensionless
#' @export
#'
#' @examples
#' fp(kv = 7247.229, dn =0.5, d1 = 0.6, d2 = 0.6)
#'
  fp <- function(kv, dn, d1, d2){
    z1 <- 0.5*(1-(dn/d1)^2)^2
    z2 <- (1-(dn/d2)^2)^2
    z  <- z1 + z2
    return(1 / sqrt(1+(z*(kv/dn^2)^2)/0.0016))
  }


#' @title Combined liquid pressure recovery factor flp
#' @description When a valve is installed with reducers or other attached
#' fittings, the liquid pressure recovery of the valve-fitting combination is
#' not the same as that for the valve alone. For calculations involving choked
#' flow, it is convenient to treat the piping geometry factor (fp) and the fl
#' factor for the valve-fitting combination as a single factor, flp.
#'
#' @param kv Flow coefficient in m³/h
#' @param fl Liquid pressure recovery factor of a control valve without attached fittings
#' @param dn diameter in meter [m]
#' @param d1 Inlet diameter reducer only in meter [m]
#' @param d2 Outlet diameter increaser only in meter [m]
#'
#' @return Product of the liquid pressure recovery factor of a valve with
#' attached fittings (no symbol has been identified) and the piping geometry
#' factor, dimensionless.
#' @export
#'
#' @examples
#' flp(kv = 7247.229, fl = 0.9, dn =0.5, d1 = 0.6, d2 = 0.6)
#'
  flp <- function(kv, fl, dn, d1, d2){
    z1 <- 0.5*(1-(dn/d1)^2)^2
    z2 <- (1-(dn/d2)^2)^2
    z  <- z1 + z2
    return(fl / sqrt(1+(z*(kv/dn^2)^2)*(fl^2)/0.0016))
  }


#' @title Type of flow
#' @description Tested ob the flow is choked or not
#'
#' @param fl Liquid pressure recovery factor of a control valve without attached fittings
#' @param p1 Inlet Absolute pressure
#' @param p2 Outlet Absolute pressure
#' @param temp Inlet water temperature in °C
#'
#' @return chr with "non-choked flow" or "choked flow"
#' @export
#'
#' @examples
#' type_of_flow(p1 = 6.8, p2 = 2.2, temp = 80, fl = 0.9)
#' type_of_flow(p1 = 6.8, p2 = 2.2, temp = 80, fl = 0.6)
#'
  type_of_flow <- function(p1, p2, temp, fl){
    dp <- p1-p2
    dp_max   <- (fl^2)*(p1-ff(temp)* vapour_pressure(temp)*0.01)
    if (dp < dp_max ) {
      return("non-choked flow")
    } else {
      return("choked flow")
    }
  }

#' @title Equations for incompressible fluids
#' @description The equations listed below identify the relationships between
#' flow rates, flow coefficients, related installation factors, and pertinent
#' service conditions for control valves handling incompressible fluids.
#' A sizing flow chart for incompressible fluids is given in Annex B. of the
#' ISA-75.01.01-2007 : Flow Equations for Sizing Control Valves.
#'
#' @param kv Flow coefficient in m³/h
#' @param p1 Inlet Absolute pressure
#' @param p2 Outlet Absolute pressure
#' @param temp temperature in °C
#' @param masl metres above sea level [m]
#' @param fl Liquid pressure recovery factor of a control valve without attached fittings
#' @param fr Reynolds number factor.
#' @note  "Absolute pressure" is zero-referenced against a perfect vacuum, using
#' an absolute scale, so it is equal to gauge pressure plus atmospheric pressure.
#' "Gauge pressure" is zero-referenced against ambient air pressure, so
#' it is equal to absolute pressure minus atmospheric pressure. [https://en.wikipedia.org/wiki/Pressure_measurement]
#' @return Volumetric flow rate in m³/h
#' @export
#'
#' @examples
#'  flow_through_valve(kv = 238, p1 = 6.8, p2 = 2.2, temp = 80, fl = 0.6)

  flow_through_valve <- function(kv, p1, p2, temp = 15, masl = 0,  fl, fr = 1){
    #  Absolute pressureis it is gauge pressure plus atmospheric pressure
    dp <- p1-p2
    relat_density <- water_density(15)/water_density(temp)
    ff_value <- ff(temp)* vapour_pressure(temp)*0.01
    dp_max   <- (fl^2)*(p1-ff_value)

    if (dp < dp_max ) {
      flow <- fr*kv*sqrt(dp/relat_density)
    } else {
      flow <- fl*fr*kv*sqrt((p1-ff_value)/relat_density)
    }
  return(flow)
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
#' @return Vapour pressure of water in (kPa). 1 kPa is  0.01 bar
#' @export
#'
#' @examples
#' vapour_pressure(25)

  vapour_pressure <- function(temp = 15){

    if (temp < 0 | temp > 100) {
      stop("\nTemperature outside range for liquid water.\n")
    }
    pv <- 0.61121*exp((18.678-temp/234.5)*(temp/(257.14+temp)))

    return(pv)
  }

#' @title Barometric formula (Atm. Pressure)
#'
#' @description The barometric formula, sometimes called the exponential atmosphere or
#' isothermal atmosphere, is a formula used to model how the pressure
#' or density of the air changes with altitude. The pressure drops
#' approximately by 11.3 Pa per meter in first 1000 meters above sea level.
#' 1 Kilopascals (kPa)	=	0.01 bar
#' https://www.math24.net/barometric-formula/
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param masl metres above sea level [m]
#' @return Atmospheric pressure in bar
#'
#' @export
#'
#' @examples
#' atm_pressure(2600)

  atm_pressure  <- function(masl = 0 ) {
    if (masl < 0)    stop("Metres above sea level cannot be less than 0m")
    if (masl > 6000) stop("Mmeters above sea level cannot be higher than 6000m")

    101.325*exp(-0.000118547*masl)*0.01
  }

