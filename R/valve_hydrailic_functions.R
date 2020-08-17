
#' @title Valve flow coefficient
#' @description  The valve flow coefficient Kv is defined as the number
#' of cubic meters per hour of water that will flow through
#' a control valve at a specified position of the control valve (travel) h
#' with a differential pressure Delta P (p1-p2) across it.
#' @param p1 Inlet pressure [bar]
#' @param p2 Outlet pressure [bar]
#' @param flow flow in m³/h
#' @param temperature Inlet water temperature in °C
#'
#' @return kv Flow coefficient in m³/h
#' @export
#'
#' @examples
#' kv(2, 1, 200)

  kv <- function(p1, p2, flow, temperature = 15.6){

    # radio of density of water (15.6)
    r_density <-  water_density(temperature)/water_density(15.6)

    kv <- flow*sqrt(r_density/(p1-p2))

    return(kv)
  }


#'
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
#' @return kv Flow coefficient in m³/h
#' @export
#'
#' @examples
#' kv_value( dn = 0.5, zeta = 1.9)

  kv_value <- function(dn, zeta){

    kv <- ((dn*1000)^2)/sqrt(626.3*zeta)
    return(kv)
  }

#' @title  Resistance Coefficient Zeta in function of Kv
#' @description Pressure drop or head loss is proportional to the velocity
#' in valves or fittings. For the most engineering practices it can be assumed
#' that pressure drop or head loss due to flow of fluids in turbulent range
#' through valves and fittings is proportional to square of velocity.
#'
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#'
#' @param kv Kv Flow coefficient in m³/h
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

    zeta <- (1/626.3)*((dn*1000)^2/kv)^2
    return(zeta)
  }


#' @title Ff Liquid critical pressure ratio factor
#' @description Ff is the liquid critical pressure ratio factor. This factor is
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


#' @title Fp Piping geometry factor
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
#' @return Fp Piping geometry factor, dimensionless
#' @export
#'
#' @examples
#' fp(kv = 7247.229, dn =0.5, d1 = 0.6, d2 = 0.6)
#'
  fp <- function(kv, dn, d1, d2){
    z1 <- 0.5*(1-(dn/d1)^2)^2
    z2 <- (1 -(dn/d2)^2)^2
    zb1 <- 1 - (dn/d1)^4
    zb2 <- 1 - (dn/d2)^4
    z  <- z1 + z2 + (zb1 - zb2)
    return(1 / sqrt(1+(z*(kv/dn^2)^2)/0.0016))
  }


#' @title Flp Combined liquid pressure recovery factor
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
#' @param p1 Inlet Absolute pressure [bar]
#' @param p2 Outlet Absolute pressure [bar]
#' @param temp Inlet water temperature in [°C]
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

#' @title Equations for in-compressible fluids
#' @description The equations listed below identify the relationships between
#' flow rates, flow coefficients, related installation factors, and pertinent
#' service conditions for control valves handling in-compressible fluids.
#' A sizing flow chart for in-compressible fluids is given in Annex B. of the
#' ISA-75.01.01-2007 : Flow Equations for Sizing Control Valves.
#'
#' @param kv Flow coefficient in [m³/h]
#' @param p1 Inlet Absolute pressure [bar]
#' @param p2 Outlet Absolute pressure [bar]
#' @param temp temperature in [°C]
#' @param masl meter above sea level [m]
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
#' @description The vapor pressure of water is the pressure at which water vapor is in
#' thermodynamic equilibrium with its condensed state. At higher pressures water
#' would condense. The water vapor pressure is the partial pressure of water
#' vapor in any gas mixture in equilibrium with solid or liquid water.
#' As for other substances, water vapor pressure is a function of temperature
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

    p_at <- 101.325*exp(-0.000118547*masl)*0.01
    return(p_at)
  }



#' @title dose-response models.
#' @description Built-in dose-response models. These models are
#' parameterized using a unified structure with a coefficient b denoting the
#' steepness of the dose-response curve, d the upper asymptotes or
#' limits of the response, and, for some models, e the effective dose.
#'
#'
#' @param x valve position
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#'
#' @return ll3
#' @export
#'
#' @examples
#' drm_LL3( 50, -2.39, 1.39, 67.42 )
  drm_LL3 <- function(x,b,d,e){
    ll3 <- d/(1+exp(b*(log(x)-log(e))))
    return(ll3)
  }


#' @title Inverse of the dose-response models.
#' @description Built-in the inverse of dose-response models.
#'
#' @param kv_kvs valve position
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#'
#' @importFrom stats uniroot
#'
#' @return position
#' @export
#'
#' @examples
#' inv_LL3(0.4567872, -2.39, 1.39, 67.42 )
  inv_LL3 <- function( kv_kvs, b, d, e ){
      root <- uniroot( function(x){ d/(1+exp(b*(log(x)-log(e))))- kv_kvs} ,
                       lower = 0,
                       upper = 100,
                       tol   = 1e-10)
      return(root$root)
    }
