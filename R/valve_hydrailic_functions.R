
#' @title Valve flow coefficient
#' @description  The valve flow coefficient Kv is defined as the number
#' of cubic meters per hour of water that will flow through
#' a control valve at a specified position of the control valve (travel) h
#' with a differential pressure Delta P (p1-p2) across it.
#' @param p1 Inlet pressure [bar]
#' @param p2 Outlet pressure [bar]
#' @param flow flow in m³/h
#' @param temp temperature is in Celsius.
#' @return kv Flow coefficient in m³/h
#' @export
#' @examples
#' kv(2, 1, 200)

  kv <- function(p1, p2, flow, temp = 15.6){
    r_density <-  water_density(temp)/water_density(15.6)
    kv <- flow*sqrt(r_density/(p1-p2))
    return(kv)
  }


#' @title flow in function of the flow coefficient Kv
#' @param p1  Inlet Gauge pressure (bar).
#' @param p2  Outlet Gauge pressure (bar).
#' @param kv Flow coefficient in (m³/h).
#' @param temp temperature is in Celsius.
#' @return flow in (m³/h).
#' @export
#'
  flow_Kv <- function(p1, p2, kv, temp){
    r_density =  water_density(temp)/water_density(15.6);
    flow = kv/sqrt(r_density/(p1-p2));
    return(flow);
  }

#'
#' @title Flow coefficient Kv Value in function of the Zeta  Value
#' @description The valve flow coefficient Kv is defined as the number
#' of cubic meters per hour of 5°C to 30°C water that will flow through
#' a control valve at a specified position of the control valve (travel) h
#' with a differential pressure Delta P (p1-p2) of 1bar  (105 Pa)
#' across it.
#' @author Dr. Raúl Trujillo Álvarez
#' @param dn diameter in millimeter (mm)
#' @param zeta dimensionless quantity
#' @return kv Flow coefficient in m³/h
#' @export
#' @examples
#' kv_value( dn = 500, zeta = 1.9)
  kv_value <- function(dn, zeta){
    kv <- ((dn)^2)/sqrt(626.3*zeta)
    return(kv)
  }

#' @title  Resistance Coefficient Zeta in function of Kv
#' @description Pressure drop or head loss is proportional to the velocity
#' in valves or fittings. For the most engineering practices it can be assumed
#' that pressure drop or head loss due to flow of fluids in turbulent range
#' through valves and fittings is proportional to square of velocity.
#' @author Dr. Raúl Trujillo Álvarez
#' @param kv Kv Flow coefficient in m³/h
#' @param dn diameter in millimeter (mm)
#' @return Zeta Vaule
#' @export
#' @examples
#' zeta_vaule( dn = 500, kv = 7247.229)
#'
  zeta_vaule  <- function(dn, kv){
    zeta <- (1/626.3)*((dn)^2/kv)^2
    return(zeta)
  }

#' @title  Resistance coefficients of all fittings attached to the control valve
#' @description  The algebraic sum of all effective resistance coefficients of
#' all fittings attached to the control valve.
#' @param dn valve diameter (mm).
#' @param d1 downstream pipe diameter (mm).
#' @param d2 upstream pipe diameter (mm)
#' @return Resistance coefficient.
#' @export
  resistance_coefficient <- function(dn, d1, d2){
    reducer  =  0.5 * ((1-(dn/d1)^2)^2)
    diffuser =  ((1-(dn/d2)^2)^2)
    bernulli =  (dn/d2)^4 - (dn/d1)^4
    result = reducer + diffuser + bernulli
    return(result)
  }

#' @title Ff Liquid critical pressure ratio factor
#' @description Ff is the liquid critical pressure ratio factor. This factor is
#' the ratio of the apparent vena contracta pressure at choked flow conditions
#' to the vapor pressure of the liquid at inlet temperature. [ISA-75.01.01-2007]
#' At vapor pressures near zero, this factor is 0.96.
#' @author Dr. Raúl Trujillo Álvarez \email{dr.ing.trujillo@gmail.com}
#' @param temp is in °C
#' @return Liquid critical pressure ratio factor Dimensionless
#' @export
#' @examples
#' ff(15)
  ff <- function(temp = 15){
    pv <- vapour_pressure(temp)
    #  the critical thermodynamic pressure for water is 221.2 bar
    pc <- 221.2
    return(0.96-0.28*sqrt(pv/pc))
  }


#' @title Fp Piping geometry factor
#' @description The piping geometry factor Fp accounts for fittings attached to
#' either the valve inlet or the outlet that disturb the flow to the extent that
#' valve capacity is affected. Fp is actually the ratio of the flow coefficient
#' of a valve with attached fittings to the flow coefficient (kv) of a valve
#' installed in a straight pipe of the same size as the valve.
#' @param kv Flow coefficient in m³/h
#' @param dn diameter in millimeter (mm).
#' @param d1 Inlet diameter reducer only in millimeter (mm).
#' @param d2 Outlet diameter increase only in millimeter (mm).
#' @return Fp Piping geometry factor, dimensionless
#' @export
#' @examples
#' fp(kv = 7247.229, dn =0.5, d1 = 0.6, d2 = 0.6)
#'
  fp <- function(kv, dn, d1, d2){
    rc <- resistance_coefficient(dn, d1, d2)
    return(1 / sqrt(1+(rc*(kv/dn^2)^2)/0.0016))
  }


#' @title Flp Combined liquid pressure recovery factor
#' @description When a valve is installed with reducers or other attached
#' fittings, the liquid pressure recovery of the valve-fitting combination is
#' not the same as that for the valve alone. For calculations involving choked
#' flow, it is convenient to treat the piping geometry factor (fp) and the fl
#' factor for the valve-fitting combination as a single factor, flp.
#' @param kv Flow coefficient in m³/h
#' @param fl Liquid pressure recovery factor of a control valve without attached fittings
#' @param dn diameter in millimeter (mm).
#' @param d1 Inlet diameter reducer only in millimeter (mm).
#' @param d2 Outlet diameter increase only in millimeter (mm).
#' @return Product of the liquid pressure recovery factor of a valve with
#' attached fittings (no symbol has been identified) and the piping geometry
#' factor, dimensionless.
#' @export
#' @examples
#' flp(kv = 7247.229, fl = 0.9, dn =0.5, d1 = 0.6, d2 = 0.6)
#'
  flp <- function(kv, fl, dn, d1, d2){
    rc <- resistance_coefficient(dn, d1, d2)
    return(fl / sqrt(1+(rc*(kv/dn^2)^2)*(fl^2)/0.0016))
  }


#' Valve Reynolds number
#' @param flow flow in m³/h
#' @param kv Flow coefficient in m³/h
#' @param dn diameter in millimeter (mm).
#' @param d1 Inlet diameter reducer only in millimeter (mm).
#' @param d2 Outlet diameter increase only in millimeter (mm).
#' @param temp temperature is in Celsius.
#' @param fl Liquid pressure recovery factor of a control valve without attached fittings
#' @param fd Valve style modifier (1.0 fuer Ventile mit V-fourmigen Drosselquerschnitt)
#' @return valve Reynolds number
#' @export
#'
  Reynolds_valve <- function( flow, kv, dn, d1, d2, temp, fl, fd){

    Fp <- fp(kv, dn, d1, d2)  # Fp Piping geometry factor

    n4 <- 7.07e4
    n2 <- 0.0016

    kvisc <- kinematic_viscosity(temp) # in mm2/s

    reynolds <- (n4*fd*flow)/(kvisc*sqrt(Fp*fl*kv))*
                ((Fp^2*fl^2*kv^2)/(n2*dn^4)+1)^(1/4)

    return(reynolds)

  }


#' Title
#' @param reynolds_factor A \code{tibble} containing the values for the
#' calculation and plot of the Valve Reynolds Number Factor.
#' @param Rev Valve Reynolds number
#' @param problem_typ types of problems: "selection", "flow", "pressure"
#' @importFrom stats spline
#' @return Valve Reynolds number factor
#' @export
#'
  fr <- function(reynolds_factor, Rev, problem_typ){

    dat <- reynolds_factor %>%
      filter(problem_typ == problem_typ)

    factor <- case_when(
      Rev >= 40000 ~ 1.0,
      Rev <  56 & problem_typ == "selection" ~ 0.0190  * (Rev)^0.67,
      Rev < 106 & problem_typ == "flow"      ~ 0.0027  * (Rev),
      Rev <  30 & problem_typ == "pressure"  ~ 0.0052  * (Rev),
      TRUE ~ with(dat, spline(dat$reynolds, dat$fr, xout = c(Rev)))$y
    )

    return(factor)

  }


#' @title  Function FL liquid pressure recovery factor
#' @description for a control valve without attached fittings
#' @param x valve position
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
#' @return fl liquid pressure recovery factor for
#'
#' @export
#'
  fl_function <- function( x, b, d, e, fls ){
    sigma_value <- 1/(fls^2) - 1
    kv_kvs <- drm_LL3(x, b, d, e)
    fl <- sqrt(1/(sigma_value * kv_kvs + 1))
    return(fl)
  }

#' Function Resistance Coefficient Zeta
#'
#' @param x valve position
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param zvs Resistance Coefficient full open
#'
#' @return zv Resistance Coefficient
#' @export
#'
  zv_function <- function( x, b, d, e, zvs ) {
    return(zvs/(drm_LL3( x, b, d, e ))^2)
  }

#' @title Differential pressure maximum between upstream and downstream pressure
#' @param p1 Gauge upstream pressure
#' @param fl liquid pressure recovery factor
#' @param kv Flow coefficient value in (m3/h).
#' @param dn valve diameter (mm).
#' @param d1 downstream pipe diameter (mm).
#' @param d2 upstream pipe diameter (mm).
#' @param masl meters above sea level (m).
#' @param temp The temperature is in Celsius.
#' @return DPmax (bar)
#'
#' @export
#'
  dp_max <- function(p1, fl, kv, dn, d1, d2, masl, temp){
    p1 = p1 + atm_pressure(masl)
    flp_value <-  flp(kv, fl, dn, d1, d2)
    fp_value  <-  fp(kv, dn, d1, d2)
    ff_value <- ff(temp)
    dp <- (flp_value/fp_value)^2 *(p1 - ff_value * vapour_pressure(temp))
    return(dp)
  }


#' @title the maximum flow through the valve
#' @param p1 Gauge upstream pressure (bar)
#' @param fl liquid pressure recovery factor
#' @param fr Reynolds number factor
#' @param kv Flow coefficient value in (m3/h).
#' @param dn valve diameter (mm).
#' @param d1 downstream pipe diameter (mm).
#' @param d2 upstream pipe diameter (mm).
#' @param masl meters above sea level (mm).
#' @param temp The temperature is in Celsius.
#' @return q_max (m3/h)
#'
#' @export
#'
  q_max <- function(p1, fl, fr, kv, dn, d1, d2, masl, temp){
    p1 = p1 + atm_pressure(masl)
    flp_value <-  flp(kv, fl, dn, d1, d2)
    ff_value <- ff(temp)
    r_density <- water_density(temp)/water_density(15.6)
    flow <- kv * flp_value * fr * sqrt((p1 - ff_value * vapour_pressure(temp))/r_density)
    return(flow)
  }


#' @title cavitation index (Reference upstream pressure P1):
#' @description The value for the operating service conditions of a valve.
#'
#' @param p1 Gauge Inlet pressure (bar).
#' @param p2 Gauge outlet pressure (bar).
#' @param masl meters above sea level (m).
#' @param temp The temperature is in Celsius.
#'
#' @return Sigma
#' @export
#'
  sigma_0 <- function(p1, p2, masl, temp){
    pv <- vapour_pressure(temp)
    p1 = p1 + atm_pressure(masl)
    p2 = p2 + atm_pressure(masl)
    return((p1-pv)/(p1-p2))
  }


#' @title cavitation index (Reference downstream pressure P2):
#' @description The value for the operating service conditions of a valve.
#' Reference downstream pressure
#'
#' @param p1 Gauge Inlet pressure (bar).
#' @param p2 Gauge outlet pressure (bar).
#' @param masl meters above sea level (m).
#' @param temp The temperature is in Celsius.
#'
#' @return Sigma
#' @export
#'
  sigma_1 <- function(p1, p2, masl, temp){
    pv <- vapour_pressure(temp)
    p1 = p1 + atm_pressure(masl)
    p2 = p2 + atm_pressure(masl)
    return((p2-pv)/(p1-p2))
  }


#' @title cavitation index (Reference downstream pressure P2):
#' @description The value for the operating service conditions of a valve.
#' Reference downstream pressure and Adding the cavitation caused by surface
#' roughness, an isolated roughness, an offset in the boundary, or by any
#' device for which it is not possible or convenient to evaluate a pressure
#' differential), the velocity head can be used in adding the DP in
#' the Equation.
#'
#' @param p1 Gauge Inlet pressure (bar).
#' @param p2 Gauge outlet pressure (bar).
#' @param flow flow in (m³/s).
#' @param dn valve diameter (mm).
#' @param masl meters above sea level (m).
#' @param temp The temperature is in Celsius.
#'
#' @return sigma
#' @export
#'
  sigma_2 <- function(p1, p2, flow, dn, masl, temp){
    pv <- vapour_pressure(temp)
    p1 = (p1 + atm_pressure(masl))
    p2 = (p2 + atm_pressure(masl))
    vfactor <- velocity(flow, dn/1000)^2/(2*9.807)
    return((p2-pv)/(p1-p2+vfactor))
  }


#' @title Incipient Cavitation.
#' @description The onset of cavitation, where only small vapor bubbles are
#' formed in the flow stream. A cavitation level sufficient to begin minor,
#' observable indications of pitting damage.
#'  - Onset of cavitation;
#'  - Detect using high-frequency vibration measurement;
#'  - Very local phenomenon; Transient: random “ticks” sound;
#'  - Low-level cavitation: usually not damaging;
#'  - Occurs prior to the loss of capacity
#'
#' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
#' @param x valve position
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#'
#' @return Sigma_i
#' @export
#'
  Sigma_i <- function(x, b, d, e, fls){
    xfz <- 0.71
    kv_kvs <- drm_LL3(x, b, d, e)
    return( (1/(xfz * fls^2) - 1) * kv_kvs)
  }

#' @title Constant Cavitation.
#'
#' @description An early level of cavitation characterized by mild,
#' steady popping or crackling sounds that may be audible or detected by
#' vibration measurements. It is the next higher inflection point on the
#' cavitation profile above the point of incipient cavitation.
#'  - More regular cavitation events
#'  - Lower frequency sound and vibration sensed: “rumbling” sound
#'  - Some damage to surfaces may occur: dependent upon valve and trim styles, and materials.
#'
#' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
#' @param x valve position
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#'
#' @return Sigma_c
#' @export
#'
  Sigma_c <- function(x, b, d, e, fls){
    kc <- 0.81
    kv_kvs <- drm_LL3(x, b, d, e)
    return( (1/(kc * fls^2) - 1) * kv_kvs)
  }


#' @title Maximum Vibration Cavitation
#'
#' @description The level of cavitation associated with peak vibration
#' measurements.
#' - Highest vibration amplitude: sounds like “marbles” or “gravel”
#'  - Vigorous, large scale cavitation
#'  - Predicted by steady flow pressure distribution (=Fl)
#'  - Very high damage potential
#'
#' @param x valve position
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
#'
#' @return Sigma_mv
#' @export
#'
  Sigma_mv <- function(x, b, d, e, fls){
    kv_kvs <- drm_LL3(x, b, d, e)
    return( (1/(fls^2) - 1) * kv_kvs )
  }



#' Cavitation Level (Regime)
#'
#' @param x valve position
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
#' @param sigma_value cavitation index (Reference downstream pressure P2)
#'
#' @return regime
#' @export
#'
  cavtation_regime <- function(x, b, d, e, fls, sigma_value) {
    limit_1 <- Sigma_i(x, b, d, e, fls)
    limit_2 <- Sigma_c(x, b, d, e, fls)
    limit_3 <- Sigma_mv(x, b, d, e, fls)

    regime <- case_when(
      sigma_value > limit_1 ~ "free of cavitation (regime I)",
      sigma_value < limit_1 & sigma_value > limit_2 ~ "incipient cavitation (regime II)",
      sigma_value < limit_2 & sigma_value > limit_3 ~ "constant cavitation (regime III)",
      sigma_value < limit_3 ~ "maximum cavitation (regime IV)"
    )

    return(regime)
  }


  #' Cavitation Index (Regime)
  #'
  #' @param x valve position
  #' @param b steepness
  #' @param d upper value
  #' @param e the effective dose
  #' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
  #' @param sigma_value cavitation index (Reference downstream pressure P2)
  #'
  #' @return index 0 = free; 1 <- incipient; 2 <- constant; 3 <- maximum
  #' @export
  #'
  cavtation_index <- function(x, b, d, e, fls, sigma_value) {
    limit_1 <- Sigma_i(x, b, d, e, fls)
    limit_2 <- Sigma_c(x, b, d, e, fls)
    limit_3 <- Sigma_mv(x, b, d, e, fls)

    index <- case_when(
      sigma_value > limit_1 ~ 0,
      sigma_value < limit_1 & sigma_value > limit_2 ~ 1,
      sigma_value < limit_2 & sigma_value > limit_3 ~ 2,
      sigma_value < limit_3 ~ 3
    )

    return(index)
  }
