#' @title Valve Analyze
#'
#' @description Valve analyze using the calculation from the EPANET
#'
#' @param net         Network data from EPANET input file with \code{epanetReader::read.inp()}
#' @param report      Report from EPANET report file with \code{epanetReader::read.rpt()}
#' @param valve_name  Valve name in EPNET network and report
#' @param temperature Temperature is in °C
#' @param masl        Meters above sea level [m]
#'
#' @return A Tible Table with the values of ..Values of the
#' @export
#' @import dplyr
#' @importFrom  lubridate hms


  valve_analyze <- function( net, report, valve_name, temperature = 15, masl = 0 ){

    # Filter RIKO Valve base values
    valve <- net$Valves %>% filter(.data$ID == valve_name)

    # Read Valve's Nodes Results
    nodes <- report$nodeResults %>%
      filter( .data$ID == valve$Node1[1] | .data$ID == valve$Node2[1]) %>%
      select( .data$Timestamp, .data$ID, .data$Pressure) %>%
      pivot_wider(names_from = .data$ID, values_from = .data$Pressure)

    # Rename columns Names
    names(nodes) <- c("Timestamp", "p1", "p2")

    # Read Valve's Results
    link <-report$linkResults %>%
      filter(.data$ID == valve_name) %>%
      select(.data$Timestamp, .data$Flow, .data$Velocity, .data$Headloss)

  # Join Nodes and Link's
    p_v   <- vapour_pressure(temperature)
    p_atm <- atm_pressure(masl)*10

    results <- full_join(nodes, link, by="Timestamp") %>%
      mutate( valve     = valve_name,
              Timestamp = hms(.data$Timestamp),
              p1_abs    = .data$p1 + p_atm,
              p2_abs    = .data$p2 + p_atm ) %>%
      mutate( zs        = ifelse( .data$Headloss > 0,
                                  .data$Headloss* ( 2*9.81 / .data$Velocity^2 ),  NaN),
              sigma_0   = ifelse( .data$Headloss > 0,
                                  (.data$p1_abs - p_v )/.data$Headloss,  NaN),
              sigma_1   = ifelse( .data$Headloss > 0,
                                  (.data$p2_abs - p_v )/.data$Headloss, NaN),
              sigma_2   = ifelse( .data$Headloss > 0,
                                  (.data$p2_abs - p_v)/(.data$Headloss + (.data$Velocity^2)/(2*9.81)), NaN ),
              kv        = ifelse( .data$Headloss > 0,
                                  (.data$Flow*3.6)/sqrt(.data$Headloss/10), NaN ) ) %>%
      select( .data$valve, .data$Timestamp, .data$p1, .data$p2, .data$p1_abs, .data$p2_abs,
              .data$Headloss, .data$Velocity, .data$Flow, .data$kv, Zv=.data$zs,
              .data$sigma_0, .data$sigma_1, .data$sigma_2)

    return(results)

  }


#' calculation of the cylinder parameter
#'
#' @param valves tibble table
#' @param dn valve diameter (mm).
#' @param d1 downstream pipe diameter (mm).
#' @param d2 upstream pipe diameter (mm).
#' @import dplyr
#' @import tidyr
#' @return calculation of the parameter as tibble table
#' @export
#'
cylinder_param <- function(valves, dn, d1, d2){
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  valves <- valves %>%
    mutate( kvs  = kv_value(dn, .data$zvs)) %>%
    mutate( fps  = fp(.data$kvs, dn, d1, d2)) %>%
    mutate( flps = flp(.data$kvs, .data$fls, dn, d1, d2)) %>%
    mutate( flps_fps = .data$flps/.data$fps) %>%
    select( .data$name, .data$kv_b, .data$kv_d, .data$kv_e, .data$kvs,
            .data$zvs,  .data$fls, .data$fps, .data$flps, .data$flps_fps)

  return(valves)
}


#' Calculation of the factors for the points cloud
#'
#' @param base_data tibble table
#' @param dn valve diameter (mm).
#' @param masl meters above sea level (m).
#' @param temp The water temperature is in Celsius.
#' @return calculation of the parameter as tibble table
#' @export
#'
points_cloud_param <- function( base_data, dn, masl, temp){
  base_data <- base_data %>%
    mutate( dp = (.data$p1 - .data$p2),
            kv = kv(.data$p1, .data$p2, .data$flow, temp)) %>%
    mutate( zeta  = zeta_vaule(dn, .data$kv),
            sig_1 = sigma_1(.data$p1, .data$p2, masl, temp),
            sig_2 = sigma_2(.data$p1, .data$p2, .data$flow/3600, dn, masl, temp))
  return(base_data)
}


#' data_analyze selection of the right valve type
#'
#' @param base_data tibble table
#' @param valves tibble table
#' @param dn valve diameter (mm).
#' @param d1 downstream pipe diameter (mm).
#' @param d2 upstream pipe diameter (mm).
#' @param masl meters above sea level (m).
#' @param temp The water temperature is in Celsius.
#' @param add_factor additional factor Select valve types that meet Kvs > 1.3 Kv
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @return data_analyze tibble table
#' @export
#'
  valve_param_calc <- function( base_data, valves, dn, d1, d2, masl, temp, add_factor = 1.3){

    valves <- cylinder_param(valves, dn, d1, d2)

    # Calculation characteristics
    base_data <- points_cloud_param( base_data, dn, masl, temp)

    min_kvs <- max(base_data$kv)*add_factor

    # Filter valves with Kvs > 1.3*Kv(max) Requerido
    valves <- valves %>%
      filter(.data$kvs >  min_kvs) %>%
      mutate( min_dn = ceiling( min_kvs^(1/2) * .data$zvs^(1/4) * 626.3^(1/4) / 100)*100) %>%
      arrange(desc(.data$fls))

    data_analyze <- valves %>%
      mutate(data = list(base_data)) %>%
      unnest(.data$data) %>%
      mutate(kv_kvs = ifelse(.data$kv > .data$kvs, NA, .data$kv/.data$kvs), position = 0)

    for(i in c(1:length(data_analyze$kv_kvs))){
      if(is.na(data_analyze$kv_kvs[i])){
        data_analyze$position[i] <- NA
      } else {
        data_analyze$position[i] <- inv_LL3(data_analyze$kv_kvs[i],
                                            data_analyze$kv_b[i],
                                            data_analyze$kv_d[i],
                                            data_analyze$kv_e[i])
      }
    }

    data_analyze <- data_analyze %>%
      mutate( flp_fp = ifelse(kv > .data$kvs, NA, fl_function( .data$position, .data$kv_b, .data$kv_d, .data$kv_e, .data$flps_fps)),
              Sig_i  = ifelse(kv > .data$kvs, NA, Sigma_i( .data$position, .data$kv_b, .data$kv_d, .data$kv_e, .data$flps_fps)),  # Incipient Cavitation
              Sig_c  = ifelse(kv > .data$kvs, NA, Sigma_c( .data$position, .data$kv_b, .data$kv_d, .data$kv_e, .data$flps_fps)),  # Constant Cavitation
              Sig_mv = ifelse(kv > .data$kvs, NA, Sigma_mv( .data$position, .data$kv_b, .data$kv_d, .data$kv_e, .data$flps_fps)), #  Maximum Vibration Cavitation
              regime = ifelse(kv > .data$kvs, NA, cavtation_regime(.data$position, .data$kv_b, .data$kv_d, .data$kv_e, .data$flps_fps, .data$sig_2)),
              cav_index = ifelse(kv > .data$kvs, NA, cavtation_index(.data$position, .data$kv_b, .data$kv_d, .data$kv_e, .data$flps_fps, .data$sig_2)))

    data_analyze <- data_analyze %>%
      group_by( .data$name, .data$kv_b, .data$kv_d, .data$kv_e, .data$zvs, .data$kvs, .data$fls, .data$fps, .data$flps, .data$flps_fps, .data$min_dn) %>%
      nest()

    # Definition of the decision conditions.
    #   cav_index_01: Mean of th cavitation index (this will be between 0 and <3)
    #   cav_index_02: Maximum cavitation Index (the cavitation index muss be lower as 3)
    #   pos_index_01: Minimum Position (This must be above 10%)
    #   pos_index_02: Standard deviation of the position (the bigger it is, the better)

    data_analyze <- data_analyze %>%
      mutate( cav_index_01 = map_dbl( .x = .data$data, .f = ~mean(.x$cav_index)),
              cav_index_02 = map_dbl( .x = .data$data, .f = ~max(.x$cav_index))) %>%
      mutate( pos_index_01 = map_dbl( .x = .data$data, .f = ~min(.x$position)),
              pos_index_02 = map_dbl( .x = .data$data, .f = ~max(.x$position)-min(.x$position))) %>%
      # filter(.data$pos_index_01 > 10 & .data$cav_index_02 < 3) %>%
      filter(.data$pos_index_01 > 10) %>%
      arrange(.data$cav_index_01, desc(.data$pos_index_02))

    return(data_analyze)
  }

