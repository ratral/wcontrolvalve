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
#' @import tidyr
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

