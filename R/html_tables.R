
#' Parameters of the Points at the valve
#' @description This function prints a table of the Points parameters measured
#' or calculated at the valve for each type of valve.
#' @param param_points tibble table
#'
#' @return param_points
#' @export
#' @import dplyr
#' @import kableExtra
#' @import scales
#'
  tab_param_points <- function(param_points){

    param_points %>%

      select( .data$measurement, .data$p1, .data$p2, .data$dp, .data$flow,
              .data$kv, .data$zeta, .data$flp_fp, .data$kv_kvs, .data$position,
              .data$sig_1, .data$sig_2, .data$Sig_i, .data$Sig_c, .data$Sig_mv,
              .data$regime) %>%
    
      mutate( flow     = scales::comma(.data$flow), 
              kv       = scales::comma(.data$kv),
              zeta     = scales::comma(.data$zeta),
              kv_kvs   = scales::percent(round(.data$kv_kvs,2)), 
              position = scales::percent(round(.data$position/100,2))) %>%

      kbl( caption ="Parameter of the valves",
           col.names = c( "Measurements", "P1", "P2", "DP", "Flow", "Kv",
                          "Zeta", "FLp/Fp", "Kv/Kvs", "Position",  "Sigma 1",
                          "Sigma 2", "Sigma_i", "Sigma_c", "Sigma_mv", "Regime"),
           digits = c(0, 2, 2, 2, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 0)) %>%

      kable_classic( bootstrap_options = "striped", full_width = F, position = "left")
      
  }
