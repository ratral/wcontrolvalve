
#' Plot the Valve flow coefficient
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param cylindertyp Name of the control type
#'
#' @import ggplot2
#' @import latex2exp
#'
#' @return ggplot graphic. Valve flow coefficient
#' @export
#'
  plot_kv_kvs <- function(b, d, e, cylindertyp){
    x <- data.frame(x = 0:100)
    ggplot( data = x, mapping = aes(x = x)) +
      stat_function( fun = function(x) {x}, aes(), size = 1,
                     color = "black", linetype = 2) +
      stat_function( fun = function(x) {drm_LL3( x, b, d, e)*100},
                     size = 1, color = "blue") +
      scale_x_continuous( breaks = seq(0, 100, 10)) +
      scale_y_continuous( breaks = seq(0, 100, 10)) +
      labs( title    = TeX("Flow Characteristics $(k_{v}/k_{vs})$"),
            subtitle = paste("For Cylinder:", cylindertyp),
            caption  = "Dr.Trujillo",
            x        = "Opening degree (%)",
            y        = TeX('$k_{v}/k_{vS}$')) +
      theme_bw()
  }


#' Plot Flow coefficient Kv Value
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param dn dn diameter in meter (m)
#' @param zvs Resistance Coefficient Zeta full open
#' @param cylindertyp Name of the control type
#'
#'
#' @import ggplot2
#' @import latex2exp
#'
#' @return ggplot graphic. Flow coefficient Kv Value
#'
#' @export
#'
  plot_kv <- function(b, d, e, dn, zvs, cylindertyp){
    x   <- data.frame(x = 0:100)
    kvs <- kv_value(dn, zvs)
    ggplot( data = x, mapping = aes(x = x)) +
      stat_function( fun = function(x) {drm_LL3( x, b, d, e)*kvs},
                     size = 1, color = "blue") +
      scale_x_continuous( breaks = seq(0, 100, 10)) +
      labs( title    = TeX("Flow Coefficient $k_{v} (m^3/h)$"),
            subtitle = paste("For Cylinder:", cylindertyp),
            caption  = "Dr.Trujillo",
            x        = "Opening degree (%)",
            y        = TeX('$k_{v} (m^3/h)$')) +
      theme_bw()
  }


#' Plot Zeta Value Curve
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param zvs Resistance Coefficient Zeta full open
#' @param cylindertyp Name of the control type
#'
#' @import ggplot2
#' @import latex2exp
#' @import scales
#'
#' @return ggplot graphic. Zeta Value Curve
#' @export
#'
  plot_zv <- function(b, d, e, zvs, cylindertyp){
    x   <- data.frame(x = 1:100)
    ggplot( data = x, mapping = aes(x = x)) +
      stat_function( fun = function(x) {zvs/(drm_LL3(x, b, d, e))^2},
                     size = 1, color = "green") +
      scale_y_log10() +
      scale_x_continuous( breaks = seq(0, 100, 10)) +
      annotation_logticks(sides = "lr") +
      labs( title    = "Zeta Value Curve",
            subtitle = paste("For Cylinder:", cylindertyp),
            caption  = "Dr.Trujillo",
            x        = "Opening degree (%)",
            y        = TeX('$\\zeta_{v}$')) +
      theme_bw()
  }

