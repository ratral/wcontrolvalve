
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
  plot_kv_kvs <- function( b, d, e, cylindertyp ){
    x <- data.frame(x = 0:100)
    ggplot( data = x, mapping = aes(x = x)) +
      stat_function( fun = function(x) {x}, aes(), size = 1,
                     color = "black", linetype = 2) +
      stat_function( fun = function(x) {drm_LL3( x, b, d, e)*100},
                     size = 1, color = "blue") +
      scale_x_continuous( breaks = seq(0, 100, 10)) +
      scale_y_continuous( breaks = seq(0, 100, 10)) +
      labs( title    = TeX("Flow Characteristics $(k_{v}/k_{vs})$"),
            subtitle = paste("Flow characteristics: ", cylindertyp),
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
#' @param cylindertyp Name of the control characteristic
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
            subtitle = paste("Flow characteristics: ", cylindertyp),
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
#' @param cylindertyp Name of the control characteristic
#'
#' @import ggplot2
#' @import latex2exp
#' @import scales
#'
#' @return ggplot graphic. Zeta Value Curve
#' @export
#'
  plot_zv <- function(b, d, e, zvs, cylindertyp){
    x   <- data.frame(x = 0:100)
    ggplot( data = x, mapping = aes(x = x)) +
      stat_function( fun = function(x) {zv_function( x, b, d, e, zvs )},
                     size = 1, color = "green") +
      scale_y_log10() +
      scale_x_continuous( breaks = seq(0, 100, 10)) +
      annotation_logticks(sides = "lr") +
      labs( title    = TeX('Zeta Value Curve $\\zeta_{v}$'),
            subtitle = paste("Flow characteristics :", cylindertyp),
            caption  = "Dr.Trujillo",
            x        = "Opening degree (%)",
            y        = TeX('$\\zeta_{v}$')) +
      theme_bw()
  }


#' Plot liquid pressure recovery factor
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
#' @param cylindertyp  Name of the control characteristic
#'
#' @return ggplot graphic. of the liquid pressure recovery factor
#' @export
#'
  plot_fl <- function( b, d, e, fls, cylindertyp){
    x   <- data.frame(x = 1:100)
    ggplot( data = x, mapping = aes(x = x)) +
      stat_function( fun = function(x) {fl_function( x, b, d, e, fls)},
                     size = 1, color = "black") +
      scale_x_continuous( breaks = seq(0, 100, 10)) +
      scale_y_continuous( breaks = seq(0, 1, 0.05)) +
      labs( title    = TeX('Liquid Pressure Recovery Factor ($F_{L}$, or
                            respectively $F_{LP} / F_{P}$)'),
             subtitle = paste("Flow characteristics :", cylindertyp),
             caption  = "Dr.Trujillo",
             x        = "Opening degree (%)",
             y        = TeX('$F_{L}$')) +
       theme_bw()
  }


#' Plot sigma for Incipient, Constant and Maximum cavitation
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
#' @param cylindertyp  Name of the control characteristic
#'
#' @return ggplot graphic. of the sigma for Incipient, Constant and Maximum cavitation
#' @export
#'
  plot_sigma <- function( b, d, e, fls, cylindertyp){
    x   <- data.frame(x = 1:100)
    ggplot( data = x, mapping = aes(x = x)) +
      stat_function( fun = function(x) {Sigma_mv(x, b, d, e, fls)}, size = 1, aes(colour = "3.-Maximum")) +
      stat_function( fun = function(x) {Sigma_c(x, b, d, e, fls)}, size = 1,  aes(colour = "2.-Constant")) +
      stat_function( fun = function(x) {Sigma_i(x, b, d, e, fls)}, size = 1,  aes(colour = "1.-Incipient")) +
      scale_colour_manual("Borders of the cavitation: ", values = c("green", "orange", "red")) +
      scale_x_continuous( breaks = seq(0, 100, 10)) +
      labs( title    = TeX('Sigma values ($\\sigma$)'),
            subtitle = paste("Flow characteristics :", cylindertyp),
            caption  = "Dr.Trujillo",
            x        = "Opening degree (%)",
            y        = TeX('Sigma values ($\\sigma$)')) +
      theme_bw() + theme(legend.position = "bottom")
  }

