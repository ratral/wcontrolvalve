

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
