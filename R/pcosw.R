#' The cumulative density function of the CosWeibull probability distribution.
#' @export
#'
#' @param q Vector of quantiles.
#' @param alpha Alpha parameter.
#' @param lambda Lambda parameter.
#' @param lower Lower parameter.
#' @param log.p Log.p parameter.
#' @return A vector with n observations of the CosWeibull distribution.
#' @examples
#' pcosw(0.5,1,1,lower = TRUE,log.p = FALSE)
#' pcosw(0.5,1.5,1,lower = TRUE,log.p = FALSE)

pcosw<-function(q,alpha,lambda,lower = TRUE,log.p = FALSE){

  if (log.p == TRUE) {
    if (lower == TRUE){
      log(1-(sin((pi/2)*exp(-(lambda*q)^alpha))))
    } else {
      log((sin((pi/2)*exp(-(lambda*q)^alpha))))
    }
  } else {
    if (lower == TRUE){
      1-(sin((pi/2)*exp(-(lambda*q)^alpha)))
    } else {
      (sin((pi/2)*exp(-(lambda*q)^alpha)))
    }
  }
}
