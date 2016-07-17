#' The probability density function of the CosWeibull probability distribution.
#' @export
#'
#' @param x Vector of quantiles.
#' @param alpha Alpha parameter.
#' @param lambda Lambda parameter.
#' @return A vector with n observations of the CosWeibull distribution.
#' @examples
#' dcosw(1,1,1)
#' dcosw(1,1.5,2)

dcosw<-function(x,alpha,lambda){
    (pi/2)*alpha*(lambda^(alpha))*(x^(alpha-1))*exp(-(lambda*x)^alpha)*cos((pi/2)*exp(-(lambda*x)^alpha))
}
