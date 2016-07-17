#' The survival function of the CosWeibull probability distribution.
#' @export
#'
#' @param x Vector of quantiles..
#' @param alpha Alpha parameter.
#' @param lambda Lambda parameter.
#' @return A vector with n observations of the CosWeibull distribution.
#' @examples
#' hcosw(1,1,1)
#' hcosw(1,1.5,2)

scosw<-function(x,alpha,lambda){
  sin((pi/2)*exp(-(lambda*x)^alpha))
}
