#' The hazard function of the CosWeibull probability distribution.
#' @export
#'
#' @param x Vector of quantiles..
#' @param alpha Alpha parameter.
#' @param lambda Lambda parameter.
#' @return A vector with n observations of the CosWeibull distribution.
#' @examples
#' hcosw(1,1.5,2)
#' hcosw(1,2,0.5)

hcosw<-function(x,alpha,lambda){
  (pi/2)*alpha*(lambda^(alpha))*(x^(alpha-1))*exp(-(lambda*x)^alpha)*pracma::cot((pi/2)*exp(-(lambda*x)^alpha))
}
