#' The quantile function of the CosWeibull probability distribution.
#' @export
#'
#' @param p Vector of probabilities.
#' @param alpha Alpha parameter.
#' @param lambda Lambda parameter.
#' @param lower Lower parameter.
#' @param log.p Log.p parameter.
#' @return A vector with n observations of the CosWeibull distribution.
#' @examples
#' qcosw(1, 1, 1, TRUE, FALSE)
#' qcosw(1, 1, 0.1, TRUE, FALSE)

qcosw<-function(p,alpha = 1,lambda,lower = TRUE,log.p = FALSE){

  if (alpha == 1){
    if (log.p == TRUE) {
      if (lower == TRUE){
        y=(acos(1-p))
        log((-1/lambda)*(log((1-((2/pi)*y))))^(1/alpha))
      }else{
        y=(acos(p))
        log((-1/lambda)*(log((1-((2/pi)*y))))^(1/alpha))
      }
    } else {
      if (lower == TRUE){
        y=(acos(1-p))
        (-1/lambda)*(log((1-((2/pi)*y))))^(1/alpha)
      }else{
        y=(acos(p))
        (-1/lambda)*(log((1-((2/pi)*y))))^(1/alpha)
      }
    }
  }else{
    print("The default value for the alpha parameter must be equal to 1.")
  }
}
