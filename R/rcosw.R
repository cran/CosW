#' Generates random deviates from a CosWeibull probability distribution.
#' @export
#' @importFrom fdrtool dhalfnorm
#' @importFrom fdrtool rhalfnorm
#'
#' @param n Number of observations to be generated.
#' @param alpha Alpha parameter.
#' @param lambda Lambda parameter.
#' @return A vector with n observations of the CosWeibull distribution.
#' @examples
#' rcosw(1000,1,1)
#' rcosw(1000,1,0.1)

rcosw<-function(n,alpha,lambda){

    accept = c()
    count = 0
    while (length(accept) < n){

      U <- rhalfnorm(1)
      x <- rhalfnorm(1)

      if(U <= dcosw(x, alpha, lambda)/(sqrt(pi)*dhalfnorm(x)/sqrt(2))) {
        accept[count] = x
        count = count + 1
      }
    }
    return(accept)
}
