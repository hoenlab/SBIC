##For a given network matrix, compute
##the mle of precision matrix
#' @name mle
#' @aliases mle
#' @title Estimate the precision matrix for multinormal distribution with given adjacency matrix
#' @author Jie Zhou
#' @usage mle(data, priori)
#' @param data An \code{n} by \code{p} matrix of observations
#' @param priori A \code{p} by \code{p} adjacency matrix of the prior network
#' @return A \code{p} by \code{p} matrix of the estimated maximum likelihood precision matrix
#' @details The methods  are based on the relationship between precision matrix of
#'   multinormal distribution and regression coefficients.
#' @examples
#' # set.seed(1)
#' # d=simulate(p=10,n=100, prob1 = 0.1, prob2 = 0.2,ka=4)
#' # data=d$data
#' # priori=d$realnetwork
#' # precision=mle(data=data,priori=priori)
#' @importFrom stats var lm
#' @export
mle=function(data,priori){
  priori=priori+t(priori)
  priori=ifelse(priori==0,0,1)
  diag(priori)=0
  p=dim(data)[2]
  n=dim(data)[1]
  precision=matrix(0,nrow = p,ncol = p)
  ##for the first row
  for (j in 1:p) {
    y=data[,j]
    index=which(priori[j,]==1)
    if (length(index)==0) {
      precision[j,]=0
      precision[j,j]=1/var(y)
      next
    }
    x=data[,index]
    result=lm(y~0+x)
    alpha=result$coefficients
    sigma=t(result$residuals)%*%(result$residuals)/result$df.residual
    precision[j,index]=-alpha/sigma[1]
    precision[j,j]=1/sigma
  }
  precision=(precision+t(precision))/2
  return(precision)
}
