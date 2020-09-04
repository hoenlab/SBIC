#' @name mle
#' @aliases mle
#' @author Jie Zhou  
#' @title Estimate the precision matrix for multivariate normal distribution with given adjacency matrix using maximum likelihood
#' @description This function find the maximum likelihood estimate of the 
#'   precision matrix with given adjacency matrix for multivariate normal distribution.
#' 
#' @param data An \code{n} by \code{p} dataframe representing the observations
#' @param priori A \code{p} by \code{p} matrix representing the given adjacency matrix
#' 
#' @details The methods are based on the relationship between precision matrix of the 
#'   multivariate normal distribution and regression coefficients.
#' 
#' @return 
#'   Returns a \code{p} by \code{p} matrix estimate of the precision matrix 
#'   
#' @examples 
#'   set.seed(1)
#'   d=simulate(n=100,p=200, m1=100, m2=30)
#'   data=d$data
#'   priori=d$realnetwork
#'   precision=mle(data=data,priori=priori)
#' @importFrom stats lm rnorm var
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
