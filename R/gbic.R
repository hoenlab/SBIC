#' @name gbic
#' @aliases gbic
#' @title Structural Bayesian information criterion for multivariate normal data with a given graph structure  
#' @author Jie Zhou
#' @description This function estimates the novel structural Bayesian information criterion given the data and
#'   a given graph structure 
#' @param data A \code{n} by \code{p} dataframe representing observations 
#' @param theta The \code{p} by \code{p} matrix representing the given graph structure
#' @param prob The expected error rate  
#' @param P The prior adjacency matrix  
#' 
#' @return 
#'  The value of gbic with given temperature parameter and prior adjacency matrix  
#' @examples 
#'   set.seed(1)
#'   d=simulate(n=100, p=100, m1 = 100, m2 = 30)
#'   data=d$data
#'   P=d$priornetwork
#'   theta=d$realnetwork
#'   prob=0.15
#'   index=gbic(data=data, theta=theta, prob=prob, P=P)
#' 
#' @importFrom stats lm rnorm var
#' @export



gbic=function(data,theta, prob,P){
  n=nrow(data)
  p=ncol(data)
  tem=log(p)/((log(1/prob-1)))
  s=var(data)
  #zero=as.matrix(which(theta==0, arr.ind = T))
  b=mle(data=data, priori=theta)
  theta_mle=if (is.matrix(b)) b else solve(diag(s))
  det=ifelse(det(theta_mle)>0, det(theta_mle),1/(det(s)+1))
  l=(n/2)*(log(det)-sum(diag(s%*%theta_mle)))
  z=ifelse(theta==0,0,1)
  z0=ifelse(P==0, 0, 1)
  d=(sum(z)-p)/2
  a=z-z0
  z1=ifelse(a==0,0,1)
  z1=as.matrix(z1)
  bic=-2*l+d*log(n)
  bolz=log(p)*sum(z1)/tem
  gbic=bic+bolz
  return(gbic=gbic)
}
