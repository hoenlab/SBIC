## SBIC criterion for Gaussian model
#' @name gbic
#' @aliases gbic
#' @author Jie Zhou
#' @title SBIC criterion for Gaussian Graphical Model
#' @description This function
#' @param data A data in matrix form of \code{n} observations and \code{p} variables
#' @param theta A \code{p} by \code{p} matrix representing the given graph structure
#' @param tem A temperature parameter involved in SBIC
#' @param P The prior adjacency matrix
#' @return \item{gbic}{the value of gbic with given temperature parameter and prior adjacency matrix}
#' @importFrom stats var
#' @examples
#'  # set.seed(1)
#'  # d=simulate(p=10,n=100, prob1 = 0.1, prob2 = 0.2,ka=4)
#'  # data=d$data
#'  # P=d$priornetwork
#'  # theta=d$realnetwork
#'  # tem=0.5
#'  # index=sbic(data=data, theta=theta, tem=tem, P=P)

gbic=function(data,theta, tem,P){
  n=nrow(data)
  p=ncol(data)
  s=stats::var(data)
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
