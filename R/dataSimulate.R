#' @name simulate
#' @aliases simulate
#' @author Jie Zhou
#' @title Randomly generate adjacent matrices for data simulation purposes
#' @description For a given edge density, first generate an adjacency matrix \code{P} of a graph.
#'   Based on \code{P}, generate multinomial data with mean 0 and a given precision matrix.
#' @usage simulate(p, n, prob1, prob2, ka)
#' @param p Number of vertices in the graph
#' @param n Sample size
#' @param prob1 The densitiy of the edge in the prior graph
#' @param prob2 The discrepancy rate between true and prior edge density
#' @param ka The diagonal entry in the true precision matrix
#' @details The precision matrix has all off-diagonal entries to be 1 and the diagonal entries to be \code{ka}
#' @return  A list including the simulated data, real adjacency matrix and a prior adjacency matrix.
#'   \item{n}{sample size}
#'   \item{prob1}{the density of edge in prior graph}
#'   \item{prob2}{the discrepancy rate between true and prior edge density}
#'   \item{ka}{the diagonal entry in real precision matrix}
#' @examples
#' # set.seed(1)
#' # d=simulate(p=10,n=100,prob1=0.2, prob2=0.1, ka=4)
#' # d$data
#' # d$realnetwork
#' # d$priornetwork
#' @export
###generate the network matrix
simulate=function(p, n, prob1, prob2, ka){
prior_stru=matrix(rbinom(n=p^2, size = 1, prob = prob1), nrow = p, ncol = p)
prior_stru[lower.tri(prior_stru)]=t(prior_stru)[lower.tri(prior_stru)]
diag(prior_stru)=1
###generate the prior network
disturbance=matrix(rbinom(n=p^2, size = 1, prob = prob2),nrow = p, ncol = p)
disturbance[lower.tri(disturbance)]=t(disturbance)[lower.tri(disturbance)]
diag(disturbance)=0
real_stru=(prior_stru+disturbance)%%2
##real network and regression matrix
realnetwork=real_stru
diag(realnetwork)=ka
cov=solve(realnetwork)
mu=rep(0,p)
data=MASS::mvrnorm(n=n, mu=mu, Sigma=cov)
 return(list(data=data, realnetwork=real_stru, priornetwork=prior_stru))
}
