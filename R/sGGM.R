#' @name sGGM
#' @aliases sGGM
#' @title Model selection of Gaussian graphical model based on SBIC
#' @description Select the model  based on the SBIC and the model pool construted by two-step algorithm
#' @usage sGGM(data, lambda, M, prob)
#' @param data A \code{n} by \code{p} data frame representing the observations
#' @param lambda The tuning parameter vector
#' @param M The prior adjacency matrix
#' @param prob The mean error rate
#' @return \item{estimate}{the final selected adjacency matrix}
#'   \item{web}{the model pool}
#' @examples
#' set.seed(1)
#' # d=simulate(p=10,n=100, prob1 = 0.1, prob2 = 0.2,ka=4)
#' # lambda=exp(seq(-5,5,length=50))
#' # data=d$data
#' # M=d$priornetwork
#' # prob=0.2
#' # final=sGGM(data=data, lambda=lambda, M=M,prob=prob)
#' # final$estimate
#' @export
sGGM=function(data,lambda,M,prob){
p=dim(data)[2]
n=dim(data)[1]
m=length(lambda)
t=log(p)/((log(1/prob-1)))
web=modelSet(data = data, lambda = lambda, P=M)
web=web[(sapply(web, sum)-p)/2<n]
sbic=seq(0,length=m)
for (i in 1:m) {
  ggbic=gbic(data=data, theta= web[[i]], tem =t, P=M)
  sbic[i]=ggbic
}
mm=which.min(sbic)
wweb=web[[mm]]

return(list(estimate=wweb, candidate=web))
}


