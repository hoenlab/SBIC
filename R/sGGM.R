#' @title Model selection of Gaussian graphical model based on SBIC
#' @aliases sggm
#' @author Jie Zhou
#' @description Select the model based on the SBIC criterion and the two-step algorithm  
#' 
#' 
#' 
#' @param data An n by p dataframe representing the observations
#' @param lambda A vector of tuning parameters used to build the model pool
#' @param M The prior adjacency matrix
#' @param prob The mean error rate
#' 
#' 
#' @return 
#'   A list of objects containing: 
#'   \item{networkhat}{The final selected adjacency matrix}
#'   \item{candidates}{The model pool}
#' 
#' @examples 
#'    set.seed(1)
#'    m1 = 100
#'    m2 = 30
#'    p = 100
#'    n = 100
#'    d=simulate(n=n,p=p, m1 = m1, m2 = m2) # simulate fake data 
#'    lambda=exp(seq(-5,5,length=100)) # tuning parameter
#'    data=d$data # data from the simulation
#'    M=d$priornetwork # prior network from simulation
#'    # calculating the error rate 
#'    r1=m2/m1
#'    r2=m2/(p*(p-1)/2-m1)
#'    r=(r1+r2)/2
#'    # apply sggm 
#'    result=sggm(data=data, lambda=lambda, M=M, prob=r)
#'    # compare the final network and the true network 
#'    result$networkhat
#'    d$realnetwork
#' @export

sggm=function(data,lambda,M,prob){
  p=dim(data)[2]
  n=dim(data)[1]
  m=length(lambda)
  t=log(p)/((log(1/prob-1)))
  web=modelset(data = data, lambda = lambda, P=M)
  web=web[(sapply(web, sum)-p)/2<n]
  sbic=seq(0,length=m)
  for (i in 1:m) {
    ggbic=gbic(data=data, theta= web[[i]], prob = prob, P=M)
    sbic[i]=ggbic
  }
  mm=which.min(sbic)
  wweb=web[[mm]]
  
  return(list(networkhat=wweb, candidates=web))
}


