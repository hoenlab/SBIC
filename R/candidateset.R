#' @title Enrichment step for constructing the model pool 
#' @description This is the esnrichment step in the two-step algorithm to construct the model pool (internal use only)  
#' @author Jie Zhou 
#' @param data An \code{n} by \code{p} matrix of observations 
#' @param lambda Vector of tuning parameter 
#' @param P Prior adjacency matrix  
#' @return 
#'   A list of model objects   
#' @import glmnet 
addition=function(data,lambda, P){
  p=dim(data)[2]
  nlambda=length(lambda)
  invP=matrix(1,nrow = p,ncol = p)-P
  n=dim(data)[1]
  arr=array(0,dim = c(p,nlambda,p))
  
    for (i in 1:p) {
      beta=matrix(0,nrow = (p-1), ncol = nlambda)
      x=as.matrix(data[,-i])
      y=data[,i]
      penalty=invP[i,][-i]
      result=glmnet::glmnet(x=x,y=y, family="gaussian",lambda = lambda, penalty.factor = penalty)
      bb=as.matrix(result$beta)
      if (ncol(bb)>0){beta[,1:ncol(bb)]=bb}
      if (i==1) {beta=rbind(1,beta)}
      if (i==p) {beta=rbind(beta,1)}
      if ((2<=i) & (i<=p-1)) {beta=rbind(beta[1:(i-1),],1,beta[i:(p-1),])}
      arr[, , i]=beta
    }
  web=vector("list", length = nlambda)
   for (j in 1:nlambda) {
    web[[j]]=arr[,j,]+t(arr[,j,])
    web[[j]]=ifelse(abs(web[[j]])<=10^(-4),0,1)
   }
  web=unique(web)
  return(web)
}

#' @title Pruning step for constructing the model pool 
#' @description This is the pruning step in the two-step algorithm to construct the model pool (internal use only)  
#' @author Jie Zhou 
#' @param data An \code{n} by \code{p} matrix of observations 
#' @param lambda Vector of tuning parameter 
#' @param P Prior adjacency matrix  
#' @return 
#'   A list of model objects   
#' @import glmnet 
deletion=function(data,lambda,P){
  a=1*(P!=0)
  #b=(a+1)%%2
  p=dim(data)[2]
  n=dim(data)[1]
  nlambda=length(lambda)
  arr=array(0,dim = c(p,nlambda,p))
    for (i in 1:p) {
      beta=matrix(0,nrow = p-1, ncol = nlambda)
      y=data[,i]
      index=which(a[i,-i]==1)
      if (length(index)>1){
      index2=index-(index>i)
      x=data[,index]
      result=glmnet::glmnet(x=x,y=y, family="gaussian",lambda = lambda)
      bb=as.matrix(result$beta)
      if (ncol(bb)>0){beta[index2,1:ncol(bb)]=bb}
      }
if (i==1) {beta=rbind(1,beta)}
if (i==p) {beta=rbind(beta,1)}
if ((2<=i) & (i<=p-1)) {beta=rbind(beta[1:(i-1),],1,beta[i:(p-1),])}
arr[, , i]=beta
    }
  web=vector("list", length = nlambda)
  for (j in 1:nlambda) {
    web[[j]]=arr[,j,]+t(arr[,j,])
    web[[j]]=ifelse(abs(web[[j]])<=10^(-1),0,1)
    if (any(diag(web[[j]])==0)) 
      stop("got zero")
  }
  web=unique(web)
  return(web)
}


#' @name modelset
#' @aliases modelset
#' @title Construct model pool using the two-step algorithm 
#' @description For a given prior graph, the two-step algorithm, including edge enrichment and pruning, 
#'   is used to construct the model pool
#' @author Jie Zhou  
#' 
#' @param data A \code{n} by \code{p} data frame of observations 
#' @param lambda Tuning parameter vector 
#' @param P Prior adjacency matrix
#'
#' @return
#'   A list including all the candidate models in the model pool. 
#'   Each model is represented by a \code{p} by \code{p} adjacency matrix

#' 
#' @examples
#' \donttest{
#'   set.seed(1)
#'   d=simulate(n=100, p=100, m1 = 100, m2 = 30)
#'   data=d$data
#'   P=d$priornetwork
#'   lambda=exp(seq(-5,5,length=100))
#'   candidates=modelset(data=data,lambda=lambda, P=P)
#' }
#' @import glmnet 
#' @export
#' 
modelset=function(data,lambda,P){
  ##P is the standized  prior information matrix
  P=ifelse(abs(P)==0,0,1)
  p=ncol(data)
  n=nrow(data)
  nlambda=length(lambda)
  inv=matrix(1,nrow = p,ncol = p)-P
  web1=addition(data=data,lambda = lambda,P = P)
  web=vector("list",length = length(web1))
  for (i in 1:length(web1)){ 
    web[[i]]=deletion(data = data,lambda = lambda,P=web1[[i]])
  }
  num=sum(lengths(web))+length(web1)+1
  webb=vector("list",length=num)
  s=cumsum(lengths(web))
  for (m in 1:length(web[[1]])){
    webb[[m]]=web[[1]][[m]]
  }
  if (length(s)>1){
    for (k in 2:length(s)) {
      for (m in 1:length(web[[k]])){
        webb[[s[k-1]+m]]=web[[k]][[m]]
      }
    }
  }
  webb[(sum(lengths(web))+1):(sum(lengths(web))+length(web1))]=web1
  webb[num]=list(P)
  
  candidates=unique(webb)
  return(candidates)
}
