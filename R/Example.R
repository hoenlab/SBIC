#
# library(glmnet)
# library(glasso)
# library(MASS)
# source(file = "dataSimulate.R")
# source(file = "candidateset.R")
# source(file = "mle.R")
# source(file = "gbic.R")
# source(file = "sGGM.R")
# source(file="comparison.R")
# prob1=0.02
# prob2=0.02
# p=100
# a=simulate(p=p, n=500, prob1 = prob1, prob2 = prob2, ka=50)
# data=a$data
# real=a$realnetwork
# priori=a$priornetwork
# aa=ifelse(real!=0,1,0)
# sum(priori[lower.tri(priori)])
# sum(priori[lower.tri(priori)])/(p*(p-1)/2)
# sum(aa[lower.tri(aa)])
# sum(aa[lower.tri(aa)])/(p*(p-1)/2)
# lambda=exp(seq(-10,10, length=30))
# result=sGGM(data = data, lambda = lambda, M=priori, prob = prob2)
# result1=comparison(real = real, estimate = estimate)
# result2=comparison(real=real, estimate = priori)
