
rm(list = ls())
#set.seed(10)

setwd("/Users/darenw/Dropbox/work/DCDP/codes/regression/")
source("functions.R")
source("regression-functions.R")
library(glmnet) 


#######################parameters
p=100 
true.change.points=c(0,200,300,500)

#lambda is for lasso
seq.lambda =c(0.5,1.5,3)

#gamma is for selecting number of change points
seq.gamma=seq(50,500,100)

#number of randomly generated subintervals
N=50
#generate data and parameters

beta.list=list()
beta.list[[1]]= c(1,1,1,1,1,rep(0,p-5))
beta.list[[2]]= 10* c(-1,-1,-1,-1,-1,rep(0,p-5))
 beta.list[[3]]=c(2,2,2,2,2,rep(0,p-5))
#beta.list[[4]]=10*c(1,1,1,1,1,rep(0,p-5))

#######################end of parameters



########## training and testing data sets
data.train= generate.regression(beta.list,true.change.points,1)
y.train=data.train$y
X.train=data.train$X
#out = glmnet(data.train$X[1:100,], data.train$y[1:100], family=c("gaussian"), alpha = 1, lambda=0.1)

#out$beta
n=length(data.train$y)

#testing set
data.test= generate.regression(beta.list,true.change.points,1)
y.test=data.test$y
X.test=data.test$X
############# end of training and testing data sets


 


cv.estimate=cv.dcdp(lambda, y.train,X.train,y.test,X.test,seq.gamma,seq.lambda,N,p)
 
 

#print("sample size"); print(n)

#print( "estimate="  ) ;print(estimate)
#print( "true changes=");print( c(1,Delta,2*Delta,3*Delta,n) ) 

  