
rm(list = ls())
#set.seed(10)

setwd("/Users/darenw/Dropbox/work/DCDP/codes")
source("functions.R")


#generate data and parameters
Delta=100 
mu1=-2
mu2=1
y=c(rnorm(Delta/2,0,1),rnorm(2*Delta,mu1,1),rnorm(Delta,10,1),rnorm(Delta/2,mu2,1))
n=length(y)

 y.test=c(rnorm(Delta/2,0,1),rnorm(2*Delta,  mu1,1),rnorm(Delta,10,1),rnorm(Delta/2,  mu2,1))

#number of randomly generated subintervals
N=50


seq.gamma=20*seq(0.0001,0.01,0.0005)*n
cv.estimate=cv.dcdp(y,y.test,seq.gamma,N)

print(cv.estimate)



 


#print("sample size"); print(n)

#print( "estimate="  ) ;print(estimate)
#print( "true changes=");print( c(1,Delta,2*Delta,3*Delta,n) ) 

  