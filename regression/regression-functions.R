generate.regression=function(beta.list,change.points,sigma.eps){
   y=c()
   X=matrix(0, nrow= change.points[length(change.points)] , ncol=p)
  for ( i in 1: ( length(change.points)-1 )) {
    s=change.points[i]+1
    e=change.points[i+1]
              X[s:e,]=
                matrix(rnorm(p*(e-s+1 ),mean=0,sd=1 ),ncol=p)
      y=c(y, 
          X[s:e,]%*%beta.list[[i]]+rnorm(e-s+1 , mean=0,sd=sigma.eps))
     
   }
  return.list=list("y"=y,"X"=X)
  return(return.list)
}



#####objective function on interval (s,e)
objective.function.regression=function(lambda, y.train,X.train,s,e,p){
   options(warn=-1)
  
  
  
    D=matrix(0, nrow=p,ncol=p)
    diag(D)=1
    out.temp =  glmnet(X.train[(s+1):e,], y.train[(s+1):e], family=c("gaussian"), 
                  alpha = 1,lambda=lambda/sqrt(e-s-1))
    res.temp= sum(( y.train[(s+1):e]- predict(out.temp, newx=X.train[(s+1):e,], lambda=lambda/sqrt(e-s+1)) )^2)
    #
   
 
  
  return(res.temp)
  
}