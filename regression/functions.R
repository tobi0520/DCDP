
#divided DP
divide.dp=function(y.train,X.train,N,gamma,lambda,p){
  n=length(y.train)
  
  sset= sample(seq(1,n-1),N,replace = F)
 
  
   # sset = unique(c(sset ,300))
  #N=length(sset)
  
   sset1=sort(c(sset,n,0))
  
  d.star=matrix(Inf, N+2,N+2)
  
  for( k in 1:(N+2)){
    for ( l in 1:(N+2)){
      
      if(k<l){
        i=sset1[k]+1
        j=sset1[l]
        if(j-i>(log(p)+1)^2 ){
        d.star[l,k] = objective.function.regression(lambda, y.train,X.train,i,j,p)}else{
          d.star[l,k]=0
        }
        
      }
      
    }
    
  }
  
  #run DP
  par.vec=rep(-1,n)
  B=rep(Inf,n)
  B0=-1*gamma
  
  sset2=sort(c(sset,n,1))
  
  for ( rr in 1:(N+2)){
    for ( ll in 1:rr ){
      r=sset2[rr]
      l=sset2[ll]
      if( l==1){Bl1=B0}else{ Bl1=B[l]}
      b= Bl1+gamma+d.star[rr,ll]
      if( b < B[r]){
        B[r]=b
        par.vec[r]=l
        
      }
      
    }
    
  }
  
  
  
  #compute p
  changepoints=c()
  r=n
  
  while(r>1){
    l=par.vec[r]
    
    changepoints=c(r,changepoints)
    r=l
    
  }
  changepoints
  rlist=list("estimate"=c(1,changepoints), "objective"=B[n] )
  return(rlist)
}






dcdp=function(y.train,X.train,N,gamma=gamma,lambda,p){
  
  obj.fun=Inf
  estimate=c()
  for( rr in 1:1){
    temp.list=divide.dp(y.train,X.train,N,gamma=gamma,lambda,p ) 
    #print(temp.list)
    if(temp.list$objective <obj.fun){
      estimate=temp.list$estimate
    }
    
  }
  return(estimate)
  
}


#test.error
test.error=function(lambda, y.train,X.train,y.test,X.test,estimation,p,gamma){
  res.total=0
  options(warn=-1)
  
  for ( kk in 2:length(estimation)){
    s=estimation[kk-1]
    e=estimation[kk]
    D=matrix(0, nrow=p,ncol=p)
    diag(D)=1
     if(e-s>1){
    out =  glmnet( X.train[(s+1):e,] , y.train[(s+1):e], family=c("gaussian"), 
                  alpha = 1,lambda=lambda/sqrt(e-s-1))
    res.temp= sum(( y.test[(s+1):e]- predict(out, newx= X.test[(s+1):e,] , lambda=lambda/sqrt(e-s+1)) )^2)
    }else{res.temp=gamma}
    
    res.total=res.total+res.temp
  }
  
  return(res.total)
  
}

 


#cv function 
cv.dcdp=function(lambda, y.train,X.train,y.test,X.test,seq.gamma,seq.lambda,N,p){
  cv.length=length(seq.gamma)*length(seq.lambda)
  tuning.matrix=expand.grid(seq.lambda,seq.gamma)
  
  
  test.rec=Inf
  cv.estimate=c()
  for ( ll in 1:cv.length){ 
    print(ll)
    gamma=tuning.matrix[ll,2]
    lambda=tuning.matrix[ll,1]
    estimate.temp= dcdp(y.train,X.train,N,gamma,lambda,p ) 
     print(estimate.temp)
     goodness.temp=test.error(lambda, y.train,X.train,y.test,X.test,estimate.temp,p,gamma)
    print(goodness.temp)
    if(  goodness.temp< test.rec ){
      test.rec=goodness.temp
      cv.estimate =estimate.temp}
      
  }
  return(cv.estimate)
}
