
#divided DP
divide.dp=function(y,N,gamma){
  n=length(y)
  
  sset= sample(seq(1,n-1),N,replace = F)
  
  #sset=c(sset,Delta,2*Delta)
  
  sset1=sort(c(sset,n,0))
  
  d.star=matrix(Inf, N+2,N+2)
  
  for( k in 1:(N+2)){
    for ( l in 1:(N+2)){
      
      if(k<l){
        i=sset1[k]+1
        j=sset1[l]
        mean.temp=mean(y[i:j])
        d.star[l,k] =sum ( (y[i:j] -mean.temp  )^2)
        
      }
      
    }
    
  }
  
  #run DP
  p=rep(-1,n)
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
        p[r]=l
        
      }
      
    }
    
  }
  
  
  
  #compute p
  changepoints=c()
  r=n
  
  while(r>1){
    l=p[r]
    
    changepoints=c(r,changepoints)
    r=l
    
  }
  
  rlist=list("estimate"=c(1,changepoints), "objective"=B[n]-gamma*(length(changepoints)-1))
  return(rlist)
}




#Goodness of fit
goodness.of.fit=function(y,y.test,estimate){
  K.hat=length(estimate)-1
  objective.test=0
  for( kk in 1:K.hat){
    mu.temp=mean(y[estimate[kk]:estimate[kk+1]])
    #print(mu.temp)
    objective.test=sum((mu.temp-y.test[estimate[kk]:estimate[kk+1]])^2)+objective.test
    
    
  }
  return(objective.test)
  
}




dcdp=function(y,gamma,N){
  
  obj.fun=Inf
  estimate=c()
  for( rr in 1:5){
    temp.list=divide.dp(y,N,gamma=gamma)  
    #print(temp.list)
    if(temp.list$objective <obj.fun){
      estimate=temp.list$estimate
    }
    
  }
  return(estimate)
  
}

#cv function 
cv.dcdp=function(y,y.test,seq.gamma,N){
  cv.length=length(seq.gamma)
  test.error=Inf
  cv.estimate=c()
  for ( ll in 1:cv.length){ 
    gamma=seq.gamma[ll]
    estimate.temp= dcdp(y,gamma,N)
     print(estimate.temp)
     goodness.temp=goodness.of.fit(y,y.test,estimate.temp)
    print(goodness.temp)
    if(  goodness.temp< test.error ){
      test.error=goodness.temp
      cv.estimate =estimate.temp}
      
  }
  return(cv.estimate)
}
