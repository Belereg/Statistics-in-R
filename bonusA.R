frepcomgen = function(n,m){
  mat <- matrix(0,n+2,m+2)
  #if(m>=n){
  mat[n+2,m+2] <- 1 #punem in colt dreapta jos 1
  for(c in 2: (ncol(mat)-1) ) #generam pe prima linie valori pentru yi
  {
    
    mat[1,c] <- mat[1,c-1]+1 
  }
  mat[2,1] <- mat[1,m+1] +1 
  for(r in 3: (nrow(mat)-1) ) #generam pe prima col valori pentru xi
  {
    mat[r,1] <- mat[r-1,1] +1 
  }
  
  mat[2,m+2] <- round(runif(1,0.0,1.0),3) #generam primul q
  
  
  
  mat[n+2,2] <- round(runif(1,0.0,0.05),3) #generez primul p
  s <- mat[n+2,2]
  for(c in 3: (ncol(mat)-2) )#generez p(m-1) valori si le pun pe ultima linie
  {
    mat[n+2,c] <- round(runif(1,0.0,1.0-s),3)
    s <- s+mat[n+2,c]
    if(s>1)
    { 
      s <- s-mat[n+2,c]
      c <- c - 1
    }
    
  }
  
  #generez prima linie de pi-uri. Vor fi m-1 pi-uri generate
  s <- 0
  for(c in 2:(ncol(mat)-2) )
  {
    
    mat[2,c] <- round(runif(1,0.0,min(mat[n+2,c],(mat[2,m+2] -s) )),3)
    s <- s+mat[2,c]
    if(s>mat[2,m+2])
    {
      s <- s - mat[2,m+2]
      c <- c-1
    }
  }
  
  for (r in 3: (nrow(mat)-2) )
  { 
    for(c in 2: (ncol(mat)-2) )
    { s <- 0
    for( i in 2:(r-1) )
    {
      s <- s + mat[i,c]
    }
    mat[r,c] <- round(runif(1,0.0, 0.0 + mat[n+2,c] - s ),3)
    s <- s+mat[r,c]
    if(s>mat[n+2,c])
    {
      s <- s - mat[r,c]
      c <- c-1
    }
    
    }
  }
  #adaugam in continuare pi-uri pentru a ajunge la nr necesar(n*m-1)
  #de pe ultima coloana incepand cu linia a doua, pana la linia n-1
  sp <- 0
  for(c in 2:(ncol(mat)-2))
    sp <- sp+ mat[n+2,c]
  
  for(r in 3:(nrow(mat)-2))
  {
    s <- 0
    for(c in 2:(ncol(mat)-2))
      s <- s + mat[r,c]
    mat[r,m+1] <- round(runif(1,0.0, min( (1.0-s-mat[2,m+2])/(n), (1.0-s-sp))  ),3)
    s <- s+mat[r,m+1]
    while((s+mat[2,m+2])>=1)
    {
      s<-s-mat[r,m+1]
      mat[r,m+1] <- round(runif(1,0.0, min( (1.0-s-mat[2,m+2])/(n) , (1.0-s-sp)) ),3)
      s <- s+mat[r,m+1]
    }
    
  }
  
  return(mat)
  
  #}
  
  
}

#stochez repartitia comuna generata
incMat <-  frepcomgen(3,3)
print(incMat)