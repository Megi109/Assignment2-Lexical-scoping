library(MASS)
makeCachematrix<- function(x = matrix()){
  inv<-NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function()(x)
  setInverse<- function(Inverse){Inv<<- Inverse}
  getInverse<- function(){Inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cachesolve<- function(x,...){
  Inv<- x$getInverse()
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  mat<- x$get()
  Inv<- solve(mat,...)
  x$setInverse(Inv)
  Inv
}
data<- x$get()
m<- mean(data,...)
x$setmean(m)
m
}

  
