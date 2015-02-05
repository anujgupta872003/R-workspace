makeCacheMatrix<-function(x=matrix()){
  i<-NULL;
  set<-function(y){
    x<<-y
    
  }
  get<-function()
    x
  setinverse<-function(inverse){
    i<<-inverse
  
  }
  getinverse<-function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
cacheSolve<-function(x){

  if(is.null(x$getinverse())){
    i<-solve(x$get());
    x$setinverse(i)
    return(i)
  }
  message("getting cached data")
  x$getinverse();
  
}