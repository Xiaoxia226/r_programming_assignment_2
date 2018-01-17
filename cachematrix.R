makeCacheMatrix<-function(x=matrix()){
  n<-NULL
  set<-function(y){
    x<<-y
    n<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) n<<-inverse
  getinverse<-function()n
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}

B <- matrix(c(1,2,3,4),2,2)
print(B)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
#print(B1)
cacheSolve(B1)
