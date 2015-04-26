## Put comments here that give an overall description of what your
## functions do

## caches and returns the inverted matrix 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Determines if matrix invertion has been cached, if cached will use cached version, if not will recompute

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
