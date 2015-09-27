## Put comments here that give an overall description of what your
## functions do


##function creates a special "matrix" object that can cache its inverse
##  set(m) - set the value of the matrix
##  get() - get the value of the matrix
##  setinverse(inv) - set the value of the inverse of the "matrix" 
##  getinverse() - get the value of the inverse of the "matrix" 

makeCacheMatrix <- function(x = matrix()) {
 
  inverse <- NULL
  set <- function(y) {
    x       <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## function computes und return the inverse of the matrix 
## x - makeCacheMatrix 

cacheSolve <- function(x, ...) {
  
  inv = x$getinverse()
  
  if(!is.null(inv)) { 
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv = solve(data)
  x$setinverse(inv)
  inv
  
}
