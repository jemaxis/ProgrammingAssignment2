# There are two functions in this script - makeCacheMatrix and cacheSolve.
# The main purpose is to efficiently calculate the inverse of a square matrix 
# by caching its inverse.

# The makeCacheMatrix function creates a special "matrix" object that can cache 
# its inverse. It returns a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
# The assumption made is that the matrix provided is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(invArg) inverse <<- invArg
  getInverse <- function() inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# The cacheSolve function  computes the inverse of the special "matrix" 
# returned by makeCacheMatrix. If the inverse has already been calculated 
# and the matrix has not changed, then the cacheSolve function will retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached inverse data")
    return(inverse)
  }else{
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
  }
}