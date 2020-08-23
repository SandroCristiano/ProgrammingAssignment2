## The functions presented here are meant to cache the inverse of a matrix.

## This function crates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) z <<- inverse
  getInverse <- function() z
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function computes the inverse of the makeCacheMatrix function 
## and retrieves it from the cache if it had already been calculated.

cacheSolve <- function(x, ...) {
      z <- x$getInverse()
    if(!is.null(z)){
      message("Getting ached data")
      return(z)
    }
    mat <- x$get()
    z <- solve(mat,...)
    x$setInverse(z)
    z
  }