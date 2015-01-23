## These functions allow the user to cache the value of the
## inverse of a matrix.  The user may call cacheSolve on 
## objects returned from makeCacheMatrix.  

## This function takes a matrix x and creates a new 
## matrix object that allows for the caching of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) xinv <<- inv
  getInverse <- function() xinv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This funtion takes a matrix object x of the type created
## by makeCacheMatrix and returns its inverse.  This function
## assumes x is invertible.  The first time this function is called
## for x the inverse of x is computed and cached; all subsequent
## function calls retrieve this cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
