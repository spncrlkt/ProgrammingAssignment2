## makeCacheMatrix and cacheSolve work together to solve matrix inverses 
## and cache the results for performance reasons. 
## Usage:
## -  Create cacheMatrix object
## cacheMatrix <- makeCacheMatrix(rawMatrix)
## -  Compute inverse the first time
## inv <- cacheSolve(cacheMatrix)
## -  Provided the cacheMatrix isn't set to a new matrix, subsequent calls
## -    retrieve the inverse from cache.
## inv <- cacheSolve(cacheMatrix)



## Creates the cached matrix object, returning a list of getters & setters
## for the raw matrix and the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cachSolve returns an inverse of x. If that inverse has been solved and 
## cached, we'll return it. Otherwise, we'll solve it and cache the solution

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    ## inverse has been cached, so return the cached copy
    return(inv)
  }
  ## inverse hasn't been cached, so compute, cache, and return it
  data <- x$get()
  inv <- solve(data, ...) ## compute matrix inverse
  x$setinv(inv)
  inv
}
