## makeCacheMatrix and cacheSolve are two functions that can calculate the inverse of an invertible matrix
## and then cache the result. If the inverse has been cached, the result will be retrieved from the cache rather than
## recalculating

## makeCacheMatrix creates a matrix object that caches its inverse

makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y){
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setinv <- function(solve) M <<- solve
  getinv <- function() M
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve will compute the inverse of the matrix created by makeCacheMatrix. If the inverse has already been
## calculated, then it will retrieve the result from the cache.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
  M <- X$getinv()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- X$get()
  M <- solve(data, ...)
  X$setinv(M)
  M
}
