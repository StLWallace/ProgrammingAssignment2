cacheSolve <- function(X, ...) {
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