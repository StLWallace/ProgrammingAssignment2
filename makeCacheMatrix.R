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

