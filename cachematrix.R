## We do not want to inverse a matrix over and over again...
## Hence we wrote these functions one checks if the values are already cached and 
## another facalitates us with setting and getting func for cache

## The job of 'makeCacheMatrix' is to store or return inverse of matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  a <- null
  set <- function(y){
    x <<- y
    a <<- null
  }
  get <- function() x
  setsolve <- function(solve) a <<- solve
  getsolve <- function() a
  list( srt = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## The job of 'cachesolve' is to check if the matrix already has a cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a <- x$getsolve()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setsolve(a)
  a
}
