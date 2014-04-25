## This code consists in a pair of functions that cache the inverse of a matrix. 
## It either computes the inverse of the matrix or (if the matrix has not changed) returns the inverse from the cache.
## 

## The following function creates a special "matrix" object that can cache its inverse. It produces a list of 4 functions. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache via the getinverse function.
## Otherwise, it computes it and sets the result in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
