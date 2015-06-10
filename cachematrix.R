## Create a special matrix object that can cache inverse
## If cache contains a cached inverse and matrix object has not changed
## function returns cached inverse

## makeCacheMatrix creates a matrix object with four functions
## set - sets the matrix supplied
## get - retrieves stored matrix
## setinverse - sets the inverse of the set matrix
## getinverse - retrieves inverse of matrix if stored
## returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve sets m as current cached inverse or null if empty
## Checks if m is null and previous matrix matches current matrix
## If cache is present, returns cached data
## If cache is absent, calculates inverse and sets inverse into x
## returns inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m) && (x$get=x)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}  

