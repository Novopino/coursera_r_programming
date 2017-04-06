## The following two functions will calculate the
## inverse matrix or retreive it from the cache

## Function below creates a special matrix object that
## can cache its inverse
## Four functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function returns the inverse of
## the matrix. It first checks if the inverse has
## already been computed. If so, it gets the result
## and skips the computation. If not, it computes
## the inverse, sets the value in the cache via
## setinverse function.

## This function assumes that the matrix is always
## invertible.

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
}