## The following are a pair of functions to cache the inverse 
## of a matrix rather than compute the matrix repeatedly during computation

## This first function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The second function computes the inverse of the previous matrix created 
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i
  }
## Return a matrix that is the inverse of 'x'
