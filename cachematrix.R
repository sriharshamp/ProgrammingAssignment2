## As described in the assignment problem statement, matrix inversion
## is a costly computation, whose cost could be reduced by caching the
## the inverse of a matrix instead of computing it repeatedly. This objective
## is achieved by the functions makeCacheMatrix() and cacheSolve(), which cache
## the inverse of a matrix

## The first function, makeCacheMatrix creates a list containing a function to 
## (a) set the value of the matrix
## (b) get the value of the matrix
## (c) set the value of the matrix inverse
## (d) get the value of the matrix inverse
## It creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(y) {
    x <<- y
    mat_inverse <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat_inverse <<- inverse
  getinverse <- function() mat_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## returned by makeCacheMatrix. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
  mat_inverse <- x$getinverse()
  if(!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get()
  mat_inverse <- solve(data)
  x$setinverse(mat_inverse)
  mat_inverse
}
