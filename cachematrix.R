##Cache the inverse of a matrix using a pair of functions

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ## Returns a list containing functions to: set the matrix, get the matrix, set the inverse,
                                             ## and get the inverse. This list is the input to cacheSolve()
  
  inverse = NULL
  set = function(y) {     
    x <<- y  ## <<- assigns a value to an object in another environment
    inverse <<- NULL
  }
  get = function() x
  setinverse = function(invert) inverse <<- invert
  getinverse = function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the "special" matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix hasn't changed, the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {  ## x is the output of makeCacheMatrix(); returns the inverse of the original matrix input to makeCacheMatrix()
  
  inverse <- x$getinverse()  ## Checks to see if the inverse has already been calculated; If it has, it gets it from the cache and skips the computation.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()  ## If it is not in the cache, it calculates the inverse
  inverse <- solve(data, ...)
  x$setinverse(inverse)  ## setinv sets the value of the inverse in the cache
  
  return(inverse)
}

