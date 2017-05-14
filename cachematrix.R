# Matrix inversion is usually a costly computation and there may be
# some benefit to caching the inverse of a matrix rather than compute
# it repeatedly.

# The next function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
        x <<- y
        inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of square matrix created by makeCacheMatrix above.
# If the inverse has already been calculated, then it shoul get back the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
 
# In case no cached inversed matrix. So, do the next...
   data <- x$get() # get matrix x
  inv <- solve(data, ...) # calculated the inverse
  x$setinverse(inv)
  inv # return inverse
}
