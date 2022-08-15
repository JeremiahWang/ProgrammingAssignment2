## Put comments here that give an overall description of what your
## functions do
### I wrote the code that creates a pair of functions which cache the inverse of
### of a matrix. The first function makeCacheMatrix() creates a special "matrix"
### object that can cache its inverse. The second function cacheSolve() computes
### the inverse of the special "matrix" returned by the first function.

## Write a short comment describing this function
### This function creates a special "matrix" object that can cache its inverse.
### Below you can find code that helps to illustrate how this function can be used
### to set/get the value of a matrix as well as its inverse.
### function_1 <- makeCacheMatrix()
### function_1$set(matrix(1:4, 2, 2))
### function_1$get()
### function_1$setInverse()
### function_1$getInverse()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- solve(x)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
### This function computes the inverse of the special "matrix" returned by 
### makeCacheMatrix above. If the inverse has already been calculated (and the 
### matrix has not changed), then the cacheSolve() should retrieve the inverse from 
### the cache. Below you can find code that helps to illustrate how this function
### can be used to compute/retrieve the inverse of a matrix. Note that the variable
### function_1 has been created in the code illustrating the use of makeCacheMatrix().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
