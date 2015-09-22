## makeCacheMatrix creates a special "matrix", which is
## really a list containing functions to:
##  .  set the value of the matrix
##  .  get the value of the matrix
##  .  (manually) set the value of the inverse
##  .  get the stored inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function (invset) inv <<- invset
  getinv <- function () inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## cachesolve calculates the inverse of the special
## "matrix" created with the above function. However, it
## first checks to see if the inverse has already been
## calculated.If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates
## the inverse of the matrix and caches the value of the
## inverse in the cache via the setinv function.

## PRECONDITION: x is an invertible matrix
## POSTCONDITION: the inverse of x is returned

cacheSolve <- function(x, ...) {
  inv <- x$getinv ()
  if(!is.null (inv)) {
    message ("getting cached data")
    return (inv)
  }
  data <- x$get ()
  inv <- solve (data, ...)
  x$setinv (inv)
  inv
} 
