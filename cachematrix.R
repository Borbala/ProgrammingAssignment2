
## Matrix inversion is usually a costly computation. But this pair of functions 
## instead of computing the inverse of a matrix repeatedly,  can cache the 
## inverse of a matrix (if the contents of a matrix has not changed). 
## In this way it is a potentially time-comsuming computation.


## The first function  makeCacheMatrix creates a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function () inv
  list (set=set,
        get=get,
        setInverse=setInverse,
        getInverse=getInverse)
}


## The second function returns the inverse of the matrix. At first it checks if
## the inverse has already been computed, in this case the function gets the 
## result from the cache and skips the computation. Otherwise, it computes the 
## inverse of the matrix, and sets it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null (inv)) {
    message ("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
