## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix() returns a list of 4 functions. Also, there are 2 internal variables to makeCacheMatrix
## x and inv
## if x is an invertible matrix, x %*% inv = I (the identity matrix)
## set() can be used to change the x value of an object already ceated by makeCacheMatrix(), 
##     set() also clears inv
## setinverse() solves for the inverse of the matrix and getinverse() returns the cached version.
##     if setinverse() has not yet been executed, getinverse() returns NULL


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve checks if the inverse exists.
##   if it does, it returns the cahced inverse, and prinits a "getting cached data" message
##   if not, then the inverse of x$get() is calculated (using solve()) and setinverse() is called to cache the inverse.
## The input to cacheSolve is an object of type returned by makeCacheMatrix(), that is a list of 4 functions.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x$get()
  
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
