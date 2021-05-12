#There are two funcitons makeCacheMatrix() and cacheSolve(). These
## functions take input as matrix and checks if the inverse is available in 
## cache memory otherwise calculates the inverse of matrix.


## makeCacheMtrix is a function which takes an input matrix variable.
## The variable x can also be assigned a new value using set() function in
## in the local environment. get() function helps to check the value of input
## variable x.
## setinv() allows the user to store the value of inverse of matrix x in local 
## environment. getinv() allows to check the value of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## the funciton cacheSolve helps in calculating the inverse of matrix if
## if it's not available in the cached memory.
## When the function is called, it first checks whether the value of inverse
## is already available in makeCacheMatrix() function, and if it's not, the 
## funciton cacheSolve create the inverse of matrix.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}