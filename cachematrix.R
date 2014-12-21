##################################################################
# R script
# Description: calculates the inverse matrix 
# Owner: nowerzt@gmail.com
# Date: Dec 21, 2014
# History:
# v1.0 init
##################################################################

##################################################################
# @Usage
# return a list of functions
##################################################################
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() i
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


##################################################################
# @Usage
# return the inverse matrix of matrix x;
# Note that this function will cache the result, 
# and return the cache value if the cache exists
##################################################################
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message('get cached inverse')
    return (i)
  }
  data <- x$get()
  # calculate the inverse matrix
  i <- solve(data)
  x$setinverse(i)
  i
}

##################################################################
# @Usage
# build test to test the functions makeCacheMatrix and cacheSolve
##################################################################
runtest <- function() {
  # generate a matrix
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  h8 <- hilbert(8)
  print(h8)
  # constructs the makeCacheMatrix var
  x <- makeCacheMatrix(h8)
  # get the inverse
  i1 <- cacheSolve(x)
  print(i1)
  print(round(h8 %*% i1, 3))
  # get the cached inverse
  i2 <- cacheSolve(x)
  print(i2)
  print(round(h8 %*% i2, 3))
}
