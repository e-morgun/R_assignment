## Caching the inverse of a matrix
## Assignment 2, Coursera R programming

## Author: Eva M.

## By using makeCacheMatrix and cacheSolve, user should
## be able to input a matrix, and obtain an object 
## that only calculates the matrix's invese once
## further calls on the matrix inverse through cacheSolve
## will draw the already saved value

## Input matrix is assumed to be invertable


## MakeCacheMatix
## takes object type matrix
## function that creates a matrix with a cacheable inverse

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) invers <<- inverse
  
  get_inverse <- function() invers
  
  list(set=set, get=get, 
       set_inverse=set_inverse, 
       get_inverse=get_inverse)

}


## CacheSolve 
## takes object created by makeCacheMatri and other paramaters of solve()
## returns calculates matrix inverse or returns cached copy

cacheSolve <- function(x, ...) {
  invers <- x$get_inverse()
  if(!is.null(invers)){
    message("getting cached data")
    return(invers)
  }
  mat <- x$get()
  invers <- solve(mat, ...)
  x$set_inverse(invers)
  invers  # returning a matrix that is inverse of x
}
