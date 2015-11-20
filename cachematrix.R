## Below are two functions that are used to create a special object that 
## stores a square invertible matrix and cache its inverse. 
## Assume that the matrix supplied is always invertible.

## Create a special "matrix" object which is a list of functions that can
## set the value of matrix
## get the value of matrix
## set the inverse of matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function (ivs) inv <<- ivs
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x'
## If the inverse is cached, get the cached data. Otherwise,
## calculate the inverse (using solve(...)function) and cache it.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("Getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

