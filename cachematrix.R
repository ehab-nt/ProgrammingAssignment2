##Two functions to cache the inverse of a matrix

## Function to crate special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse property
  i<-NULL
  
  ## function to set the matrix
  set <-function( matrix ){
    m <<- matrix
    i <<- NULL
  }
  
  ## function to get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Function to set the inverse of matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Function to get the inverse of the matrix
  getInverse <- function() {
    ##Return the inverse property
    i
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Calculate inverse of the matrix returned by function 1. 
## If inverse is already calculated, then function should retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Check the cache whether inverse is already calculated. If yes, return it.
  if( !is.null(m)) {
    print("getting cached inverse")
    return(m)
  }
  
  ## Get the original matrix for inverse calculation
  dat <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inverse<-solve(dat)
  
  ## Set the calculated inverse 
  x$setInverse(inverse)
  
  ## Return the inverse matrix
  inverse
  
}
