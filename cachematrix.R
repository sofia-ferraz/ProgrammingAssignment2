## These functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ## initialize the inverse
  i <- NULL
  ## set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  ## get the matrix
  get <- function() {
    ## return the matrix
    x
  }
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## get the inverse of the matrix
  getInverse <- function() {
    ## return the inverse
    i
  }
  ## return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x <- x$getInverse()
  ## only returns the inverse if it has already been calculated
  if( !is.null(x) ) {
    message("getting cached data")
    return(x)
  }
  ## get the matrix 
  data <- x$get()
  ## calculate the inverse 
  x <- solve(data) %*% data
  ## set the inverse 
  x$setInverse(x)
  ## return the matrix
  x
  
  }
