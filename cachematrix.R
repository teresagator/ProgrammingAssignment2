## This program creates a special matrix type that 
## caches the inverse of the matrix upon first use. 
## Subsequent calls will use the cached value rather 
## than recomputing the inverse.


## makeCacheMatrix(x = matrix()) creates a matrix that
## contains a cached value.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse matrix to NULL on first call
  xi <- NULL

  ## Function to get the value of the original matrix 
  get <- function() x
  
  ## Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  
  ## function to set the cached value of the inverse matrix
  setinverse <- function(inv) xi <<- inv

  ## function to get the cached value of the inverse matrix.
  ## returns NULL if cached value has not been set
  getinverse <- function() xi
  
  ## list of functions available in the matrix
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function using the special cache matrix construct to retrieve or compute
## the cached value for the inverse of that matrix.  Requires that the 
## argument x be created using makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the cached matrix inverse
  xi <- x$getinverse()
  
  ## Check to see if matrix inverse has already been created and cached.
  ## if it has, return it.
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  
  ## Matrix inverse has not been cached - compute the inverse, cache it,
  ## and return the inverse
  data <- x$get()
  xi <- solve(data, ...)
  x$setinverse(xi)
  xi
}
