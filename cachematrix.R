# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * setInv     set the inverse of the matrix
# * getInv     get the cahced value (inverse of the matrix)
#  it uses <<- assignment operator so that
# these internal variables are not exposed to the
# outside environment. 

makeCacheMatrix <- function(x = matrix()){
  inverseMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  getMatrix <- function() x
  setInv <- function(inv) inverseMatrix <<- inv
  getInv <- function() inverseMatrix
  list(set = setMatrix, get = getMatrix,
       setInv = setInv,
       getInv = getInv)
}

## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setInv function.

cacheSolve <- function(x, ...) {

  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
}
