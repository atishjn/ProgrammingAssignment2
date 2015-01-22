## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)
# this function works like a class, it creates a list
# that contains 4 member functions: set, get, setInv
# and getInv. it uses <<- assignment operator so that
# these internal variables are not exposed to the
# outside environment. 



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverseMatrix <<- inv
  getinv <- function() inverseMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  Inv <- x$getinv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinv(Inv)
  Inv
}
