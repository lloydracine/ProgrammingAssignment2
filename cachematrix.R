## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
##
## x: a square invertible matrix
## return: a list containing functions to
##         - set the matrix
##         - get the matrix
##         - set the inverse
##         - get the inverse
##         this list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    # use '<<-' to assign a value to an object in an environment
    # different from the current environment.
    x <<- y
    inv <<- NULL
  }
  
  get <- function()
    x
  
  setinv <- function(inverse)
    inv <<- inverse
  
  getinv <- function()
    inv

  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.
##
## x: output of makeCacheMatrix()
## return: inverse of the matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  # get inverse from the cache
  inv <- x$getinv()
  
  # if the inverse is not null then it has already been calculated
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # calculate the inverse
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
