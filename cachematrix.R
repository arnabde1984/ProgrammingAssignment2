
## This function takes a matrix and returns a list of functions that act as input to cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) x_inv <<- inverse
  getInverse <- function() x_inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function retrieves inverse from cache if the calculated inverse is same as the matrix, else it calculates the inverse. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  x_inv <- x$getInverse()
  if (!is.null(x_inv)) {
    message("Getting cached data!")
    return(x_inv)
  }
  mat <- x$get()
  x_inv <- solve(mat, ...)
  x$setInverse(x_inv)
  x_inv
  
}