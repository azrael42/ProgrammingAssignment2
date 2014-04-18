## Two functions to automatically cache matrix inversion 

# Creates a special kind of "matrix" which is really a list 
# containing four functions. 
# set: Sets the matrix which is represented
# get: Gets the matrix which is represented
# setInverse: Sets the inverse matrix of the represented matrix 
# getInverse: Gets the inverse matrix of the represented matrix 

makeCacheMatrix <- function(initMatrix = matrix()) {
  inv <- NULL
  set <- function(newMatrix) {
    initMatrix <<- newMatrix
    inv <<- NULL
  }
  get <- function() initMatrix
  setInverse <- function(newInverse) inv <<- newInverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of a matrix represented by a makeCacheMatrix list
## If the inverse has been computed before, the cached value is returned

cacheSolve <- function(X, ...) {
  ## Return a matrix that is the inverse of 'X'
  inv <- X$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data, ...)
  X$setInverse(inv)
  inv
}

A <- makeCacheMatrix()
A$get()
cacheSolve(A)
cacheSolve(A)

A$set(matrix(rnorm(16),4,4))
cacheSolve(A)
cacheSolve(A) 
cacheSolve(A) %*% A$get()
round(cacheSolve(A) %*% A$get(), 10)
round(A$get() %*% cacheSolve(A), 10)
