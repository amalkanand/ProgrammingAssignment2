## Creates the program to cache the matrix 
## so that inverison takes less time


## Creates the function to cache the matrix

makeCacheMatrix <- function(x = matrix()) {
  Matinv <- NULL
  set <- function(y){
    x <<- y
    Matinv <<- NULL
  }
  get <- function() x
  setMatInverse <- function(solveMatrixInv) Matinv <<- solveMatrixInv
  getMatInverse <- function() Matinv
  list(set = set, get = get, setMatInverse = setMatInverse, getMatInverse = getMatInverse)
}


## Funtion to check on cache and calculate inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Matinv <- x$getMatInverse()
  if(!is.null(Matinv)){
    message("getting cached data")
    return(Matinv)
  }
  data <- x$get()
  Matinv <- solve(data)
  x$setMatInverse(Matinv)
  Matinv
}
