## Functions to manipulate matrixes
## 

## A function that enables the creation of Cache-capable matrix

makeCacheMatrix <- function(x = matrix()) {
  MatrixInv <- NULL
  set <- function(y) {
    x<<- y
    MatrixInv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) MatrixInv <<- inver
  getinv <- function() MatrixInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Creates a function to calculate the inverse or to load it from the Cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  MatrixInv <- x$getinv()
  if(!is.null(MatrixInv)) {
    message("getting cached data")
    return(MatrixInv)
  }
  data <- x$get()
  MatrixInv <- solve(data, ...)
  x$setinv(MatrixInv)
  MatrixInv
}
