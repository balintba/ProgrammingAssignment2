## Functions to manipulate matrixes

## A function that enables the creation of Cache-capable matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ##Setting the matrix inverse to null
  MatrixInv <- NULL
  ## Defining the Set function
  set <- function(y) {
    x<<- y
    ## As the matrix previously stored is changed, 
    ## setting the Inverse once again to null
    MatrixInv <<- NULL
  }
  ##Getfunction
  get <- function() x
  ##Set/Get for MatrixInv
  setinv <- function(inver) MatrixInv <<- inver
  getinv <- function() MatrixInv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Creates a function to calculate the inverse or to load it from the Cache 

cacheSolve <- function(x, ...) {
  ## Checking if the Matrix already has an inverse
  MatrixInv <- x$getinv()
  if(!is.null(MatrixInv)) {
    ## If yes, inform the user
    message("getting cached data")
    ##return the inverse, finish the function
    return(MatrixInv)
  }
  #otherwise calculate the inverse
  data <- x$get()
  MatrixInv <- solve(data, ...)
  ##Set the inverse
  x$setinv(MatrixInv)
  ##Return the inverse
  MatrixInv
}
