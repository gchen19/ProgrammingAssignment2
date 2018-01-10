## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list that stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL ## xInv stores the inverse of the matrix x
  ## set is used to set the matrix in the cacheMatrix
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  ## get is used to get the matrix in the cacheMatrix
  get <- function() x
  ##setInv is used to set the inverse matrix
  setInv <- function(inv) xInv <<-inv
  ##getInv is used to get the inverse matrix
  getInv <- function() xInv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve takes in special cachematrix x and solves for its inverse
## IF an inverse was not previously calculated, otherwise returns the
## cached inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("retrieving cached inverse matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setInv(inv)
  inv
}
