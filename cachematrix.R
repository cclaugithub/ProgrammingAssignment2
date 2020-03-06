## This source contain :
## - an makeCacheMatrix object storing matrix and it's inverse
## - a cacheSolve function able to search if a makeCacheMatrix passed 
##   in parameters have it's inverse cashed and if not cache it's inverse.
##   Returns the inverse of 
## Assumption: The matrix is invertible (n by n matrix with determinant <>0)
## Example:
##> aMatrix<-makeCacheMatrix(matrix(1:4, nrow=2,ncol=2))
##> cacheSolve(aMatrix)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(aMatrix)
##getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5 
##

## makeCacheMatrix object is a matrix able to cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve search for the inverse of the provided inversible matrix 
## and return it if cashed or copute the inverse of a inversible matrix.
## Returns the inversed matrix (found in cash or computed)
## x: a matrix created with makeCasheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
