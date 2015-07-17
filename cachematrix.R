## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function generates a list, that contains functions of setting/getting the matrix data
## as well as setting/getting the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  setInv <- function(i) inv <<- i
  getInv <- function() { inv }
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}

## Write a short comment describing this function
## This function first checks the required inversed matrix is already calculated and cached.
## If the cache exists, this function just return it. 
## Otherwise it solves the matrix and saves the result in the memory, then returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  curInv <- x$getInv()
  if(!is.null(curInv)) {
    message("getting cached data")
    return(curInv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}

## e.g.)
##
##
## > m <- matrix(c(1,2,3,4), nrow=2, ncol=2)
## > m
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheableMatrix <- makeCacheMatrix(m)
## > cacheSolve(cacheableMatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cacheableMatrix)
## getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


