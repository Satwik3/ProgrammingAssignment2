## We have two functions, 'makeCacheMatrix' and 'cacheSolve'
## that cache the inverse of the matrix

## makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse for the input 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache


 cacheSolve <- function(x, ...) {	cacheSolve <- function(x, ...) {
               ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}	

 
## ---------------Checking the program------------------------
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)
##       [,1]        [,2]      [,3]       [,4]
##[1,]  0.1198832 -0.02755019 0.2356463  2.0538177
##[2,] -0.0305479 -0.20902400 1.3602928 -3.5114857
##[3,]  0.6263563 -0.14964531 0.8514540 -1.2382608
##[4,] -0.3096068  0.39323607 0.2152657 -0.9000226

