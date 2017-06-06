## cacheMatrix.R:  This file contains functions to invert matricies
##                 and cache the result to be used on repeated calls.
##
##   Ex:
##
## > source('cachematrix.R')
## > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## > m1
##       [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75
## > myMatrix_object <- makeCacheMatrix(m1)
## > cacheSolve(myMatrix_object)
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## > cacheSolve(myMatrix_object)
## getting cached data
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4

## makeCacheMatrix() takes a matrix x as an argument and returns a list with
##  function attributes: x$set(), x$get(), x$setinverse(), and x$getinverse().
##  x$set() and x$get() set and get the matrix itself.
##  x$setinverse() and x$getinverse() set and get the computed inverse.
##  If the inverse has not been cached, x$getinverse() will return NULL.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() takes the "matrix"-like object x returned by makeCacheMatrix()
##  as an argument and returns a matrix that is the inverse of x.
##  If the inverse has been previously computed, the cached copy is returned.
##  If not, the inverse is computed on-demand, cached, and returned.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
