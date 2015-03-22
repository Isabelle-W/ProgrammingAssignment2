## Functions makeCacheMatrix and cacheSolve allow to compute 
## the inverse of a matrix with caching of the result
## in order to reduce costly repeated computations.
##
## usage:
## First create a sample matrix m
## m <- matrix(c(-1, -2, 1, 1), 2,2)
## x <- makeCacheMatrix(m)
## cacheSolve(x)
## call the cacheSolve function twice to test data caching:
## cacheSolve(x)

## Function makeCacheMatrix takes a matrix as argument,
## defines and returns
## the list of functions to be called by function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function cacheSolve takes as argument the list returned by
## function makeCacheMatrix, calculates and caches the inverted
## matrix if it is not already cached in memory. 
## It returns the resulting inverted matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
