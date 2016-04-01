## makeCacheMatrix is a function that creates a special object that
## stores a matrix and allows you to take action on the matrix.
## cacheSolve is a function that will allow you to calculate the inverse
## of a matrix from makeCacheMatrix,
## pulling the inverse from cache if it was already calculated previously.

## makeCacheMatrix creates a special "matrix" that consists of functions to
## set the value of the matrix, get the value of the matrix,
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list (set = set, get = get,
    setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special matrix created in makeCacheMatrix
## however, it also first checks to see if the inverse was already calculated and if it
## has been, grabs the value from cache instead of computing it again

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}