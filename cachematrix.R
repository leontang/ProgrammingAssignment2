## General description of functions in this file
## function makeCacheMatrix() - creates a special matrix for 
##                              caching it's inverse
## function cacheSolve() - cache the inverse of a special matrix


## This function creates a special matrix which is 
## actually a list containing functions to set/get 
## the matrix and set/get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    
    set <- function(y) {
        x <<- y
        inverse_x <- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse_matrix) {
        inverse_x <<- inverse_matrix
    }
    
    getinverse <- function() {
        inverse_x
    }
    
    list(set = set, get = get
         , setinverse = setinverse
         , getinverse = getinverse)
}


## This function cache the inverse of the special 
## matrix created via makeCacheMatrix() and returns
## the inverse provided if the matrix is a square 
## invertible matrix

cacheSolve <- function(x, ...) {
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("getting cached data")
        return (inverse_x)
    }
    data <- x$get()
    inverse_x <- solve(data, ...)
    x$setinverse(inverse_x)
    inverse_x
}
