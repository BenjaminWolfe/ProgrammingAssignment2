################################################################################
## Matrix inversion is often a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
##
## Below, makeCacheMatrix() creates a special "matrix" object that can cache
## its inverse. cacheSolve() then computes the inverse of that object. If the
## inverse is already cached, cacheSolve() will use the cached version. If not,
## it will compute the inverse, return it, and also cache it for later use.
################################################################################


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## create the inv object to represent the matrix's inverse.
        inv <- NULL

        ## define getter and setter functions for the matrix and its inverse.
        ## when only one line is required for a function, skip the curly braces.
        set <- function(y) {
                  x <<- y
                inv <<- NULL
        }
        get <- function() x
        set.inverse <- function(inverse) inv <<- inverse
        get.inverse <- function() inv
        
        ## return a list of getter and setter functions
        list(
                set = set,
                get = get,
                set.inverse = set.inverse,
                get.inverse = get.inverse
        )
}


## efficiently computes the inverse of a "matrix" returned by makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of "x"

        ## Try get.inverse(). If it works, use it.
        inv <- x$get.inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        ## If not, get the matrix itself, and compute using solve().
        data <- x$get()
         inv <- solve(data, ...)

        ## Use set.inverse() to cache the inverse; then return it.
        x$set.inverse(inv)
        inv
}