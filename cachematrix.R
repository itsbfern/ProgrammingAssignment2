## makeCacheMatrix is a function that generates a template variable made up of a 
## a matrix, its inverse and a list containing a set of functions to keep and 
## retrieve them.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve is a function which computes the inverse of the "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated and the matrix
## has not changed, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        if (!is.null(inverse)) {
                message("Getting cached data...")
                return(inverse)
        }
        m <- x$get()
        inverse <- solve(m, ...)
        x$setInv(inverse)
        inverse
}
