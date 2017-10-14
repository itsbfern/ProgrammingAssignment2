## This function will create a template variable made up of a function list which 
## keeps and retrieve a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set < - function(y) {
                x <<- Y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function() inv <<- solve(x)               
        getInverse <- function() inv
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## cacheSolve is a function which computes the inverse of the "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated and the matrix
## has not changed, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse
        if (!is.null(inv)) {
                message("Getting cached data...")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
