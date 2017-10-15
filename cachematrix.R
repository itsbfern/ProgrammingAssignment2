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

cacheSolve <- function(x, ...) {
        inverse <- x$getInv
        if (!is.null(inv)) {
                message("Getting cached data...")
                return(inv)
        }
        m <- x$get()
        inverse <- solve(m, ...)
        x$setInv(inverse)
        inv
}