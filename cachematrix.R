## This is a pair of functions for calculating, caching,
## and retrieving the inverse of a square matrix.

## makeCacheMatrix is a functional interface to the inverse
## matrix cache, providing a means of retrieving and setting
## inverse matrix values.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## cacheSolve takes a matrix and returns the inverse in one of
## two ways: (1) retrieves it from the cache, or (2) computes
## it and stores the result in the cache before returning it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
