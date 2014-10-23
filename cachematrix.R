## These funtions use the scoping rules in R in order to save computing resources
## Creating a matrix with the function makeCacheMatrix, the values are stored in 
## the working environment.

## makeCacheMatrix creates the x matrix and the inverse in the cache. Also provides the 
## methods to access to the data environment where the matrix and its inverse are stored.
## This function assume that the matrix supplied is always invertible.

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


## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("The inverse is already calculated. Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
