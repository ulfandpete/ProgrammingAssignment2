## Proposed solution to: Assignment: Programming Assignment 2: Lexical Scoping
## Function that caches the Inverse of a Matrix

## This function creates the matrix for which the inverse should be cached

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) m <<- mean
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function cache-solves the inverse of a matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    if (nrow(data) == ncol(data)) {
        m <- solve(data, ...)
    } else {
        message("not a square matrix. cannot inverse")        
    }
    x$setinverse(m)
    m
}