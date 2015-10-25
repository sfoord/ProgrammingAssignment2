## These two functions creates and caches the inverse of matrix in the global environment. This is  ##particularly useful when these matrices are used in loop functions, resulting in a considerable ##reduction in computational time

## this function caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) m <<- inverse
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## This function computes the inverse of the matrix from "makeCacheMatrix", or retrieve it from the cache ##if it's already been calculated

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setsolve(m)
        m
}


