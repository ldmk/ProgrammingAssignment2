## These two functions use caching to store the inverse of a matrix in a different environment, so it does not have to be 
## computed more than once (unless the matrix has changed)

## This function creates a vector containing a list of functions to set the value of the matrix, 
## get the value, set the inverse, and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <-function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks whether inverse of the matrix has been already calculated, in which case it returns the inverse from the cache,
## otherwise it calculates and returns the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
