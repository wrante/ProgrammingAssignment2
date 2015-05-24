## The following functions create a special matrix which can
## contain a cache of the original matrix and a cache of its
## inverse matrix.

## Creates a special matrix which contains the original matrix and
## its inverse. It contains four functions which can set the value
## of the matrix, get the value of the matrix, set the value of the
## inverse and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the given
## matrix and caches it for the original matrix. If there is
## already an inverse in the cache, it returns the cached
## inverse matrix.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
