## This pair of functions enables to create a cache object which
## stores the inverse of a matrix once it has been solved.

## This function creates a special "matrix" object, which contains
## the given matrix and functions to 1. set the value of the matrix,
## 2. get the value of the matrix, 3. set the value of the inverse,
## 4. get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function solves the inverse of the "matrix" object, caches it
## in the "matrix" object and returns it. If the inverse has already
## been calculated, it returns the cached value.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
