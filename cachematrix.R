## makeCacheMatrix creates a matrix object from a matrix, x, that includes 
## the ability to store it's inverse

## makeCacheMatrix creates an object that stores a matrix() object and it's inverse.
## The set function sets the matrix, and clears any saved matrixinverses.
## The get function returns the matrix.
## The setinverse function and getinverse, set and get the matrixinverse.

makeCacheMatrix <- function(x = matrix()) {
        matrixinverse <- NULL
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrixinverse <<- solve
        getinverse <- function() matrixinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a CacheMatrix as an argument, returns the inverse of
## the matrix contained in the CacheMatrix, returning a cached version of the
## inverse if the inverse has already been calculated, or calculating and 
## returning the inverse, if it has not.

cacheSolve <- function(x, ...) {
        matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
        data <- x$get()
        matrixinverse <- solve(data)
        x$setinverse(matrixinverse)
        matrixinverse
}
