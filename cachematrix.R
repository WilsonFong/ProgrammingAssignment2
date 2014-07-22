## These functions will create a cached inverse of a matrix and retrieve the matrix from the cache.

## This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get =get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by the above matrix. 
## It will retrieve the inverse from the cache is it's already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
