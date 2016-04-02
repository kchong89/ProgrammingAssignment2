## This R script aims to perform matrix inversion and cache it.

## function that creates a "matrix" and cache it

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) z <<- inverse
        getinverse <- function() z
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Computes the inverse of the matrix or retriving it from the cache

cacheSolve <- function(x, ...) {
        z <- x$getinverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        compiled <- x$get()
        z <- solve(compiled, ...)
        x$setinverse(z)
        z
        ## Return a matrix that is the inverse of 'x'
}
