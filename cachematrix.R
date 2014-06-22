## makeCacheMatrix and cacheSolve are functions that are able to cache matrixes
## and their inverses in case are needed during future computations


## makeCacheMatrix is a function that create a special matrix object if the 
## matrix is not in cache.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}

## cacheSolve is a function that calculate the inverse of a matrix if the
## the inverse matrix is not in cache, in other case it will get from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

