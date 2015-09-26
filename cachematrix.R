## pair of functions to create a 'CacheMatrix' object that allows
## user to cache a matrix's inverse after the first time it is calculated

## makeCacheMatrix defines components of the 'CacheMatrix' object we are creating,
## including 4 functions to set, get, setinverse, and getinverse of the object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve operates on a 'cachematrix' object to see if the inverse is already cached.
## if the inverse isn't cached, then the inverse is solved for

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

