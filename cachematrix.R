## Created functions provide possibility to cache the inverse of an invertible 
## matrix instead of computing it every time

## this function takes an invertible matrix and creates
## a matrix object capable of caching its own inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <-function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) im <<- solve
        getinverse <- function() im
        list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## takes cachable matrix object created by makeCacheMatrix function
## and other arguments to be passed to solve function to find inverse matrix
## if inverse matrix is cached, returns it as well as message that cahce is used
## if not cached yet, inverse matrix is calculated and cached and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
