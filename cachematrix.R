## makeCacheMatrix This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Matinv <- NULL
        set <- function(y) {
                x <<- y
                Matinv <<- NULL
        }
        get <- function() x
        setMatinv <- function(inverse) Matinv <<- inverse
        getinv <- function() Matinv
        list(set = set, get = get, setMatinv = setMatinv, getinv = getinv)
}


## cacheSolve : This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'm'
        Matinv <- x$getinv()
        ## Check if m is cached
        if(!is.null(Matinv)) {
                message("Getting the result in the cache")
                return(Matinv)
        }
        data <- x$get()
        Matinv <- solve(data, ...)
        x$setMatinv(Matinv)
        Matinv
}
