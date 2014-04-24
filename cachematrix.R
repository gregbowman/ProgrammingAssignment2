## Implement a "cached inverse matrix" object that can cache its inverse
## 

## This function creates a "cached inverse matrix" object which can cache it's calculated inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y                                 ## set the original matrix value
                i <<- NULL                              ## set the cached inverse to unknown
        }
        get <- function() x                             ## return the original matrix
        setinv <- function(inverse) i <<- inverse       ## store the argument as the cached inverse
        getinv <- function() i                          ## return the cached inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## This function computes the inverse of "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()                         ## get the saved inverse
        if(!is.null(i)) {
                message("getting cached data")
                return(i)                       ## use saved inverse if it has been solved
        }
        data <- x$get()                         ## get the value of the original matrix
        i <- solve(data, ...)                   ## use solve to invert it
        x$setinv(i)                             ## save the inverted matrix
        i                                       ## return the inverted matrix
}
