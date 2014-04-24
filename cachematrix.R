## Implement a "cached inverse matrix" object that can cache its inverse
## 

## This function creates a "cached inverse matrix" object which can cache it's calculated inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## This function computes the inverse of "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
