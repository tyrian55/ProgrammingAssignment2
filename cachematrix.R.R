## This function creates a matrix which is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv) 
}

## This function computes the inverse of the matrix created by the above
## makeCachemMatrix function. If the result has been calculated before, then
## the function will retrieve the result from the cache instead of 
## re-calculating the function.

cacheSolve <- function (x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}