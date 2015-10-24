## These functions belong to Assignment2_R Programming Course offered by Johns Hopkin University.
## The overal aim is to cache the inverse of a matrix. 

## makeCacheMatrix function creates a special matrix object that can cache its inverse.
## The function stores a list of 4 functions: get, set, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
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

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated, then `cacheSolve` should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("got cached inversed matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
## Return a matrix that is the inverse of 'x'