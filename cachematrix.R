## Programming Assignment 2
## Caching the inverse of a matrix
## Nguyen's copy

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invSol) inv <<- invSol
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                meassage("I already have solution. Here it is")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
