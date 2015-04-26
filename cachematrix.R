## This source constructs a matrix that supports caching of the inverse.
## It does via the two methods below.
## The first, makeCacheMatrix, takes a raw matrix and "wraps" it in a wrapper which defines get / set
## methods both on the matrix itself and its inverse.
## The getinv method is the key one used for caching.
## The second method, cacheSolve is the one that manages the caching.

## Usage: 
## [1] call makeCacheMatrix to wrap a raw matrix; the matrix can be unassigned, since it can be set via set()
## [2] call cacheSolve to get the inverse, returning the cached value is already found

## Assumptions:
## in accordance with the assignment rubric, it's assumed all matrices will be numeric, square invertible.

## -----------------------------------------------------------------------------------------------------------
## wrap a raw matrix and furnish it with 4 methods - get/set on data/inverse
## if no matrix provided, it creates a default empty matrix
## note that inv is initially set to NULL to correctly initiate the caching logic in cacheSolve
## similarly, when assigning a new matrix, the inv must be set to null to cacheSolve recognises it needs
## to solve afresh

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(x_new){
        x <<- x_new
        inv <- NULL
    }
    get <- function() x
    setinv <- function(inv_new) inv <<- inv_new
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## -----------------------------------------------------------------------------------------------------------
## pass a CacheMatrix to this method to get the inverse returned.
## if the method has previously been called on the matrix, then the cached value will be returned
## whenever a new inverse is calculated, the setinv method is called on the CacheMatrix to update the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

## -----------------------------------------------------------------------------------------------------------
