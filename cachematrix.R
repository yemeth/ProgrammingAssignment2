## The file defines two functions that try to avoid having to repeatedly compute the inverse of a matrix. 
## Given a matrix 'x', the function 'makeCacheMatrix' returns a list of four functions to access the matrix 'x'.
## The function 'cacheSolve' uses this list to access the matrix 'x', calculate
## its inverse only once, and then return the cached inverse matrix if needed again.

## Encapsulates the matrix in an object with four operations to simplify the four desired operations.
## set: Establishes the matrix to solve. Also cleans the cache.
## get: Returns the matrix to solve.
## setInverse: Should *only* be called in cacheSolve. Puts the inverse matrix in the cache.
## getInverse: Returns the state of the cache. May return NULL.
## The function does not test if the matrix 'x' is inversible.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() { x }
    setInverse <- function(solve) { inverse <<- solve }
    getInverse <- function() { inverse }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Tests if the custom object 'z' already computed the inverse matrix and returns it.
## Otherwise, it computes the inverse, caches it, and returns it.
## 'z' is a list returned by the function 'makeCacheMatrix'
## The function does not test if the matrix 'x' within the objetc 'z' is inversible.
cacheSolve <- function(z, ...) {
    ## Return a matrix that is the inverse of 'x' 
    inverse <- z$getInverse()
    if(!is.null(inverse)) {
        message("Return cached data")
        return(inverse)
    }
    message("Compute inverse matrix and cache solution")
    data <- z$get()
    inverse <- solve(data, ...)
    z$setInverse(inverse)
    inverse
}
