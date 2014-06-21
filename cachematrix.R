## These functions create a special object that can cache the value of the
## inverse of a matrix if necessary

## Create an object that can hold a matrix value and optionally set a
## computed inverse.  If the matrix value changes, invalidate the computed
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    derivedval <- NULL
    set <- function(y) {
        x <<- y
        derivedval <<- NULL
    }
    get <- function() x
    setInv <- function(invVal) derivedval <<- invVal
    getInv <- function() derivedval
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Return a pre-calculated inverse if present, otherwise compute the value
## of the inverse of the given matrix.

cacheSolve <- function(x, ...) {
    derivedVal <- x$getInv()
    if (!is.null(derivedVal)) {
        message("getting cached data")
        return(derivedVal)
    }

    data <- x$get()
    derivedVal <- solve(data, ...)
    x$setInv(derivedVal)
    derivedVal
}
