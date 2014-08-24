## Together, these functions produce a matrix data structure whose inverse
## is cached for speed.

## Produces an object that contains a matrix data structure and its inverse
## once calculated by cacheSolve. Returns 4 lambdas -
##  get - gets the value of the matrix
##  set - changes the value of the matrix
##  getInverse - gets the cached inverse of the matrix once set
##  setInverse - sets the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Immediately returns the cached inverse value of a CacheMatrix if available.
## Otherwise calculates, caches, and returns the inverse of the CacheMatrix.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}