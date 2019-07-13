### The following pair of functions are used to cache the inverse of a matrix, which can avoid repeated computation.
## It is assumed that the matrix supplied is always invertible.

## The function below is used to create a special object that stores a matrix and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invs <<-inverse
    getInverse <- function() invs
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function computes the inverse of the special "matrix" created with the above function.
## It wil first examine whether the inverse has already been calculated. If so, the inverse will be got from the cache.
## Ohterwise, it will caculate the inverse of the matrix and set the value of inverse in cache.

cacheSolve <- function(x, ...) {
        invs <- x$getInverse()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs) ## Return a matrix that is the inverse of 'x'
        }
        newx <- x$get()
        invs <- solve(newx, ...)
        x$setInverse(invs)
        invs
}
