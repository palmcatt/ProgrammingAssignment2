###############################################################################
##  1. makeCacheMatrix: This function creates a special "matrix" object 
##  that can cache its inverse.
###############################################################################
##  2. cacheSolve: This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been 
##  calculated (and the matrix has not changed), then the cachesolve should 
##  retrieve the inverse from the cache.
###############################################################################
##  Computing the inverse of a square matrix can be done with the solve 
##  function in R. For example, if X is a square invertible matrix, 
##  then solve(X) returns its inverse.
###############################################################################

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## get-check if cached data exits, if not run rest of the code
        m <- x$getInverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## solve(a,b, ...), If missing, b is taken to be an identity matrix 
        ## and solve will return the inverse of a. 
        ## hence, get the matrix and get inverse of a, then cached up
        mat <- x$get()        
        m <- solve(mat, ...)
        x$setInverse(m)
        m
}