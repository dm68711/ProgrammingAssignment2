##
## makeCacheMatrix(x) - given a matrix x, return a cached matrix instance
## capable of caching the matrix's inverse
##
## cacheSolve(x) - given a cached matrix x, return its matrix's
## inverse -- using a cached value if it is available.
##

##
## makeCacheMatrix(x) - given a matrix x, return a cached matrix instance
## capable of caching the matrix's inverse
##
## INPUT: matrix x, assumed to be square and invertible
##
## OUTPUT: a cached matrix represented as a list containing functions
## to get the matrix, set the matrix, set the inverse, and get the
## inverse
##
## NOTES: a warning is printed and NULL is returned if the given matrix is
## not square.
## 

makeCacheMatrix <- function(x = matrix()) {

        if( nrow(x) != ncol(x) ) {
           warning("warning: makeCacheMatrix requires a square invertible matrix as input")
           return
        }

        cachedInverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                cachedInverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) cachedInverseMatrix <<- inverseMatrix
        getInverse <- function() cachedInverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


##
## cacheSolve(x) - given a cached matrix x, return its matrix's
## inverse -- using a cached value if it is available.
##
## INPUT: list representation of a cached matrix
## 
## OUTPUT: the matrix's inverse. Return the cached value if it is
## available, other compute and cache the inverse before returning.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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

