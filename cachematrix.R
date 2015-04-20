## This pair of functions is adapted from sample material
## provided in the introduction to Programming Assignment 2
## for Programming in R.

## This first function establishes a vector loaded with functions
## that can be applied in the cacheSolve() function to 
## set and display cached values.  The <<- operators in the 
## set() and setinv() functions set values in the scope of the calling
## function cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
        mx <- NULL
                set <- function(y) {
                        x <<- y
                        mx <<- NULL
                }
                get <- function() x
                setinv <- function(inv) mx <<- inv
                getinv <- function() mx
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}


## This second function is the outer of the pair and, once 
## the sub-functions are built with makeCacheMatrix, calls them
## to recall and then store cached versions of the inverted matrix.
## Lines 35-37 check to see whether this inversion has already
## been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mx <- x$getinv()
        if(!is.null(mx)) {
                message("getting cached data")
                return(mx)
        }
        data <- x$get()
        mx <- solve(data, ...)
        x$setinv(mx)
        mx
}
