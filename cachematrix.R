## Kebra Thompson
## 11-14-14
## R Programming Assignment 2

## These functions will cache the inverse of a matrix so that it
## does not have to be calculated multiple times.

## This function describes the methods that will be used for 
## each new matrix object.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             ## inv is the matrix inverse; 
                                            ## reset to NULL with each new object
    set <- function(y) {                    ## sets new matrix as x
        x <<- y
        inv <<- NULL
    }
    get <- function() {                     ## get returns value of original matrix
        x
    }
    setinv <- function(solve) {             ## sets the value of the inverse
        inv <<- solve
    }
    getinv <- function() {                  ## gets the value of the inverse
        inv
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function retrieves or calculates the inverse of a matrix object.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()                       ## accesses object x and gets value of inverse
    
    if (!is.null(inv)) {                    ## if inverse was already cached (not null)
        message("getting cached data")      ## display this message
        return(inv)                         ## return the inverse
    }
    invmat <- x$get()                       ## we only get here if x$getinv returned null
    inv <- solve(invmat, ...)               ## if inv was null, we have to calculate the inverse
    x$setinv(inv)                           ## store calculated inv for x
    inv                                     ## return the inverse
}
