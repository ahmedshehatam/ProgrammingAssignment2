## create the makeCacheMatrix List of Functions
## The function simply save the original matrix and its inverse
## It does not calculate the Inverse, just store it
makeCacheMatrix <- function(x = matrix()) {
        Ix <- NULL
        set <- function(y) {
                x <<- y
                Ix <<- NULL
        }
        get <- function() x
        setInv <- function(Invx) Ix <<- Invx
        getInv <- function() Ix
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## create the CacheSolve function
## This function call the makeCacheMatrix to return the cached Inverse Matrix.
## if inverse is not cached, then caculate the inverse.
  
cacheSolve <- function(x, ...) {
        Invx <- x$getInv()
        if(!is.null(Invx)) {
                message("Returning cached Inverse Matrix")
                return(Invx)
        }
        message("calculating The Inverse Matrix")
        data <- x$get()
        Invx <- solve(data, ...)
        x$setInv(Invx)
        Invx
}

## Example Run
## define square nonsingular matrix y
## a <- makeCacheMatrix(y)
## cacheSolve (a)
## should return message "calculating The Inverse Matrix" followed by Inverse Matrix
## cacheSolve (a)
## should return message "Returning cached Inverse Matrix" followed by Inverse Matrix
