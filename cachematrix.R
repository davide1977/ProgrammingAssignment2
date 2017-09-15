## This functions allows to stock a matrix, calculate the invers of this matrix,
##  and stock both (matrix and the inverted matrix)

## the function makecacheMatrix create a matrix obect that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inverse.x <- NULL

set <- function(y) {
        x <<- y
        inverse.x <<- NULL
}
get <- function() x
setinv <- function(invx) inverse.x <<- invx
getinv <- function() inverse.x
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculate the inverse of the matrix x and return it only if its not already calculated 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse.x <- x$getinv()
        if(!is.null(inverse.x)) {
                message("getting cached data")
                return(inverse.x)
        }
        data <- x$get()
        inverse.x <- solve(data, ...)
        x$setinv(inverse.x)
        inverse.x
}
