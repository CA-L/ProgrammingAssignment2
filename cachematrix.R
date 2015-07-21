## This set of functions cater for calculating the inverse of a matrix, and storing
## this value for future caching. 

## This function sets out the four functions belonging to makeCacheMatrix,
## setting and getting the matrix, and setting or getting its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() { x }
    set_inverse <- function(inverse) { inv <<- inverse }
    get_inverse <- function() { inv }
    
    list( set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## This function tries to get the value of the inverse of a matrix stored 
## in makeCacheMatrix. If this is not null, a message goes to the user to confirm
## the cached data is being returned, then returns this.
## If the value of the inverse is null, the inverse is calculated, stored for the 
## future, and then returned to the user.

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)){
        message("Getting cached inverse...") 
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$set_inverse(inv)
    inv
}
