## Matrix inversion is usually a costly computation and there may be some benfit to cache
## the inverse of a matrix rather than compute it repeatedly.
## The below 2 functions are used to cache the inverse of a matrix. 

## the makeCacheMatrix creates a list to set value of the matrix, then get its value. 
## It will then set the value of the inverse, then get its value.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## the cacheSolve functions will return the inverse if it's already calculated, and skip
##the calculations. If not, it computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data.")
return(inv)
}
data <- x$get()
inv <- solve(data)
x$setinverse(inv)
inv
}

